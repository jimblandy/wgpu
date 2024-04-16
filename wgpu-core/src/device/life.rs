use crate::{
    binding_model::{BindGroup, BindGroupLayout, PipelineLayout},
    command::RenderBundle,
    device::{
        queue::{EncoderInFlight, SubmittedWorkDoneClosure},
        DeviceError, DeviceLostClosure,
    },
    hal_api::HalApi,
    id,
    pipeline::{ComputePipeline, RenderPipeline},
    resource::{
        self, Buffer, DestroyedBuffer, DestroyedTexture, QuerySet, Resource, Sampler,
        StagingBuffer, Texture, TextureView,
    },
    snatch::SnatchGuard,
    track::{ResourceTracker, Tracker, TrackerIndex},
    FastHashMap, SubmissionIndex,
};
use smallvec::SmallVec;

use parking_lot::Mutex;
use std::sync::Arc;
use thiserror::Error;

/// A struct that keeps lists of resources that are no longer needed by the user.
#[allow(dead_code)] // JIMB
pub(crate) struct ResourceMaps<A: HalApi> {
    pub buffers: FastHashMap<TrackerIndex, Arc<Buffer<A>>>,
    pub staging_buffers: FastHashMap<TrackerIndex, Arc<StagingBuffer<A>>>,
    pub textures: FastHashMap<TrackerIndex, Arc<Texture<A>>>,
    pub texture_views: FastHashMap<TrackerIndex, Arc<TextureView<A>>>,
    pub samplers: FastHashMap<TrackerIndex, Arc<Sampler<A>>>,
    pub bind_groups: FastHashMap<TrackerIndex, Arc<BindGroup<A>>>,
    pub bind_group_layouts: FastHashMap<TrackerIndex, Arc<BindGroupLayout<A>>>,
    pub render_pipelines: FastHashMap<TrackerIndex, Arc<RenderPipeline<A>>>,
    pub compute_pipelines: FastHashMap<TrackerIndex, Arc<ComputePipeline<A>>>,
    pub pipeline_layouts: FastHashMap<TrackerIndex, Arc<PipelineLayout<A>>>,
    pub render_bundles: FastHashMap<TrackerIndex, Arc<RenderBundle<A>>>,
    pub query_sets: FastHashMap<TrackerIndex, Arc<QuerySet<A>>>,
    pub destroyed_buffers: FastHashMap<TrackerIndex, Arc<DestroyedBuffer<A>>>,
    pub destroyed_textures: FastHashMap<TrackerIndex, Arc<DestroyedTexture<A>>>,
}

/// Resources used by a queue submission, and work to be done once it completes.
#[allow(dead_code)] // JIMB
struct ActiveSubmission<A: HalApi> {
    /// The index of the submission we track.
    ///
    /// When `Device::fence`'s value is greater than or equal to this, our queue
    /// submission has completed.
    index: SubmissionIndex,

    /// Resources to be freed once this queue submission has completed.
    ///
    /// When the device is polled, for completed submissions,
    /// `triage_submissions` removes resources that don't need to be held alive any longer
    /// from there.
    ///
    /// This includes things like temporary resources and resources that are
    /// used by submitted commands but have been dropped by the user (meaning that
    /// this submission is their last reference.)
    last_resources: ResourceMaps<A>,

    /// Buffers to be mapped once this submission has completed.
    mapped: Vec<Arc<Buffer<A>>>,

    /// Command buffers used by this submission, and the encoder that owns them.
    ///
    /// [`wgpu_hal::Queue::submit`] requires the submitted command buffers to
    /// remain alive until the submission has completed execution. Command
    /// encoders double as allocation pools for command buffers, so holding them
    /// here and cleaning them up in [`LifetimeTracker::triage_submissions`]
    /// satisfies that requirement.
    ///
    /// Once this submission has completed, the command buffers are reset and
    /// the command encoder is recycled.
    encoders: Vec<EncoderInFlight<A>>,

    /// List of queue "on_submitted_work_done" closures to be called once this
    /// submission has completed.
    work_done_closures: SmallVec<[SubmittedWorkDoneClosure; 1]>,
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum WaitIdleError {
}

/// Resource tracking for a device.
///
/// ## Host mapping buffers
///
/// A buffer cannot be mapped until all active queue submissions that use it
/// have completed. To that end:
///
/// -   Each buffer's `ResourceInfo::submission_index` records the index of the
///     most recent queue submission that uses that buffer.
///
/// -   Calling `Global::buffer_map_async` adds the buffer to
///     `self.mapped`, and changes `Buffer::map_state` to prevent it
///     from being used in any new submissions.
///
/// -   When the device is polled, the following `LifetimeTracker` methods decide
///     what should happen next:
///
///     1)  `triage_mapped` drains `self.mapped`, checking the submission index
///         of each buffer against the queue submissions that have finished
///         execution. Buffers used by submissions still in flight go in
///         `self.active[index].mapped`, and the rest go into
///         `self.ready_to_map`.
///
///     2)  `triage_submissions` moves entries in `self.active[i]` for completed
///         submissions to `self.ready_to_map`.  At this point, both
///         `self.active` and `self.ready_to_map` are up to date with the given
///         submission index.
///
///     3)  `handle_mapping` drains `self.ready_to_map` and actually maps the
///         buffers, collecting a list of notification closures to call. But any
///         buffers that were dropped by the user get moved to
///         `self.free_resources`.
///
/// Only calling `Global::buffer_map_async` clones a new `Arc` for the
/// buffer. This new `Arc` is only dropped by `handle_mapping`.
#[allow(dead_code)] // JIMB
pub(crate) struct LifetimeTracker<A: HalApi> {
    /// Resources that the user has requested be mapped, but which are used by
    /// queue submissions still in flight.
    mapped: Vec<Arc<Buffer<A>>>,

    /// Buffers can be used in a submission that is yet to be made, by the
    /// means of `write_buffer()`, so we have a special place for them.
    pub future_suspected_buffers: Vec<Arc<Buffer<A>>>,

    /// Textures can be used in the upcoming submission by `write_texture`.
    pub future_suspected_textures: Vec<Arc<Texture<A>>>,

    /// Resources whose user handle has died (i.e. drop/destroy has been called)
    /// and will likely be ready for destruction soon.
    pub suspected_resources: ResourceMaps<A>,

    /// Resources used by queue submissions still in flight. One entry per
    /// submission, with older submissions appearing before younger.
    ///
    /// Entries are added by `track_submission` and drained by
    /// `LifetimeTracker::triage_submissions`. Lots of methods contribute data
    /// to particular entries.
    active: Vec<ActiveSubmission<A>>,

    /// Buffers the user has asked us to map, and which are not used by any
    /// queue submission still in flight.
    ready_to_map: Vec<Arc<Buffer<A>>>,

    /// Queue "on_submitted_work_done" closures that were initiated for while there is no
    /// currently pending submissions. These cannot be immediately invoked as they
    /// must happen _after_ all mapped buffer callbacks are mapped, so we defer them
    /// here until the next time the device is maintained.
    work_done_closures: SmallVec<[SubmittedWorkDoneClosure; 1]>,

    /// Closure to be called on "lose the device". This is invoked directly by
    /// device.lose or by the UserCallbacks returned from maintain when the device
    /// has been destroyed and its queues are empty.
    pub device_lost_closure: Option<DeviceLostClosure>,
}


