#[cfg(feature = "trace")]
use crate::device::trace::Action;
use crate::{
    api_log,
    command::{
        ClearError, CommandAllocator, CommandBuffer, CopySide, ImageCopyTexture, TransferError,
    },
    conv,
    device::{life::ResourceMaps, DeviceError, WaitIdleError},
    global::Global,
    hal_api::HalApi,
    id::{self, DeviceId, QueueId},
    init_tracker::{has_copy_partial_init_tracker_coverage, TextureInitRange},
    resource::{
        Buffer, BufferAccessError, BufferMapState, DestroyedBuffer, DestroyedTexture, Resource,
        ResourceInfo, ResourceType, StagingBuffer, Texture, TextureInner,
    },
    resource_log, track, FastHashMap, SubmissionIndex,
};

use hal::{CommandEncoder as _, Device as _, Queue as _};
use parking_lot::Mutex;
use smallvec::SmallVec;

use std::{
    iter, mem, ptr,
    sync::{atomic::Ordering, Arc},
};
use thiserror::Error;

use super::Device;

#[allow(dead_code)] // JIMB
pub struct Queue<A: HalApi> {
    pub(crate) device: Option<Arc<Device<A>>>,
    pub(crate) raw: Option<A::Queue>,
    pub(crate) info: ResourceInfo<Queue<A>>,
}

impl<A: HalApi> Resource for Queue<A> {
    const TYPE: ResourceType = "Queue";

    type Marker = crate::id::markers::Queue;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }
}

impl<A: HalApi> Drop for Queue<A> {
    fn drop(&mut self) { todo!() }
}

#[repr(C)]
pub struct SubmittedWorkDoneClosureC {
    pub callback: unsafe extern "C" fn(user_data: *mut u8),
    pub user_data: *mut u8,
}

#[cfg(send_sync)]
unsafe impl Send for SubmittedWorkDoneClosureC {}

pub struct SubmittedWorkDoneClosure {
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct WrappedSubmissionIndex {
    pub queue_id: QueueId,
    pub index: SubmissionIndex,
}

/// A texture or buffer to be freed soon.
///
/// This is just a tagged raw texture or buffer, generally about to be added to
/// some other more specific container like:
///
/// - `PendingWrites::temp_resources`: resources used by queue writes and
///   unmaps, waiting to be folded in with the next queue submission
///
/// - `ActiveSubmission::last_resources`: temporary resources used by a queue
///   submission, to be freed when it completes
///
/// - `LifetimeTracker::free_resources`: resources to be freed in the next
///   `maintain` call, no longer used anywhere
#[derive(Debug)]
#[allow(dead_code)] // JIMB
pub enum TempResource<A: HalApi> {
    Buffer(Arc<Buffer<A>>),
    StagingBuffer(Arc<StagingBuffer<A>>),
    DestroyedBuffer(Arc<DestroyedBuffer<A>>),
    DestroyedTexture(Arc<DestroyedTexture<A>>),
    Texture(Arc<Texture<A>>),
}

/// A series of [`CommandBuffers`] that have been submitted to a
/// queue, and the [`wgpu_hal::CommandEncoder`] that built them.
pub(crate) struct EncoderInFlight<A: HalApi> {
    marker: std::marker::PhantomData<A>,
}

/// A private command encoder for writes made directly on the device
/// or queue.
///
/// Operations like `buffer_unmap`, `queue_write_buffer`, and
/// `queue_write_texture` need to copy data to the GPU. At the hal
/// level, this must be done by encoding and submitting commands, but
/// these operations are not associated with any specific wgpu command
/// buffer.
///
/// Instead, `Device::pending_writes` owns one of these values, which
/// has its own hal command encoder and resource lists. The commands
/// accumulated here are automatically submitted to the queue the next
/// time the user submits a wgpu command buffer, ahead of the user's
/// commands.
///
/// Important:
/// When locking pending_writes be sure that tracker is not locked
/// and try to lock trackers for the minimum timespan possible
///
/// All uses of [`StagingBuffer`]s end up here.
#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct PendingWrites<A: HalApi> {
    pub command_encoder: A::CommandEncoder,

    /// True if `command_encoder` is in the "recording" state, as
    /// described in the docs for the [`wgpu_hal::CommandEncoder`]
    /// trait.
    pub is_recording: bool,

    pub temp_resources: Vec<TempResource<A>>,
    pub dst_buffers: FastHashMap<id::BufferId, Arc<Buffer<A>>>,
    pub dst_textures: FastHashMap<id::TextureId, Arc<Texture<A>>>,

    /// All command buffers allocated from `command_encoder`.
    pub executing_command_buffers: Vec<A::CommandBuffer>,
}

#[derive(Clone, Debug, Error)]
#[error("Queue is invalid")]
pub struct InvalidQueue;

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum QueueWriteError {
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum QueueSubmitError {
}

//TODO: move out common parts of write_xxx.

