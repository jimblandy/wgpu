#[cfg(feature = "trace")]
use crate::device::trace;
use crate::{
    binding_model::{self, BindGroup, BindGroupLayout, BindGroupLayoutEntryError},
    command, conv,
    device::{
        bgl,
        life::{LifetimeTracker, WaitIdleError},
        queue::PendingWrites,
        AttachmentData, DeviceLostInvocation, MissingDownlevelFlags, MissingFeatures,
        RenderPassContext, 
    },
    hal_api::HalApi,
    hub::Hub,
    init_tracker::{
        BufferInitTracker, BufferInitTrackerAction, MemoryInitKind, TextureInitRange,
        TextureInitTracker, TextureInitTrackerAction,
    },
    instance::Adapter,
    pipeline,
    pool::ResourcePool,
    registry::Registry,
    resource::{
        self, Buffer, QuerySet, Resource, ResourceInfo, ResourceType, Sampler, Texture,
        TextureView, TextureViewNotRenderableReason,
    },
    resource_log,
    snatch::{SnatchGuard, SnatchLock, Snatchable},
    storage::Storage,
    track::{
        BindGroupStates, TextureSelector, Tracker, TrackerIndexAllocators, UsageScope,
        UsageScopePool,
    },
    validation::{
        self, check_buffer_usage, check_texture_usage, validate_color_attachment_bytes_per_sample,
    },
    FastHashMap, LabelHelpers as _, SubmissionIndex,
};

use arrayvec::ArrayVec;
use hal::{CommandEncoder as _, Device as _};
use once_cell::sync::OnceCell;
use parking_lot::{Mutex, MutexGuard, RwLock};

use smallvec::SmallVec;
use thiserror::Error;
use wgt::{DeviceLostReason, TextureFormat, TextureSampleType, TextureViewDimension};

use std::{
    borrow::Cow,
    iter,
    num::NonZeroU32,
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering},
        Arc, Weak,
    },
};

use super::{
    life::{self, ResourceMaps},
    queue::{self, Queue},
    DeviceDescriptor, DeviceError, ImplicitPipelineContext, UserClosures,
};

/// Structure describing a logical device. Some members are internally mutable,
/// stored behind mutexes.
///
/// TODO: establish clear order of locking for these:
/// `life_tracker`, `trackers`, `render_passes`, `pending_writes`, `trace`.
///
/// Currently, the rules are:
/// 1. `life_tracker` is locked after `hub.devices`, enforced by the type system
/// 1. `self.trackers` is locked last (unenforced)
/// 1. `self.trace` is locked last (unenforced)
///
/// Right now avoid locking twice same resource or registry in a call execution
/// and minimize the locking to the minimum scope possible
/// Unless otherwise specified, no lock may be acquired while holding another lock.
/// This means that you must inspect function calls made while a lock is held
/// to see what locks the callee may try to acquire.
///
/// As far as this point:
/// device_maintain_ids locks Device::lifetime_tracker, and calls...
/// triage_suspected locks Device::trackers, and calls...
/// Registry::unregister locks Registry::storage
///
/// Important:
/// When locking pending_writes please check that trackers is not locked
/// trackers should be locked only when needed for the shortest time possible
#[allow(dead_code)] // JIMB
pub struct Device<A: HalApi> {
    raw: Option<A::Device>,
    pub(crate) adapter: Arc<Adapter<A>>,
    pub(crate) queue: OnceCell<Weak<Queue<A>>>,
    queue_to_drop: OnceCell<A::Queue>,
    pub(crate) zero_buffer: Option<A::Buffer>,
    pub(crate) info: ResourceInfo<Device<A>>,

    pub(crate) command_allocator: command::CommandAllocator<A>,
    //Note: The submission index here corresponds to the last submission that is done.
    pub(crate) active_submission_index: AtomicU64, //SubmissionIndex,
    // NOTE: if both are needed, the `snatchable_lock` must be consistently acquired before the
    // `fence` lock to avoid deadlocks.
    pub(crate) fence: RwLock<Option<A::Fence>>,
    pub(crate) snatchable_lock: SnatchLock,

    /// Is this device valid? Valid is closely associated with "lose the device",
    /// which can be triggered by various methods, including at the end of device
    /// destroy, and by any GPU errors that cause us to no longer trust the state
    /// of the device. Ideally we would like to fold valid into the storage of
    /// the device itself (for example as an Error enum), but unfortunately we
    /// need to continue to be able to retrieve the device in poll_devices to
    /// determine if it can be dropped. If our internal accesses of devices were
    /// done through ref-counted references and external accesses checked for
    /// Error enums, we wouldn't need this. For now, we need it. All the call
    /// sites where we check it are areas that should be revisited if we start
    /// using ref-counted references for internal access.
    pub(crate) valid: AtomicBool,

    /// All live resources allocated with this [`Device`].
    ///
    /// Has to be locked temporarily only (locked last)
    /// and never before pending_writes
    pub(crate) trackers: Mutex<Tracker<A>>,
    pub(crate) tracker_indices: TrackerIndexAllocators,
    // Life tracker should be locked right after the device and before anything else.
    life_tracker: Mutex<LifetimeTracker<A>>,
    /// Temporary storage for resource management functions. Cleared at the end
    /// of every call (unless an error occurs).
    pub(crate) temp_suspected: Mutex<Option<ResourceMaps<A>>>,
    /// Pool of bind group layouts, allowing deduplication.
    pub(crate) bgl_pool: ResourcePool<bgl::EntryMap, BindGroupLayout<A>>,
    pub(crate) alignments: hal::Alignments,
    pub(crate) limits: wgt::Limits,
    pub(crate) features: wgt::Features,
    pub(crate) downlevel: wgt::DownlevelCapabilities,
    pub(crate) instance_flags: wgt::InstanceFlags,
    pub(crate) pending_writes: Mutex<Option<PendingWrites<A>>>,
    pub(crate) deferred_destroy: Mutex<Vec<DeferredDestroy<A>>>,
    #[cfg(feature = "trace")]
    pub(crate) trace: Mutex<Option<trace::Trace>>,
    pub(crate) usage_scopes: UsageScopePool<A>,
}

#[allow(dead_code)] // JIMB
pub(crate) enum DeferredDestroy<A: HalApi> {
    TextureView(Weak<TextureView<A>>),
    BindGroup(Weak<BindGroup<A>>),
}

impl<A: HalApi> std::fmt::Debug for Device<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { todo!() }
}

impl<A: HalApi> Drop for Device<A> {
    fn drop(&mut self) { todo!() }
}

#[derive(Clone, Debug, Error)]
pub enum CreateDeviceError {
}


impl<A: HalApi> Resource for Device<A> {
    const TYPE: ResourceType = "Device";

    type Marker = crate::id::markers::Device;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }
}
