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
        RenderPassContext, CLEANUP_WAIT_MS,
    },
    hal_api::HalApi,
    hal_label,
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
    DeviceDescriptor, DeviceError, ImplicitPipelineContext, UserClosures, ENTRYPOINT_FAILURE_ERROR,
    IMPLICIT_BIND_GROUP_LAYOUT_ERROR_LABEL, ZERO_BUFFER_SIZE,
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
    #[error("Not enough memory left to create device")]
    OutOfMemory,
    #[error("Failed to create internal buffer for initializing textures")]
    FailedToCreateZeroBuffer(#[from] DeviceError),
}

impl<A: HalApi> Device<A> {
    pub(crate) fn raw(&self) -> &A::Device { todo!() }
    pub(crate) fn require_features(&self, feature: wgt::Features) -> Result<(), MissingFeatures> { todo!() }

    pub(crate) fn require_downlevel_flags(
        &self,
        flags: wgt::DownlevelFlags,
    ) -> Result<(), MissingDownlevelFlags> { todo!() }
}

impl<A: HalApi> Device<A> {
    pub(crate) fn new(
        raw_device: A::Device,
        raw_queue: &A::Queue,
        adapter: &Arc<Adapter<A>>,
        desc: &DeviceDescriptor,
        trace_path: Option<&std::path::Path>,
        instance_flags: wgt::InstanceFlags,
    ) -> Result<Self, CreateDeviceError> { todo!() }

    pub fn is_valid(&self) -> bool { todo!() }

    pub(crate) fn release_queue(&self, queue: A::Queue) { todo!() }

    pub(crate) fn lock_life<'a>(&'a self) -> MutexGuard<'a, LifetimeTracker<A>> { todo!() }

    /// Run some destroy operations that were deferred.
    ///
    /// Destroying the resources requires taking a write lock on the device's snatch lock,
    /// so a good reason for deferring resource destruction is when we don't know for sure
    /// how risky it is to take the lock (typically, it shouldn't be taken from the drop
    /// implementation of a reference-counted structure).
    /// The snatch lock must not be held while this function is called.
    pub(crate) fn deferred_resource_destruction(&self) { todo!() }

    pub fn get_queue(&self) -> Option<Arc<Queue<A>>> { todo!() }

    pub fn set_queue(&self, queue: Arc<Queue<A>>) { todo!() }

    /// Check this device for completed commands.
    ///
    /// The `maintain` argument tells how the maintenance function should behave, either
    /// blocking or just polling the current state of the gpu.
    ///
    /// Return a pair `(closures, queue_empty)`, where:
    ///
    /// - `closures` is a list of actions to take: mapping buffers, notifying the user
    ///
    /// - `queue_empty` is a boolean indicating whether there are more queue
    ///   submissions still in flight. (We have to take the locks needed to
    ///   produce this information for other reasons, so we might as well just
    ///   return it to our callers.)
    pub(crate) fn maintain<'this>(
        &'this self,
        fence: &A::Fence,
        maintain: wgt::Maintain<queue::WrappedSubmissionIndex>,
        snatch_guard: SnatchGuard,
    ) -> Result<(UserClosures, bool), WaitIdleError> { todo!() }

    pub(crate) fn untrack(&self, trackers: &Tracker<A>) { todo!() }

    pub(crate) fn create_buffer(
        self: &Arc<Self>,
        desc: &resource::BufferDescriptor,
        transient: bool,
    ) -> Result<Buffer<A>, resource::CreateBufferError> { todo!() }

    pub(crate) fn create_texture_from_hal(
        self: &Arc<Self>,
        hal_texture: A::Texture,
        hal_usage: hal::TextureUses,
        desc: &resource::TextureDescriptor,
        format_features: wgt::TextureFormatFeatures,
        clear_mode: resource::TextureClearMode<A>,
    ) -> Texture<A> { todo!() }

    pub fn create_buffer_from_hal(
        self: &Arc<Self>,
        hal_buffer: A::Buffer,
        desc: &resource::BufferDescriptor,
    ) -> Buffer<A> { todo!() }

    pub(crate) fn create_texture(
        self: &Arc<Self>,
        adapter: &Adapter<A>,
        desc: &resource::TextureDescriptor,
    ) -> Result<Texture<A>, resource::CreateTextureError> { todo!() }

    pub(crate) fn create_texture_view(
        self: &Arc<Self>,
        texture: &Arc<Texture<A>>,
        desc: &resource::TextureViewDescriptor,
    ) -> Result<TextureView<A>, resource::CreateTextureViewError> { todo!() }

    pub(crate) fn create_sampler(
        self: &Arc<Self>,
        desc: &resource::SamplerDescriptor,
    ) -> Result<Sampler<A>, resource::CreateSamplerError> { todo!() }

    pub(crate) fn create_shader_module<'a>(
        self: &Arc<Self>,
        desc: &pipeline::ShaderModuleDescriptor<'a>,
        source: pipeline::ShaderModuleSource<'a>,
    ) -> Result<pipeline::ShaderModule<A>, pipeline::CreateShaderModuleError> { todo!() }

    #[allow(unused_unsafe)]
    pub(crate) unsafe fn create_shader_module_spirv<'a>(
        self: &Arc<Self>,
        desc: &pipeline::ShaderModuleDescriptor<'a>,
        source: &'a [u32],
    ) -> Result<pipeline::ShaderModule<A>, pipeline::CreateShaderModuleError> { todo!() }

    /// Generate information about late-validated buffer bindings for pipelines.
    //TODO: should this be combined with `get_introspection_bind_group_layouts` in some way?
    pub(crate) fn make_late_sized_buffer_groups(
        shader_binding_sizes: &FastHashMap<naga::ResourceBinding, wgt::BufferSize>,
        layout: &binding_model::PipelineLayout<A>,
    ) -> ArrayVec<pipeline::LateSizedBufferGroup, { hal::MAX_BIND_GROUPS }> { todo!() }

    pub(crate) fn create_bind_group_layout(
        self: &Arc<Self>,
        label: &crate::Label,
        entry_map: bgl::EntryMap,
        origin: bgl::Origin,
    ) -> Result<BindGroupLayout<A>, binding_model::CreateBindGroupLayoutError> { todo!() }

    pub(crate) fn create_buffer_binding<'a>(
        bb: &binding_model::BufferBinding,
        binding: u32,
        decl: &wgt::BindGroupLayoutEntry,
        used_buffer_ranges: &mut Vec<BufferInitTrackerAction<A>>,
        dynamic_binding_info: &mut Vec<binding_model::BindGroupDynamicBindingData>,
        late_buffer_binding_sizes: &mut FastHashMap<u32, wgt::BufferSize>,
        used: &mut BindGroupStates<A>,
        storage: &'a Storage<Buffer<A>>,
        limits: &wgt::Limits,
        snatch_guard: &'a SnatchGuard<'a>,
    ) -> Result<hal::BufferBinding<'a, A>, binding_model::CreateBindGroupError> { todo!() }

    pub(crate) fn create_texture_binding(
        view: &TextureView<A>,
        internal_use: hal::TextureUses,
        pub_usage: wgt::TextureUsages,
        used: &mut BindGroupStates<A>,
        used_texture_ranges: &mut Vec<TextureInitTrackerAction<A>>,
    ) -> Result<(), binding_model::CreateBindGroupError> { todo!() }

    // This function expects the provided bind group layout to be resolved
    // (not passing a duplicate) beforehand.
    pub(crate) fn create_bind_group(
        self: &Arc<Self>,
        layout: &Arc<BindGroupLayout<A>>,
        desc: &binding_model::BindGroupDescriptor,
        hub: &Hub<A>,
    ) -> Result<BindGroup<A>, binding_model::CreateBindGroupError> { todo!() }

    pub(crate) fn check_array_binding(
        features: wgt::Features,
        count: Option<NonZeroU32>,
        num_bindings: usize,
    ) -> Result<(), super::binding_model::CreateBindGroupError> { todo!() }

    pub(crate) fn texture_use_parameters(
        self: &Arc<Self>,
        binding: u32,
        decl: &wgt::BindGroupLayoutEntry,
        view: &TextureView<A>,
        expected: &'static str,
    ) -> Result<(wgt::TextureUsages, hal::TextureUses), binding_model::CreateBindGroupError> { todo!() }

    pub(crate) fn create_pipeline_layout(
        self: &Arc<Self>,
        desc: &binding_model::PipelineLayoutDescriptor,
        bgl_registry: &Registry<BindGroupLayout<A>>,
    ) -> Result<binding_model::PipelineLayout<A>, binding_model::CreatePipelineLayoutError> { todo!() }

    //TODO: refactor this. It's the only method of `Device` that registers new objects
    // (the pipeline layout).
    pub(crate) fn derive_pipeline_layout(
        self: &Arc<Self>,
        implicit_context: Option<ImplicitPipelineContext>,
        mut derived_group_layouts: ArrayVec<bgl::EntryMap, { hal::MAX_BIND_GROUPS }>,
        bgl_registry: &Registry<BindGroupLayout<A>>,
        pipeline_layout_registry: &Registry<binding_model::PipelineLayout<A>>,
    ) -> Result<Arc<binding_model::PipelineLayout<A>>, pipeline::ImplicitLayoutError> { todo!() }

    pub(crate) fn create_compute_pipeline(
        self: &Arc<Self>,
        desc: &pipeline::ComputePipelineDescriptor,
        implicit_context: Option<ImplicitPipelineContext>,
        hub: &Hub<A>,
    ) -> Result<pipeline::ComputePipeline<A>, pipeline::CreateComputePipelineError> { todo!() }

    pub(crate) fn create_render_pipeline(
        self: &Arc<Self>,
        adapter: &Adapter<A>,
        desc: &pipeline::RenderPipelineDescriptor,
        implicit_context: Option<ImplicitPipelineContext>,
        hub: &Hub<A>,
    ) -> Result<pipeline::RenderPipeline<A>, pipeline::CreateRenderPipelineError> { todo!() }

    pub(crate) fn get_texture_format_features(
        &self,
        adapter: &Adapter<A>,
        format: TextureFormat,
    ) -> wgt::TextureFormatFeatures { todo!() }

    pub(crate) fn describe_format_features(
        &self,
        adapter: &Adapter<A>,
        format: TextureFormat,
    ) -> Result<wgt::TextureFormatFeatures, MissingFeatures> { todo!() }

    pub(crate) fn wait_for_submit(
        &self,
        submission_index: SubmissionIndex,
    ) -> Result<(), WaitIdleError> { todo!() }

    pub(crate) fn create_query_set(
        self: &Arc<Self>,
        desc: &resource::QuerySetDescriptor,
    ) -> Result<QuerySet<A>, resource::CreateQuerySetError> { todo!() }

    pub(crate) fn lose(&self, message: &str) { todo!() }

    pub(crate) fn release_gpu_resources(&self) { todo!() }

    pub(crate) fn new_usage_scope(&self) -> UsageScope<'_, A> { todo!() }
}

impl<A: HalApi> Device<A> {
    pub(crate) fn destroy_command_buffer(&self, mut cmd_buf: command::CommandBuffer<A>) { todo!() }

    /// Wait for idle and remove resources that we can, before we die.
    pub(crate) fn prepare_to_die(&self) { todo!() }
}

impl<A: HalApi> Resource for Device<A> {
    const TYPE: ResourceType = "Device";

    type Marker = crate::id::markers::Device;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }
}
