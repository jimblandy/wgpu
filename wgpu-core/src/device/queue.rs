#[cfg(feature = "trace")]
use crate::device::trace::Action;
use crate::{
    api_log,
    command::{
        extract_texture_selector, validate_linear_texture_data, validate_texture_copy_range,
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

/// Number of command buffers that we generate from the same pool
/// for the write_xxx commands, before the pool is recycled.
///
/// If we don't stop at some point, the pool will grow forever,
/// without a concrete moment of when it can be cleared.
const WRITE_COMMAND_BUFFERS_PER_POOL: usize = 64;

#[repr(C)]
pub struct SubmittedWorkDoneClosureC {
    pub callback: unsafe extern "C" fn(user_data: *mut u8),
    pub user_data: *mut u8,
}

#[cfg(send_sync)]
unsafe impl Send for SubmittedWorkDoneClosureC {}

pub struct SubmittedWorkDoneClosure {
    // We wrap this so creating the enum in the C variant can be unsafe,
    // allowing our call function to be safe.
    inner: SubmittedWorkDoneClosureInner,
}

#[cfg(send_sync)]
type SubmittedWorkDoneCallback = Box<dyn FnOnce() + Send + 'static>;
#[cfg(not(send_sync))]
type SubmittedWorkDoneCallback = Box<dyn FnOnce() + 'static>;

enum SubmittedWorkDoneClosureInner {
    Rust { callback: SubmittedWorkDoneCallback },
    C { inner: SubmittedWorkDoneClosureC },
}

impl SubmittedWorkDoneClosure {
    pub fn from_rust(callback: SubmittedWorkDoneCallback) -> Self { todo!() }

    /// # Safety
    ///
    /// - The callback pointer must be valid to call with the provided `user_data`
    ///   pointer.
    ///
    /// - Both pointers must point to `'static` data, as the callback may happen at
    ///   an unspecified time.
    pub unsafe fn from_c(inner: SubmittedWorkDoneClosureC) -> Self { todo!() }

    pub(crate) fn call(self) { todo!() }
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
    raw: A::CommandEncoder,
    cmd_buffers: Vec<A::CommandBuffer>,
}

impl<A: HalApi> EncoderInFlight<A> {
    /// Free all of our command buffers.
    ///
    /// Return the command encoder, fully reset and ready to be
    /// reused.
    pub(crate) unsafe fn land(mut self) -> A::CommandEncoder { todo!() }
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

impl<A: HalApi> PendingWrites<A> {
    pub fn new(command_encoder: A::CommandEncoder) -> Self { todo!() }

    pub fn dispose(mut self, device: &A::Device) { todo!() }

    pub fn consume_temp(&mut self, resource: TempResource<A>) { todo!() }

    fn consume(&mut self, buffer: Arc<StagingBuffer<A>>) { todo!() }

    fn pre_submit(&mut self) -> Result<Option<&A::CommandBuffer>, DeviceError> { todo!() }

    #[must_use]
    fn post_submit(
        &mut self,
        command_allocator: &CommandAllocator<A>,
        device: &A::Device,
        queue: &A::Queue,
    ) -> Option<EncoderInFlight<A>> { todo!() }

    pub fn activate(&mut self) -> &mut A::CommandEncoder { todo!() }

    pub fn deactivate(&mut self) { todo!() }
}

fn prepare_staging_buffer<A: HalApi>(
    device: &Arc<Device<A>>,
    size: wgt::BufferAddress,
    instance_flags: wgt::InstanceFlags,
) -> Result<(StagingBuffer<A>, *mut u8), DeviceError> { todo!() }

impl<A: HalApi> StagingBuffer<A> {
    unsafe fn flush(&self, device: &A::Device) -> Result<(), DeviceError> { todo!() }
}

#[derive(Clone, Debug, Error)]
#[error("Queue is invalid")]
pub struct InvalidQueue;

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum QueueWriteError {
    #[error(
        "Device of queue ({:?}) does not match device of write recipient ({:?})",
        queue_device_id,
        target_device_id
    )]
    DeviceMismatch {
        queue_device_id: DeviceId,
        target_device_id: DeviceId,
    },
    #[error(transparent)]
    Queue(#[from] DeviceError),
    #[error(transparent)]
    Transfer(#[from] TransferError),
    #[error(transparent)]
    MemoryInitFailure(#[from] ClearError),
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum QueueSubmitError {
    #[error(transparent)]
    Queue(#[from] DeviceError),
    #[error("Buffer {0:?} is destroyed")]
    DestroyedBuffer(id::BufferId),
    #[error("Texture {0:?} is destroyed")]
    DestroyedTexture(id::TextureId),
    #[error(transparent)]
    Unmap(#[from] BufferAccessError),
    #[error("Buffer {0:?} is still mapped")]
    BufferStillMapped(id::BufferId),
    #[error("Surface output was dropped before the command buffer got submitted")]
    SurfaceOutputDropped,
    #[error("Surface was unconfigured before the command buffer got submitted")]
    SurfaceUnconfigured,
    #[error("GPU got stuck :(")]
    StuckGpu,
}

//TODO: move out common parts of write_xxx.

impl Global {
    pub fn queue_write_buffer<A: HalApi>(
        &self,
        queue_id: QueueId,
        buffer_id: id::BufferId,
        buffer_offset: wgt::BufferAddress,
        data: &[u8],
    ) -> Result<(), QueueWriteError> { todo!() }

    pub fn queue_create_staging_buffer<A: HalApi>(
        &self,
        queue_id: QueueId,
        buffer_size: wgt::BufferSize,
        id_in: Option<id::StagingBufferId>,
    ) -> Result<(id::StagingBufferId, *mut u8), QueueWriteError> { todo!() }

    pub fn queue_write_staging_buffer<A: HalApi>(
        &self,
        queue_id: QueueId,
        buffer_id: id::BufferId,
        buffer_offset: wgt::BufferAddress,
        staging_buffer_id: id::StagingBufferId,
    ) -> Result<(), QueueWriteError> { todo!() }

    pub fn queue_validate_write_buffer<A: HalApi>(
        &self,
        _queue_id: QueueId,
        buffer_id: id::BufferId,
        buffer_offset: u64,
        buffer_size: u64,
    ) -> Result<(), QueueWriteError> { todo!() }

    fn queue_validate_write_buffer_impl<A: HalApi>(
        &self,
        buffer: &Buffer<A>,
        buffer_id: id::BufferId,
        buffer_offset: u64,
        buffer_size: u64,
    ) -> Result<(), TransferError> { todo!() }

    fn queue_write_staging_buffer_impl<A: HalApi>(
        &self,
        device: &Device<A>,
        pending_writes: &mut PendingWrites<A>,
        staging_buffer: &StagingBuffer<A>,
        buffer_id: id::BufferId,
        buffer_offset: u64,
    ) -> Result<(), QueueWriteError> { todo!() }

    pub fn queue_write_texture<A: HalApi>(
        &self,
        queue_id: QueueId,
        destination: &ImageCopyTexture,
        data: &[u8],
        data_layout: &wgt::ImageDataLayout,
        size: &wgt::Extent3d,
    ) -> Result<(), QueueWriteError> { todo!() }

    #[cfg(webgl)]
    pub fn queue_copy_external_image_to_texture<A: HalApi>(
        &self,
        queue_id: QueueId,
        source: &wgt::ImageCopyExternalImage,
        destination: crate::command::ImageCopyTextureTagged,
        size: wgt::Extent3d,
    ) -> Result<(), QueueWriteError> { todo!() }

    pub fn queue_submit<A: HalApi>(
        &self,
        queue_id: QueueId,
        command_buffer_ids: &[id::CommandBufferId],
    ) -> Result<WrappedSubmissionIndex, QueueSubmitError> { todo!() }

    pub fn queue_get_timestamp_period<A: HalApi>(
        &self,
        queue_id: QueueId,
    ) -> Result<f32, InvalidQueue> { todo!() }

    pub fn queue_on_submitted_work_done<A: HalApi>(
        &self,
        queue_id: QueueId,
        closure: SubmittedWorkDoneClosure,
    ) -> Result<(), InvalidQueue> { todo!() }
}
