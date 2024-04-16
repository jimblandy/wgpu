use crate::{
    binding_model,
    hal_api::HalApi,
    hub::Hub,
    id::{BindGroupLayoutId, PipelineLayoutId},
    resource::{Buffer, BufferAccessError, BufferAccessResult, BufferMapOperation},
    snatch::SnatchGuard,
    Label, DOWNLEVEL_ERROR_MESSAGE,
};

use arrayvec::ArrayVec;
use hal::Device as _;
use smallvec::SmallVec;
use std::os::raw::c_char;
use thiserror::Error;
use wgt::{BufferAddress, DeviceLostReason, TextureFormat};

use std::{iter, num::NonZeroU32, ptr};

pub mod any_device;
pub(crate) mod bgl;
pub mod global;
mod life;
pub mod queue;
pub mod resource;
#[cfg(any(feature = "trace", feature = "replay"))]
pub mod trace;
pub use {life::WaitIdleError, resource::Device};

pub const SHADER_STAGE_COUNT: usize = hal::MAX_CONCURRENT_SHADER_STAGES;
// Should be large enough for the largest possible texture row. This
// value is enough for a 16k texture with float4 format.
pub(crate) const ZERO_BUFFER_SIZE: BufferAddress = 512 << 10;

const CLEANUP_WAIT_MS: u32 = 5000;

const IMPLICIT_BIND_GROUP_LAYOUT_ERROR_LABEL: &str = "Implicit BindGroupLayout in the Error State";
const ENTRYPOINT_FAILURE_ERROR: &str = "The given EntryPoint is Invalid";

pub type DeviceDescriptor<'a> = wgt::DeviceDescriptor<Label<'a>>;

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum HostMap {
    Read,
    Write,
}

#[derive(Clone, Debug, Hash, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub(crate) struct AttachmentData<T> {
    pub colors: ArrayVec<Option<T>, { hal::MAX_COLOR_ATTACHMENTS }>,
    pub resolves: ArrayVec<T, { hal::MAX_COLOR_ATTACHMENTS }>,
    pub depth_stencil: Option<T>,
}
impl<T: PartialEq> Eq for AttachmentData<T> {}
impl<T> AttachmentData<T> {
    pub(crate) fn map<U, F: Fn(&T) -> U>(&self, fun: F) -> AttachmentData<U> { todo!() }
}

#[derive(Debug, Copy, Clone)]
pub enum RenderPassCompatibilityCheckType {
    RenderPipeline,
    RenderBundle,
}

#[derive(Clone, Debug, Hash, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub(crate) struct RenderPassContext {
    pub attachments: AttachmentData<TextureFormat>,
    pub sample_count: u32,
    pub multiview: Option<NonZeroU32>,
}
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum RenderPassCompatibilityError {
    #[error(
        "Incompatible color attachments at indices {indices:?}: the RenderPass uses textures with formats {expected:?} but the {ty:?} uses attachments with formats {actual:?}",
    )]
    IncompatibleColorAttachment {
        indices: Vec<usize>,
        expected: Vec<Option<TextureFormat>>,
        actual: Vec<Option<TextureFormat>>,
        ty: RenderPassCompatibilityCheckType,
    },
    #[error(
        "Incompatible depth-stencil attachment format: the RenderPass uses a texture with format {expected:?} but the {ty:?} uses an attachment with format {actual:?}",
    )]
    IncompatibleDepthStencilAttachment {
        expected: Option<TextureFormat>,
        actual: Option<TextureFormat>,
        ty: RenderPassCompatibilityCheckType,
    },
    #[error(
        "Incompatible sample count: the RenderPass uses textures with sample count {expected:?} but the {ty:?} uses attachments with format {actual:?}",
    )]
    IncompatibleSampleCount {
        expected: u32,
        actual: u32,
        ty: RenderPassCompatibilityCheckType,
    },
    #[error("Incompatible multiview setting: the RenderPass uses setting {expected:?} but the {ty:?} uses setting {actual:?}")]
    IncompatibleMultiview {
        expected: Option<NonZeroU32>,
        actual: Option<NonZeroU32>,
        ty: RenderPassCompatibilityCheckType,
    },
}

impl RenderPassContext {
    // Assumes the renderpass only contains one subpass
    pub(crate) fn check_compatible(
        &self,
        other: &Self,
        ty: RenderPassCompatibilityCheckType,
    ) -> Result<(), RenderPassCompatibilityError> { todo!() }
}

pub type BufferMapPendingClosure = (BufferMapOperation, BufferAccessResult);

#[derive(Default)]
pub struct UserClosures {
    pub mappings: Vec<BufferMapPendingClosure>,
    pub submissions: SmallVec<[queue::SubmittedWorkDoneClosure; 1]>,
    pub device_lost_invocations: SmallVec<[DeviceLostInvocation; 1]>,
}

impl UserClosures {
    fn extend(&mut self, other: Self) { todo!() }

    fn fire(self) { todo!() }
}

#[cfg(send_sync)]
pub type DeviceLostCallback = Box<dyn Fn(DeviceLostReason, String) + Send + 'static>;
#[cfg(not(send_sync))]
pub type DeviceLostCallback = Box<dyn Fn(DeviceLostReason, String) + 'static>;

pub struct DeviceLostClosureRust {
    pub callback: DeviceLostCallback,
    consumed: bool,
}

impl Drop for DeviceLostClosureRust {
    fn drop(&mut self) { todo!() }
}

#[repr(C)]
pub struct DeviceLostClosureC {
    pub callback: unsafe extern "C" fn(user_data: *mut u8, reason: u8, message: *const c_char),
    pub user_data: *mut u8,
    consumed: bool,
}

#[cfg(send_sync)]
unsafe impl Send for DeviceLostClosureC {}

impl Drop for DeviceLostClosureC {
    fn drop(&mut self) { todo!() }
}

pub struct DeviceLostClosure {
    // We wrap this so creating the enum in the C variant can be unsafe,
    // allowing our call function to be safe.
    inner: DeviceLostClosureInner,
}

pub struct DeviceLostInvocation {
    closure: DeviceLostClosure,
    reason: DeviceLostReason,
    message: String,
}

enum DeviceLostClosureInner {
    Rust { inner: DeviceLostClosureRust },
    C { inner: DeviceLostClosureC },
}

impl DeviceLostClosure {
    pub fn from_rust(callback: DeviceLostCallback) -> Self { todo!() }

    /// # Safety
    ///
    /// - The callback pointer must be valid to call with the provided `user_data`
    ///   pointer.
    ///
    /// - Both pointers must point to `'static` data, as the callback may happen at
    ///   an unspecified time.
    pub unsafe fn from_c(mut closure: DeviceLostClosureC) -> Self { todo!() }

    pub(crate) fn call(self, reason: DeviceLostReason, message: String) { todo!() }
}

fn map_buffer<A: HalApi>(
    raw: &A::Device,
    buffer: &Buffer<A>,
    offset: BufferAddress,
    size: BufferAddress,
    kind: HostMap,
    snatch_guard: &SnatchGuard,
) -> Result<ptr::NonNull<u8>, BufferAccessError> { todo!() }

#[derive(Clone, Debug, Error)]
#[error("Device is invalid")]
pub struct InvalidDevice;

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum DeviceError {
    #[error("Parent device is invalid.")]
    Invalid,
    #[error("Parent device is lost")]
    Lost,
    #[error("Not enough memory left.")]
    OutOfMemory,
    #[error("Creation of a resource failed for a reason other than running out of memory.")]
    ResourceCreationFailed,
    #[error("QueueId is invalid")]
    InvalidQueueId,
    #[error("Attempt to use a resource with a different device from the one that created it")]
    WrongDevice,
}

impl From<hal::DeviceError> for DeviceError {
    fn from(error: hal::DeviceError) -> Self { todo!() }
}

#[derive(Clone, Debug, Error)]
#[error("Features {0:?} are required but not enabled on the device")]
pub struct MissingFeatures(pub wgt::Features);

#[derive(Clone, Debug, Error)]
#[error(
    "Downlevel flags {0:?} are required but not supported on the device.\n{}",
    DOWNLEVEL_ERROR_MESSAGE
)]
pub struct MissingDownlevelFlags(pub wgt::DownlevelFlags);

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ImplicitPipelineContext {
    pub root_id: PipelineLayoutId,
    pub group_ids: ArrayVec<BindGroupLayoutId, { hal::MAX_BIND_GROUPS }>,
}

pub struct ImplicitPipelineIds<'a> {
    pub root_id: Option<PipelineLayoutId>,
    pub group_ids: &'a [Option<BindGroupLayoutId>],
}

impl ImplicitPipelineIds<'_> {
    fn prepare<A: HalApi>(self, hub: &Hub<A>) -> ImplicitPipelineContext { todo!() }
}
