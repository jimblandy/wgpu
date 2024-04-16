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

#[derive(Debug, Copy, Clone)]
pub enum RenderPassCompatibilityCheckType {
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
}

pub type BufferMapPendingClosure = (BufferMapOperation, BufferAccessResult);

#[derive(Default)]
pub struct UserClosures {
    pub mappings: Vec<BufferMapPendingClosure>,
    pub submissions: SmallVec<[queue::SubmittedWorkDoneClosure; 1]>,
    pub device_lost_invocations: SmallVec<[DeviceLostInvocation; 1]>,
}


pub(crate) struct DeviceLostClosureRust {
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
}

pub struct DeviceLostInvocation {
}

#[derive(Clone, Debug, Error)]
#[error("Device is invalid")]
pub struct InvalidDevice;

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum DeviceError {
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

