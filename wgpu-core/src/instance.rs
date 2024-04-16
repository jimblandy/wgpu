use std::sync::Arc;

use crate::{
    any_surface::AnySurface,
    api_log,
    device::{queue::Queue, resource::Device, DeviceDescriptor},
    global::Global,
    hal_api::HalApi,
    id::markers,
    id::{AdapterId, DeviceId, Id, Marker, QueueId, SurfaceId},
    present::Presentation,
    resource::{Resource, ResourceInfo, ResourceType},
    resource_log, LabelHelpers, DOWNLEVEL_WARNING_MESSAGE,
};

use parking_lot::Mutex;
use wgt::{Backend, Backends, PowerPreference};

use hal::{Adapter as _, Instance as _, OpenDevice};
use thiserror::Error;

pub type RequestAdapterOptions = wgt::RequestAdapterOptions<SurfaceId>;
type HalInstance<A> = <A as hal::Api>::Instance;

#[derive(Clone, Debug, Error)]
#[error("Limit '{name}' value {requested} is better than allowed {allowed}")]
pub struct FailedLimit {
    name: &'static str,
    requested: u64,
    allowed: u64,
}

fn check_limits(requested: &wgt::Limits, allowed: &wgt::Limits) -> Vec<FailedLimit> { todo!() }

#[test]
fn downlevel_default_limits_less_than_default_limits() { todo!() }

#[derive(Default)]
pub struct Instance {
    #[allow(dead_code)]
    pub name: String,
    #[cfg(vulkan)]
    pub vulkan: Option<HalInstance<hal::api::Vulkan>>,
    #[cfg(metal)]
    pub metal: Option<HalInstance<hal::api::Metal>>,
    #[cfg(dx12)]
    pub dx12: Option<HalInstance<hal::api::Dx12>>,
    #[cfg(gles)]
    pub gl: Option<HalInstance<hal::api::Gles>>,
    pub flags: wgt::InstanceFlags,
}

impl Instance {
    pub fn new(name: &str, instance_desc: wgt::InstanceDescriptor) -> Self { todo!() }

    pub(crate) fn destroy_surface(&self, surface: Surface) { todo!() }
}

pub struct Surface {
    pub(crate) presentation: Mutex<Option<Presentation>>,
    pub(crate) info: ResourceInfo<Surface>,
    pub(crate) raw: AnySurface,
}

impl Resource for Surface {
    const TYPE: ResourceType = "Surface";

    type Marker = markers::Surface;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }

    fn label(&self) -> String { todo!() }
}

impl Surface {
    pub fn get_capabilities<A: HalApi>(
        &self,
        adapter: &Adapter<A>,
    ) -> Result<hal::SurfaceCapabilities, GetSurfaceSupportError> { todo!() }
}

pub struct Adapter<A: HalApi> {
    pub(crate) raw: hal::ExposedAdapter<A>,
    pub(crate) info: ResourceInfo<Adapter<A>>,
}

impl<A: HalApi> Adapter<A> {
    fn new(mut raw: hal::ExposedAdapter<A>) -> Self { todo!() }

    pub fn is_surface_supported(&self, surface: &Surface) -> bool { todo!() }

    pub(crate) fn get_texture_format_features(
        &self,
        format: wgt::TextureFormat,
    ) -> wgt::TextureFormatFeatures { todo!() }

    fn create_device_and_queue_from_hal(
        self: &Arc<Self>,
        hal_device: OpenDevice<A>,
        desc: &DeviceDescriptor,
        instance_flags: wgt::InstanceFlags,
        trace_path: Option<&std::path::Path>,
    ) -> Result<(Device<A>, Queue<A>), RequestDeviceError> { todo!() }

    fn create_device_and_queue(
        self: &Arc<Self>,
        desc: &DeviceDescriptor,
        instance_flags: wgt::InstanceFlags,
        trace_path: Option<&std::path::Path>,
    ) -> Result<(Device<A>, Queue<A>), RequestDeviceError> { todo!() }
}

impl<A: HalApi> Resource for Adapter<A> {
    const TYPE: ResourceType = "Adapter";

    type Marker = markers::Adapter;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum IsSurfaceSupportedError {
    #[error("Invalid adapter")]
    InvalidAdapter,
    #[error("Invalid surface")]
    InvalidSurface,
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum GetSurfaceSupportError {
    #[error("Invalid adapter")]
    InvalidAdapter,
    #[error("Invalid surface")]
    InvalidSurface,
    #[error("Surface is not supported by the adapter")]
    Unsupported,
}

#[derive(Clone, Debug, Error)]
/// Error when requesting a device from the adaptor
#[non_exhaustive]
pub enum RequestDeviceError {
    #[error("Parent adapter is invalid")]
    InvalidAdapter,
    #[error("Connection to device was lost during initialization")]
    DeviceLost,
    #[error("Device initialization failed due to implementation specific errors")]
    Internal,
    #[error(transparent)]
    LimitsExceeded(#[from] FailedLimit),
    #[error("Device has no queue supporting graphics")]
    NoGraphicsQueue,
    #[error("Not enough memory left to request device")]
    OutOfMemory,
    #[error("Unsupported features were requested: {0:?}")]
    UnsupportedFeature(wgt::Features),
}

pub enum AdapterInputs<'a, M: Marker> {
    IdSet(&'a [Id<M>]),
    Mask(Backends, fn(Backend) -> Option<Id<M>>),
}

impl<M: Marker> AdapterInputs<'_, M> {
    fn find(&self, b: Backend) -> Option<Option<Id<M>>> { todo!() }
}

#[derive(Clone, Debug, Error)]
#[error("Adapter is invalid")]
pub struct InvalidAdapter;

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum RequestAdapterError {
    #[error("No suitable adapter found")]
    NotFound,
    #[error("Surface {0:?} is invalid")]
    InvalidSurface(SurfaceId),
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum CreateSurfaceError {
    #[error("No backend is available")]
    NoSupportedBackend,
    #[error(transparent)]
    InstanceError(#[from] hal::InstanceError),
}

impl Global {
    /// # Safety
    ///
    /// - `display_handle` must be a valid object to create a surface upon.
    /// - `window_handle` must remain valid as long as the returned
    ///   [`SurfaceId`] is being used.
    #[cfg(feature = "raw-window-handle")]
    pub unsafe fn instance_create_surface(
        &self,
        display_handle: raw_window_handle::RawDisplayHandle,
        window_handle: raw_window_handle::RawWindowHandle,
        id_in: Option<SurfaceId>,
    ) -> Result<SurfaceId, CreateSurfaceError> { todo!() }

    /// # Safety
    ///
    /// `layer` must be a valid pointer.
    #[cfg(metal)]
    pub unsafe fn instance_create_surface_metal(
        &self,
        layer: *mut std::ffi::c_void,
        id_in: Option<SurfaceId>,
    ) -> SurfaceId { todo!() }

    #[cfg(dx12)]
    /// # Safety
    ///
    /// The visual must be valid and able to be used to make a swapchain with.
    pub unsafe fn instance_create_surface_from_visual(
        &self,
        visual: *mut std::ffi::c_void,
        id_in: Option<SurfaceId>,
    ) -> SurfaceId { todo!() }

    #[cfg(dx12)]
    /// # Safety
    ///
    /// The surface_handle must be valid and able to be used to make a swapchain with.
    pub unsafe fn instance_create_surface_from_surface_handle(
        &self,
        surface_handle: *mut std::ffi::c_void,
        id_in: Option<SurfaceId>,
    ) -> SurfaceId { todo!() }

    #[cfg(dx12)]
    /// # Safety
    ///
    /// The swap_chain_panel must be valid and able to be used to make a swapchain with.
    pub unsafe fn instance_create_surface_from_swap_chain_panel(
        &self,
        swap_chain_panel: *mut std::ffi::c_void,
        id_in: Option<SurfaceId>,
    ) -> SurfaceId { todo!() }

    pub fn surface_drop(&self, id: SurfaceId) { todo!() }

    fn enumerate<A: HalApi>(
        &self,
        _: A,
        instance: &Option<A::Instance>,
        inputs: &AdapterInputs<markers::Adapter>,
        list: &mut Vec<AdapterId>,
    ) { todo!() }

    pub fn enumerate_adapters(&self, inputs: AdapterInputs<markers::Adapter>) -> Vec<AdapterId> { todo!() }

    fn select<A: HalApi>(
        &self,
        selected: &mut usize,
        new_id: Option<AdapterId>,
        mut list: Vec<hal::ExposedAdapter<A>>,
    ) -> Option<AdapterId> { todo!() }

    pub fn request_adapter(
        &self,
        desc: &RequestAdapterOptions,
        inputs: AdapterInputs<markers::Adapter>,
    ) -> Result<AdapterId, RequestAdapterError> { todo!() }

    /// # Safety
    ///
    /// `hal_adapter` must be created from this global internal instance handle.
    pub unsafe fn create_adapter_from_hal<A: HalApi>(
        &self,
        hal_adapter: hal::ExposedAdapter<A>,
        input: Option<AdapterId>,
    ) -> AdapterId { todo!() }

    pub fn adapter_get_info<A: HalApi>(
        &self,
        adapter_id: AdapterId,
    ) -> Result<wgt::AdapterInfo, InvalidAdapter> { todo!() }

    pub fn adapter_get_texture_format_features<A: HalApi>(
        &self,
        adapter_id: AdapterId,
        format: wgt::TextureFormat,
    ) -> Result<wgt::TextureFormatFeatures, InvalidAdapter> { todo!() }

    pub fn adapter_features<A: HalApi>(
        &self,
        adapter_id: AdapterId,
    ) -> Result<wgt::Features, InvalidAdapter> { todo!() }

    pub fn adapter_limits<A: HalApi>(
        &self,
        adapter_id: AdapterId,
    ) -> Result<wgt::Limits, InvalidAdapter> { todo!() }

    pub fn adapter_downlevel_capabilities<A: HalApi>(
        &self,
        adapter_id: AdapterId,
    ) -> Result<wgt::DownlevelCapabilities, InvalidAdapter> { todo!() }

    pub fn adapter_get_presentation_timestamp<A: HalApi>(
        &self,
        adapter_id: AdapterId,
    ) -> Result<wgt::PresentationTimestamp, InvalidAdapter> { todo!() }

    pub fn adapter_drop<A: HalApi>(&self, adapter_id: AdapterId) { todo!() }
}

impl Global {
    pub fn adapter_request_device<A: HalApi>(
        &self,
        adapter_id: AdapterId,
        desc: &DeviceDescriptor,
        trace_path: Option<&std::path::Path>,
        device_id_in: Option<DeviceId>,
        queue_id_in: Option<QueueId>,
    ) -> (DeviceId, QueueId, Option<RequestDeviceError>) { todo!() }

    /// # Safety
    ///
    /// - `hal_device` must be created from `adapter_id` or its internal handle.
    /// - `desc` must be a subset of `hal_device` features and limits.
    pub unsafe fn create_device_from_hal<A: HalApi>(
        &self,
        adapter_id: AdapterId,
        hal_device: OpenDevice<A>,
        desc: &DeviceDescriptor,
        trace_path: Option<&std::path::Path>,
        device_id_in: Option<DeviceId>,
        queue_id_in: Option<QueueId>,
    ) -> (DeviceId, QueueId, Option<RequestDeviceError>) { todo!() }
}

/// Generates a set of backends from a comma separated list of case-insensitive backend names.
///
/// Whitespace is stripped, so both 'gl, dx12' and 'gl,dx12' are valid.
///
/// Always returns WEBGPU on wasm over webgpu.
///
/// Names:
/// - vulkan = "vulkan" or "vk"
/// - dx12   = "dx12" or "d3d12"
/// - metal  = "metal" or "mtl"
/// - gles   = "opengl" or "gles" or "gl"
/// - webgpu = "webgpu"
pub fn parse_backends_from_comma_list(string: &str) -> Backends { todo!() }
