#[cfg(feature = "trace")]
use crate::device::trace;
use crate::{
    api_log, binding_model, command, conv,
    device::{
        bgl, life::WaitIdleError, map_buffer, queue, DeviceError, DeviceLostClosure,
        DeviceLostReason, HostMap, IMPLICIT_BIND_GROUP_LAYOUT_ERROR_LABEL,
    },
    global::Global,
    hal_api::HalApi,
    id::{self, AdapterId, DeviceId, QueueId, SurfaceId},
    init_tracker::TextureInitTracker,
    instance::{self, Adapter, Surface},
    pipeline, present,
    resource::{self, BufferAccessResult},
    resource::{BufferAccessError, BufferMapOperation, CreateBufferError, Resource},
    validation::check_buffer_usage,
    Label, LabelHelpers as _,
};

use arrayvec::ArrayVec;
use hal::Device as _;
use parking_lot::RwLock;

use wgt::{BufferAddress, TextureFormat};

use std::{
    borrow::Cow,
    iter, ptr,
    sync::{atomic::Ordering, Arc},
};

use super::{ImplicitPipelineIds, InvalidDevice, UserClosures};

impl Global {
    pub fn adapter_is_surface_supported<A: HalApi>(
        &self,
        adapter_id: AdapterId,
        surface_id: SurfaceId,
    ) -> Result<bool, instance::IsSurfaceSupportedError> { todo!() }

    pub fn surface_get_capabilities<A: HalApi>(
        &self,
        surface_id: SurfaceId,
        adapter_id: AdapterId,
    ) -> Result<wgt::SurfaceCapabilities, instance::GetSurfaceSupportError> { todo!() }

    fn fetch_adapter_and_surface<
        A: HalApi,
        F: FnOnce(&Adapter<A>, &Surface) -> Result<B, instance::GetSurfaceSupportError>,
        B,
    >(
        &self,
        surface_id: SurfaceId,
        adapter_id: AdapterId,
        get_supported_callback: F,
    ) -> Result<B, instance::GetSurfaceSupportError> { todo!() }

    pub fn device_features<A: HalApi>(
        &self,
        device_id: DeviceId,
    ) -> Result<wgt::Features, InvalidDevice> { todo!() }

    pub fn device_limits<A: HalApi>(
        &self,
        device_id: DeviceId,
    ) -> Result<wgt::Limits, InvalidDevice> { todo!() }

    pub fn device_downlevel_properties<A: HalApi>(
        &self,
        device_id: DeviceId,
    ) -> Result<wgt::DownlevelCapabilities, InvalidDevice> { todo!() }

    pub fn device_create_buffer<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &resource::BufferDescriptor,
        id_in: Option<id::BufferId>,
    ) -> (id::BufferId, Option<CreateBufferError>) { todo!() }

    /// Assign `id_in` an error with the given `label`.
    ///
    /// Ensure that future attempts to use `id_in` as a buffer ID will propagate
    /// the error, following the WebGPU ["contagious invalidity"] style.
    ///
    /// Firefox uses this function to comply strictly with the WebGPU spec,
    /// which requires [`GPUBufferDescriptor`] validation to be generated on the
    /// Device timeline and leave the newly created [`GPUBuffer`] invalid.
    ///
    /// Ideally, we would simply let [`device_create_buffer`] take care of all
    /// of this, but some errors must be detected before we can even construct a
    /// [`wgpu_types::BufferDescriptor`] to give it. For example, the WebGPU API
    /// allows a `GPUBufferDescriptor`'s [`usage`] property to be any WebIDL
    /// `unsigned long` value, but we can't construct a
    /// [`wgpu_types::BufferUsages`] value from values with unassigned bits
    /// set. This means we must validate `usage` before we can call
    /// `device_create_buffer`.
    ///
    /// When that validation fails, we must arrange for the buffer id to be
    /// considered invalid. This method provides the means to do so.
    ///
    /// ["contagious invalidity"]: https://www.w3.org/TR/webgpu/#invalidity
    /// [`GPUBufferDescriptor`]: https://www.w3.org/TR/webgpu/#dictdef-gpubufferdescriptor
    /// [`GPUBuffer`]: https://www.w3.org/TR/webgpu/#gpubuffer
    /// [`wgpu_types::BufferDescriptor`]: wgt::BufferDescriptor
    /// [`device_create_buffer`]: Global::device_create_buffer
    /// [`usage`]: https://www.w3.org/TR/webgpu/#dom-gputexturedescriptor-usage
    /// [`wgpu_types::BufferUsages`]: wgt::BufferUsages
    pub fn create_buffer_error<A: HalApi>(&self, id_in: Option<id::BufferId>, label: Label) { todo!() }

    pub fn create_render_bundle_error<A: HalApi>(
        &self,
        id_in: Option<id::RenderBundleId>,
        label: Label,
    ) { todo!() }

    /// Assign `id_in` an error with the given `label`.
    ///
    /// See `create_buffer_error` for more context and explanation.
    pub fn create_texture_error<A: HalApi>(&self, id_in: Option<id::TextureId>, label: Label) { todo!() }

    #[cfg(feature = "replay")]
    pub fn device_wait_for_buffer<A: HalApi>(
        &self,
        device_id: DeviceId,
        buffer_id: id::BufferId,
    ) -> Result<(), WaitIdleError> { todo!() }

    #[doc(hidden)]
    pub fn device_set_buffer_sub_data<A: HalApi>(
        &self,
        device_id: DeviceId,
        buffer_id: id::BufferId,
        offset: BufferAddress,
        data: &[u8],
    ) -> BufferAccessResult { todo!() }

    #[doc(hidden)]
    pub fn device_get_buffer_sub_data<A: HalApi>(
        &self,
        device_id: DeviceId,
        buffer_id: id::BufferId,
        offset: BufferAddress,
        data: &mut [u8],
    ) -> BufferAccessResult { todo!() }

    pub fn buffer_label<A: HalApi>(&self, id: id::BufferId) -> String { todo!() }

    pub fn buffer_destroy<A: HalApi>(
        &self,
        buffer_id: id::BufferId,
    ) -> Result<(), resource::DestroyError> { todo!() }

    pub fn buffer_drop<A: HalApi>(&self, buffer_id: id::BufferId, wait: bool) { todo!() }

    pub fn device_create_texture<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &resource::TextureDescriptor,
        id_in: Option<id::TextureId>,
    ) -> (id::TextureId, Option<resource::CreateTextureError>) { todo!() }

    /// # Safety
    ///
    /// - `hal_texture` must be created from `device_id` corresponding raw handle.
    /// - `hal_texture` must be created respecting `desc`
    /// - `hal_texture` must be initialized
    pub unsafe fn create_texture_from_hal<A: HalApi>(
        &self,
        hal_texture: A::Texture,
        device_id: DeviceId,
        desc: &resource::TextureDescriptor,
        id_in: Option<id::TextureId>,
    ) -> (id::TextureId, Option<resource::CreateTextureError>) { todo!() }

    /// # Safety
    ///
    /// - `hal_buffer` must be created from `device_id` corresponding raw handle.
    /// - `hal_buffer` must be created respecting `desc`
    /// - `hal_buffer` must be initialized
    pub unsafe fn create_buffer_from_hal<A: HalApi>(
        &self,
        hal_buffer: A::Buffer,
        device_id: DeviceId,
        desc: &resource::BufferDescriptor,
        id_in: Option<id::BufferId>,
    ) -> (id::BufferId, Option<CreateBufferError>) { todo!() }

    pub fn texture_label<A: HalApi>(&self, id: id::TextureId) -> String { todo!() }

    pub fn texture_destroy<A: HalApi>(
        &self,
        texture_id: id::TextureId,
    ) -> Result<(), resource::DestroyError> { todo!() }

    pub fn texture_drop<A: HalApi>(&self, texture_id: id::TextureId, wait: bool) { todo!() }

    #[allow(unused_unsafe)]
    pub fn texture_create_view<A: HalApi>(
        &self,
        texture_id: id::TextureId,
        desc: &resource::TextureViewDescriptor,
        id_in: Option<id::TextureViewId>,
    ) -> (id::TextureViewId, Option<resource::CreateTextureViewError>) { todo!() }

    pub fn texture_view_label<A: HalApi>(&self, id: id::TextureViewId) -> String { todo!() }

    pub fn texture_view_drop<A: HalApi>(
        &self,
        texture_view_id: id::TextureViewId,
        wait: bool,
    ) -> Result<(), resource::TextureViewDestroyError> { todo!() }

    pub fn device_create_sampler<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &resource::SamplerDescriptor,
        id_in: Option<id::SamplerId>,
    ) -> (id::SamplerId, Option<resource::CreateSamplerError>) { todo!() }

    pub fn sampler_label<A: HalApi>(&self, id: id::SamplerId) -> String { todo!() }

    pub fn sampler_drop<A: HalApi>(&self, sampler_id: id::SamplerId) { todo!() }

    pub fn device_create_bind_group_layout<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &binding_model::BindGroupLayoutDescriptor,
        id_in: Option<id::BindGroupLayoutId>,
    ) -> (
        id::BindGroupLayoutId,
        Option<binding_model::CreateBindGroupLayoutError>,
    ) { todo!() }

    pub fn bind_group_layout_label<A: HalApi>(&self, id: id::BindGroupLayoutId) -> String { todo!() }

    pub fn bind_group_layout_drop<A: HalApi>(&self, bind_group_layout_id: id::BindGroupLayoutId) { todo!() }

    pub fn device_create_pipeline_layout<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &binding_model::PipelineLayoutDescriptor,
        id_in: Option<id::PipelineLayoutId>,
    ) -> (
        id::PipelineLayoutId,
        Option<binding_model::CreatePipelineLayoutError>,
    ) { todo!() }

    pub fn pipeline_layout_label<A: HalApi>(&self, id: id::PipelineLayoutId) -> String { todo!() }

    pub fn pipeline_layout_drop<A: HalApi>(&self, pipeline_layout_id: id::PipelineLayoutId) { todo!() }

    pub fn device_create_bind_group<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &binding_model::BindGroupDescriptor,
        id_in: Option<id::BindGroupId>,
    ) -> (id::BindGroupId, Option<binding_model::CreateBindGroupError>) { todo!() }

    pub fn bind_group_label<A: HalApi>(&self, id: id::BindGroupId) -> String { todo!() }

    pub fn bind_group_drop<A: HalApi>(&self, bind_group_id: id::BindGroupId) { todo!() }

    /// Create a shader module with the given `source`.
    ///
    /// <div class="warning">
    // NOTE: Keep this in sync with `naga::front::wgsl::parse_str`!
    // NOTE: Keep this in sync with `wgpu::Device::create_shader_module`!
    ///
    /// This function may consume a lot of stack space. Compiler-enforced limits for parsing
    /// recursion exist; if shader compilation runs into them, it will return an error gracefully.
    /// However, on some build profiles and platforms, the default stack size for a thread may be
    /// exceeded before this limit is reached during parsing. Callers should ensure that there is
    /// enough stack space for this, particularly if calls to this method are exposed to user
    /// input.
    ///
    /// </div>
    pub fn device_create_shader_module<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &pipeline::ShaderModuleDescriptor,
        source: pipeline::ShaderModuleSource,
        id_in: Option<id::ShaderModuleId>,
    ) -> (
        id::ShaderModuleId,
        Option<pipeline::CreateShaderModuleError>,
    ) { todo!() }

    // Unsafe-ness of internal calls has little to do with unsafe-ness of this.
    #[allow(unused_unsafe)]
    /// # Safety
    ///
    /// This function passes SPIR-V binary to the backend as-is and can potentially result in a
    /// driver crash.
    pub unsafe fn device_create_shader_module_spirv<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &pipeline::ShaderModuleDescriptor,
        source: Cow<[u32]>,
        id_in: Option<id::ShaderModuleId>,
    ) -> (
        id::ShaderModuleId,
        Option<pipeline::CreateShaderModuleError>,
    ) { todo!() }

    pub fn shader_module_label<A: HalApi>(&self, id: id::ShaderModuleId) -> String { todo!() }

    pub fn shader_module_drop<A: HalApi>(&self, shader_module_id: id::ShaderModuleId) { todo!() }

    pub fn device_create_command_encoder<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &wgt::CommandEncoderDescriptor<Label>,
        id_in: Option<id::CommandEncoderId>,
    ) -> (id::CommandEncoderId, Option<DeviceError>) { todo!() }

    pub fn command_buffer_label<A: HalApi>(&self, id: id::CommandBufferId) -> String { todo!() }

    pub fn command_encoder_drop<A: HalApi>(&self, command_encoder_id: id::CommandEncoderId) { todo!() }

    pub fn command_buffer_drop<A: HalApi>(&self, command_buffer_id: id::CommandBufferId) { todo!() }

    pub fn device_create_render_bundle_encoder(
        &self,
        device_id: DeviceId,
        desc: &command::RenderBundleEncoderDescriptor,
    ) -> (
        *mut command::RenderBundleEncoder,
        Option<command::CreateRenderBundleError>,
    ) { todo!() }

    pub fn render_bundle_encoder_finish<A: HalApi>(
        &self,
        bundle_encoder: command::RenderBundleEncoder,
        desc: &command::RenderBundleDescriptor,
        id_in: Option<id::RenderBundleId>,
    ) -> (id::RenderBundleId, Option<command::RenderBundleError>) { todo!() }

    pub fn render_bundle_label<A: HalApi>(&self, id: id::RenderBundleId) -> String { todo!() }

    pub fn render_bundle_drop<A: HalApi>(&self, render_bundle_id: id::RenderBundleId) { todo!() }

    pub fn device_create_query_set<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &resource::QuerySetDescriptor,
        id_in: Option<id::QuerySetId>,
    ) -> (id::QuerySetId, Option<resource::CreateQuerySetError>) { todo!() }

    pub fn query_set_drop<A: HalApi>(&self, query_set_id: id::QuerySetId) { todo!() }

    pub fn query_set_label<A: HalApi>(&self, id: id::QuerySetId) -> String { todo!() }

    pub fn device_create_render_pipeline<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &pipeline::RenderPipelineDescriptor,
        id_in: Option<id::RenderPipelineId>,
        implicit_pipeline_ids: Option<ImplicitPipelineIds<'_>>,
    ) -> (
        id::RenderPipelineId,
        Option<pipeline::CreateRenderPipelineError>,
    ) { todo!() }

    /// Get an ID of one of the bind group layouts. The ID adds a refcount,
    /// which needs to be released by calling `bind_group_layout_drop`.
    pub fn render_pipeline_get_bind_group_layout<A: HalApi>(
        &self,
        pipeline_id: id::RenderPipelineId,
        index: u32,
        id_in: Option<id::BindGroupLayoutId>,
    ) -> (
        id::BindGroupLayoutId,
        Option<binding_model::GetBindGroupLayoutError>,
    ) { todo!() }

    pub fn render_pipeline_label<A: HalApi>(&self, id: id::RenderPipelineId) -> String { todo!() }

    pub fn render_pipeline_drop<A: HalApi>(&self, render_pipeline_id: id::RenderPipelineId) { todo!() }

    pub fn device_create_compute_pipeline<A: HalApi>(
        &self,
        device_id: DeviceId,
        desc: &pipeline::ComputePipelineDescriptor,
        id_in: Option<id::ComputePipelineId>,
        implicit_pipeline_ids: Option<ImplicitPipelineIds<'_>>,
    ) -> (
        id::ComputePipelineId,
        Option<pipeline::CreateComputePipelineError>,
    ) { todo!() }

    /// Get an ID of one of the bind group layouts. The ID adds a refcount,
    /// which needs to be released by calling `bind_group_layout_drop`.
    pub fn compute_pipeline_get_bind_group_layout<A: HalApi>(
        &self,
        pipeline_id: id::ComputePipelineId,
        index: u32,
        id_in: Option<id::BindGroupLayoutId>,
    ) -> (
        id::BindGroupLayoutId,
        Option<binding_model::GetBindGroupLayoutError>,
    ) { todo!() }

    pub fn compute_pipeline_label<A: HalApi>(&self, id: id::ComputePipelineId) -> String { todo!() }

    pub fn compute_pipeline_drop<A: HalApi>(&self, compute_pipeline_id: id::ComputePipelineId) { todo!() }

    pub fn surface_configure<A: HalApi>(
        &self,
        surface_id: SurfaceId,
        device_id: DeviceId,
        config: &wgt::SurfaceConfiguration<Vec<TextureFormat>>,
    ) -> Option<present::ConfigureSurfaceError> { todo!() }

    #[cfg(feature = "replay")]
    /// Only triangle suspected resource IDs. This helps us to avoid ID collisions
    /// upon creating new resources when re-playing a trace.
    pub fn device_maintain_ids<A: HalApi>(&self, device_id: DeviceId) -> Result<(), InvalidDevice> { todo!() }

    /// Check `device_id` for freeable resources and completed buffer mappings.
    ///
    /// Return `queue_empty` indicating whether there are more queue submissions still in flight.
    pub fn device_poll<A: HalApi>(
        &self,
        device_id: DeviceId,
        maintain: wgt::Maintain<queue::WrappedSubmissionIndex>,
    ) -> Result<bool, WaitIdleError> { todo!() }

    fn poll_single_device<A: HalApi>(
        device: &crate::device::Device<A>,
        maintain: wgt::Maintain<queue::WrappedSubmissionIndex>,
    ) -> Result<DevicePoll, WaitIdleError> { todo!() }

    /// Poll all devices belonging to the backend `A`.
    ///
    /// If `force_wait` is true, block until all buffer mappings are done.
    ///
    /// Return `all_queue_empty` indicating whether there are more queue
    /// submissions still in flight.
    fn poll_all_devices_of_api<A: HalApi>(
        &self,
        force_wait: bool,
        closures: &mut UserClosures,
    ) -> Result<bool, WaitIdleError> { todo!() }

    /// Poll all devices on all backends.
    ///
    /// This is the implementation of `wgpu::Instance::poll_all`.
    ///
    /// Return `all_queue_empty` indicating whether there are more queue
    /// submissions still in flight.
    pub fn poll_all_devices(&self, force_wait: bool) -> Result<bool, WaitIdleError> { todo!() }

    pub fn device_label<A: HalApi>(&self, id: DeviceId) -> String { todo!() }

    pub fn device_start_capture<A: HalApi>(&self, id: DeviceId) { todo!() }

    pub fn device_stop_capture<A: HalApi>(&self, id: DeviceId) { todo!() }

    // This is a test-only function to force the device into an
    // invalid state by inserting an error value in its place in
    // the registry.
    pub fn device_make_invalid<A: HalApi>(&self, device_id: DeviceId) { todo!() }

    pub fn device_drop<A: HalApi>(&self, device_id: DeviceId) { todo!() }

    // This closure will be called exactly once during "lose the device",
    // or when it is replaced.
    pub fn device_set_device_lost_closure<A: HalApi>(
        &self,
        device_id: DeviceId,
        device_lost_closure: DeviceLostClosure,
    ) { todo!() }

    pub fn device_destroy<A: HalApi>(&self, device_id: DeviceId) { todo!() }

    pub fn device_mark_lost<A: HalApi>(&self, device_id: DeviceId, message: &str) { todo!() }

    pub fn queue_drop<A: HalApi>(&self, queue_id: QueueId) { todo!() }

    pub fn buffer_map_async<A: HalApi>(
        &self,
        buffer_id: id::BufferId,
        offset: BufferAddress,
        size: Option<BufferAddress>,
        op: BufferMapOperation,
    ) -> BufferAccessResult { todo!() }

    // Returns the mapping callback in case of error so that the callback can be fired outside
    // of the locks that are held in this function.
    fn buffer_map_async_inner<A: HalApi>(
        &self,
        buffer_id: id::BufferId,
        offset: BufferAddress,
        size: Option<BufferAddress>,
        op: BufferMapOperation,
    ) -> Result<(), (BufferMapOperation, BufferAccessError)> { todo!() }

    pub fn buffer_get_mapped_range<A: HalApi>(
        &self,
        buffer_id: id::BufferId,
        offset: BufferAddress,
        size: Option<BufferAddress>,
    ) -> Result<(*mut u8, u64), BufferAccessError> { todo!() }
    pub fn buffer_unmap<A: HalApi>(&self, buffer_id: id::BufferId) -> BufferAccessResult { todo!() }
}

struct DevicePoll {
    closures: UserClosures,
    queue_empty: bool,
}
