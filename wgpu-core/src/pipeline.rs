#[cfg(feature = "trace")]
use crate::device::trace;
use crate::{
    binding_model::{CreateBindGroupLayoutError, CreatePipelineLayoutError, PipelineLayout},
    command::ColorAttachmentError,
    device::{Device, DeviceError, MissingDownlevelFlags, MissingFeatures, RenderPassContext},
    hal_api::HalApi,
    resource::{Resource, ResourceInfo, ResourceType},
    resource_log, validation, Label,
};
use arrayvec::ArrayVec;
use std::{borrow::Cow, error::Error, fmt, marker::PhantomData, num::NonZeroU32, sync::Arc};
use thiserror::Error;

/// Information about buffer bindings, which
/// is validated against the shader (and pipeline)
/// at draw time as opposed to initialization time.
#[derive(Debug)]
pub(crate) struct LateSizedBufferGroup {
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ShaderModuleDescriptor<'a> {
    pub label: Label<'a>,
    #[cfg_attr(feature = "serde", serde(default))]
    pub shader_bound_checks: wgt::ShaderBoundChecks,
}

#[derive(Debug)]
#[allow(dead_code)] // JIMB 38s
pub struct ShaderModule<A: HalApi> {
    pub(crate) raw: Option<A::ShaderModule>,
    pub(crate) device: Arc<Device<A>>,
    pub(crate) interface: Option<validation::Interface>,
    pub(crate) info: ResourceInfo<ShaderModule<A>>,
    pub(crate) label: String,
}

impl<A: HalApi> Drop for ShaderModule<A> {
    fn drop(&mut self) { todo!() }
}

impl<A: HalApi> Resource for ShaderModule<A> {
    const TYPE: ResourceType = "ShaderModule";

    type Marker = crate::id::markers::ShaderModule;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }

    fn label(&self) -> String { todo!() }
}

#[derive(Clone, Debug)]
pub struct ShaderError<E> {
    pub source: String,
    pub label: Option<String>,
    pub inner: Box<E>,
}
#[cfg(feature = "wgsl")]
impl fmt::Display for ShaderError<naga::front::wgsl::ParseError> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}
#[cfg(feature = "glsl")]
impl fmt::Display for ShaderError<naga::front::glsl::ParseError> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}
#[cfg(feature = "spirv")]
impl fmt::Display for ShaderError<naga::front::spv::Error> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}
impl fmt::Display for ShaderError<naga::WithSpan<naga::valid::ValidationError>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}
impl<E> Error for ShaderError<E>
where
    ShaderError<E>: fmt::Display,
    E: Error + 'static,
{
    fn source(&self) -> Option<&(dyn Error + 'static)> { todo!() }
}

//Note: `Clone` would require `WithSpan: Clone`.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum CreateShaderModuleError {
}

/// Describes a programmable pipeline stage.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ProgrammableStageDescriptor<'a> {
    /// The compiled shader module for this stage.
    pub module: (),
    /// The name of the entry point in the compiled shader. The name is selected using the
    /// following logic:
    ///
    /// * If `Some(name)` is specified, there must be a function with this name in the shader.
    /// * If a single entry point associated with this stage must be in the shader, then proceed as
    ///   if `Some(…)` was specified with that entry point's name.
    pub entry_point: Option<Cow<'a, str>>,
    /// Specifies the values of pipeline-overridable constants in the shader module.
    ///
    /// If an `@id` attribute was specified on the declaration,
    /// the key must be the pipeline constant ID as a decimal ASCII number; if not,
    /// the key must be the constant's identifier name.
    ///
    /// The value may represent any of WGSL's concrete scalar types.
    pub constants: Cow<'a, naga::back::PipelineConstants>,
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum ImplicitLayoutError {
}

/// Describes a compute pipeline.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ComputePipelineDescriptor<'a> {
    pub label: Label<'a>,
    /// The layout of bind groups for this pipeline.
    pub layout: Option<()>,
    /// The compiled compute stage and its entry point.
    pub stage: ProgrammableStageDescriptor<'a>,
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum CreateComputePipelineError {
}

#[derive(Debug)]
#[allow(dead_code)] // JIMB 40s
pub struct ComputePipeline<A: HalApi> {
    pub(crate) raw: Option<A::ComputePipeline>,
    pub(crate) layout: Arc<PipelineLayout<A>>,
    pub(crate) device: Arc<Device<A>>,
    pub(crate) _shader_module: Arc<ShaderModule<A>>,
    pub(crate) late_sized_buffer_groups: ArrayVec<LateSizedBufferGroup, { hal::MAX_BIND_GROUPS }>,
    pub(crate) info: ResourceInfo<ComputePipeline<A>>,
}

impl<A: HalApi> Drop for ComputePipeline<A> {
    fn drop(&mut self) { todo!() }
}

impl<A: HalApi> Resource for ComputePipeline<A> {
    const TYPE: ResourceType = "ComputePipeline";

    type Marker = crate::id::markers::ComputePipeline;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }
}

/// Describes how the vertex buffer is interpreted.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "camelCase"))]
pub struct VertexBufferLayout<'a> {
    /// The stride, in bytes, between elements of this buffer.
    pub array_stride: wgt::BufferAddress,
    /// How often this vertex buffer is "stepped" forward.
    pub step_mode: wgt::VertexStepMode,
    /// The list of attributes which comprise a single vertex.
    pub attributes: Cow<'a, [wgt::VertexAttribute]>,
}

/// Describes the vertex process in a render pipeline.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct VertexState<'a> {
    /// The compiled vertex stage and its entry point.
    pub stage: ProgrammableStageDescriptor<'a>,
    /// The format of any vertex buffers used with this pipeline.
    pub buffers: Cow<'a, [VertexBufferLayout<'a>]>,
}

/// Describes fragment processing in a render pipeline.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FragmentState<'a> {
    /// The compiled fragment stage and its entry point.
    pub stage: ProgrammableStageDescriptor<'a>,
    /// The effect of draw calls on the color aspect of the output target.
    pub targets: Cow<'a, [Option<wgt::ColorTargetState>]>,
}

/// Describes a render (graphics) pipeline.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RenderPipelineDescriptor<'a> {
    pub label: Label<'a>,
    /// The layout of bind groups for this pipeline.
    pub layout: Option<()>,
    /// The vertex processing state for this pipeline.
    pub vertex: VertexState<'a>,
    /// The properties of the pipeline at the primitive assembly and rasterization level.
    #[cfg_attr(feature = "serde", serde(default))]
    pub primitive: wgt::PrimitiveState,
    /// The effect of draw calls on the depth and stencil aspects of the output target, if any.
    #[cfg_attr(feature = "serde", serde(default))]
    pub depth_stencil: Option<wgt::DepthStencilState>,
    /// The multi-sampling properties of the pipeline.
    #[cfg_attr(feature = "serde", serde(default))]
    pub multisample: wgt::MultisampleState,
    /// The fragment processing state for this pipeline.
    pub fragment: Option<FragmentState<'a>>,
    /// If the pipeline will be used with a multiview render pass, this indicates how many array
    /// layers the attachments will have.
    pub multiview: Option<NonZeroU32>,
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum ColorStateError {
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum DepthStencilStateError {
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum CreateRenderPipelineError {
}

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub struct PipelineFlags: u32 {
        const BLEND_CONSTANT = 1 << 0;
        const STENCIL_REFERENCE = 1 << 1;
        const WRITES_DEPTH = 1 << 2;
        const WRITES_STENCIL = 1 << 3;
    }
}

/// How a render pipeline will retrieve attributes from a particular vertex buffer.
#[derive(Clone, Copy, Debug)]
pub struct VertexStep {
    /// The byte stride in the buffer between one attribute value and the next.
    pub stride: wgt::BufferAddress,

    /// The byte size required to fit the last vertex in the stream.
    pub last_stride: wgt::BufferAddress,

    /// Whether the buffer is indexed by vertex number or instance number.
    pub mode: wgt::VertexStepMode,
}

impl Default for VertexStep {
    fn default() -> Self { todo!() }
}

#[derive(Debug)]
#[allow(dead_code)] // JIMB 40s
pub struct RenderPipeline<A: HalApi> {
    pub(crate) raw: Option<A::RenderPipeline>,
    pub(crate) device: Arc<Device<A>>,
    pub(crate) layout: Arc<PipelineLayout<A>>,
    pub(crate) _shader_modules:
        ArrayVec<Arc<ShaderModule<A>>, { hal::MAX_CONCURRENT_SHADER_STAGES }>,
    pub(crate) pass_context: RenderPassContext,
    pub(crate) flags: PipelineFlags,
    pub(crate) strip_index_format: Option<wgt::IndexFormat>,
    pub(crate) vertex_steps: Vec<VertexStep>,
    pub(crate) late_sized_buffer_groups: ArrayVec<LateSizedBufferGroup, { hal::MAX_BIND_GROUPS }>,
    pub(crate) info: ResourceInfo<RenderPipeline<A>>,
}

impl<A: HalApi> Drop for RenderPipeline<A> {
    fn drop(&mut self) { todo!() }
}

impl<A: HalApi> Resource for RenderPipeline<A> {
    const TYPE: ResourceType = "RenderPipeline";

    type Marker = crate::id::markers::RenderPipeline;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }
}
