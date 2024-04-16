/*! Render Bundles

A render bundle is a prerecorded sequence of commands that can be replayed on a
command encoder with a single call. A single bundle can replayed any number of
times, on different encoders. Constructing a render bundle lets `wgpu` validate
and analyze its commands up front, so that replaying a bundle can be more
efficient than simply re-recording its commands each time.

Not all commands are available in bundles; for example, a render bundle may not
contain a [`RenderCommand::SetViewport`] command.

Most of `wgpu`'s backend graphics APIs have something like bundles. For example,
Vulkan calls them "secondary command buffers", and Metal calls them "indirect
command buffers". Although we plan to take advantage of these platform features
at some point in the future, for now `wgpu`'s implementation of render bundles
does not use them: at the hal level, `wgpu` render bundles just replay the
commands.

## Render Bundle Isolation

One important property of render bundles is that the draw calls in a render
bundle depend solely on the pipeline and state established within the render
bundle itself. A draw call in a bundle will never use a vertex buffer, say, that
was set in the `RenderPass` before executing the bundle. We call this property
'isolation', in that a render bundle is somewhat isolated from the passes that
use it.

Render passes are also isolated from the effects of bundles. After executing a
render bundle, a render pass's pipeline, bind groups, and vertex and index
buffers are are unset, so the bundle cannot affect later draw calls in the pass.

A render pass is not fully isolated from a bundle's effects on push constant
values. Draw calls following a bundle's execution will see whatever values the
bundle writes to push constant storage. Setting a pipeline initializes any push
constant storage it could access to zero, and this initialization may also be
visible after bundle execution.

## Render Bundle Lifecycle

To create a render bundle:

1) Create a [`RenderBundleEncoder`] by calling
   [`Global::device_create_render_bundle_encoder`][Gdcrbe].

2) Record commands in the `RenderBundleEncoder` using functions from the
   [`bundle_ffi`] module.

3) Call [`Global::render_bundle_encoder_finish`][Grbef], which analyzes and cleans up
   the command stream and returns a `RenderBundleId`.

4) Then, any number of times, call [`wgpu_render_pass_execute_bundles`][wrpeb] to
   execute the bundle as part of some render pass.

## Implementation

The most complex part of render bundles is the "finish" step, mostly implemented
in [`RenderBundleEncoder::finish`]. This consumes the commands stored in the
encoder's [`BasePass`], while validating everything, tracking the state,
dropping redundant or unnecessary commands, and presenting the results as a new
[`RenderBundle`]. It doesn't actually execute any commands.

This step also enforces the 'isolation' property mentioned above: every draw
call is checked to ensure that the resources it uses on were established since
the last time the pipeline was set. This means the bundle can be executed
verbatim without any state tracking.

### Execution

When the bundle is used in an actual render pass, `RenderBundle::execute` is
called. It goes through the commands and issues them into the native command
buffer. Thanks to isolation, it doesn't track any bind group invalidations or
index format changes.

[Gdcrbe]: crate::global::Global::device_create_render_bundle_encoder
[Grbef]: crate::global::Global::render_bundle_encoder_finish
[wrpeb]: crate::command::render_ffi::wgpu_render_pass_execute_bundles
!*/

#![allow(clippy::reversed_empty_ranges)]

#[cfg(feature = "trace")]
use crate::device::trace;
use crate::{
    binding_model::{buffer_binding_type_alignment, BindGroup, BindGroupLayout, PipelineLayout},
    command::{
        BasePass, BindGroupStateChange, ColorAttachmentError, DrawError, MapPassErr,
        PassErrorScope, RenderCommand, RenderCommandError, StateChange,
    },
    conv,
    device::{
        AttachmentData, Device, DeviceError, MissingDownlevelFlags,
        RenderPassCompatibilityCheckType, RenderPassContext, SHADER_STAGE_COUNT,
    },
    error::{ErrorFormatter, PrettyError},
    hal_api::HalApi,
    hub::Hub,
    id,
    init_tracker::{BufferInitTrackerAction, MemoryInitKind, TextureInitTrackerAction},
    pipeline::{PipelineFlags, RenderPipeline, VertexStep},
    resource::{Buffer, Resource, ResourceInfo, ResourceType},
    resource_log,
    snatch::SnatchGuard,
    track::RenderBundleScope,
    validation::check_buffer_usage,
    Label, LabelHelpers,
};
use arrayvec::ArrayVec;

use std::{borrow::Cow, mem, num::NonZeroU32, ops::Range, sync::Arc};
use thiserror::Error;

use hal::CommandEncoder as _;

use super::ArcRenderCommand;

/// https://gpuweb.github.io/gpuweb/#dom-gpurendercommandsmixin-draw
fn validate_draw<A: HalApi>(
    vertex: &[Option<VertexState<A>>],
    step: &[VertexStep],
    first_vertex: u32,
    vertex_count: u32,
    first_instance: u32,
    instance_count: u32,
) -> Result<(), DrawError> { todo!() }

// See https://gpuweb.github.io/gpuweb/#dom-gpurendercommandsmixin-drawindexed
fn validate_indexed_draw<A: HalApi>(
    vertex: &[Option<VertexState<A>>],
    step: &[VertexStep],
    index_state: &IndexState<A>,
    first_index: u32,
    index_count: u32,
    first_instance: u32,
    instance_count: u32,
) -> Result<(), DrawError> { todo!() }

/// Describes a [`RenderBundleEncoder`].
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RenderBundleEncoderDescriptor<'a> {
    /// Debug label of the render bundle encoder.
    ///
    /// This will show up in graphics debuggers for easy identification.
    pub label: Label<'a>,
    /// The formats of the color attachments that this render bundle is capable
    /// to rendering to.
    ///
    /// This must match the formats of the color attachments in the
    /// renderpass this render bundle is executed in.
    pub color_formats: Cow<'a, [Option<wgt::TextureFormat>]>,
    /// Information about the depth attachment that this render bundle is
    /// capable to rendering to.
    ///
    /// The format must match the format of the depth attachments in the
    /// renderpass this render bundle is executed in.
    pub depth_stencil: Option<wgt::RenderBundleDepthStencil>,
    /// Sample count this render bundle is capable of rendering to.
    ///
    /// This must match the pipelines and the renderpasses it is used in.
    pub sample_count: u32,
    /// If this render bundle will rendering to multiple array layers in the
    /// attachments at the same time.
    pub multiview: Option<NonZeroU32>,
}

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub struct RenderBundleEncoder {
    base: BasePass<RenderCommand>,
    parent_id: id::DeviceId,
    pub(crate) context: RenderPassContext,
    pub(crate) is_depth_read_only: bool,
    pub(crate) is_stencil_read_only: bool,

    // Resource binding dedupe state.
    #[cfg_attr(feature = "serde", serde(skip))]
    current_bind_groups: BindGroupStateChange,
    #[cfg_attr(feature = "serde", serde(skip))]
    current_pipeline: StateChange<id::RenderPipelineId>,
}

impl RenderBundleEncoder {
    pub fn new(
        desc: &RenderBundleEncoderDescriptor,
        parent_id: id::DeviceId,
        base: Option<BasePass<RenderCommand>>,
    ) -> Result<Self, CreateRenderBundleError> { todo!() }

    pub fn dummy(parent_id: id::DeviceId) -> Self { todo!() }

    #[cfg(feature = "trace")]
    pub(crate) fn to_base_pass(&self) -> BasePass<RenderCommand> { todo!() }

    pub fn parent(&self) -> id::DeviceId { todo!() }

    /// Convert this encoder's commands into a [`RenderBundle`].
    ///
    /// We want executing a [`RenderBundle`] to be quick, so we take
    /// this opportunity to clean up the [`RenderBundleEncoder`]'s
    /// command stream and gather metadata about it that will help
    /// keep [`ExecuteBundle`] simple and fast. We remove redundant
    /// commands (along with their side data), note resource usage,
    /// and accumulate buffer and texture initialization actions.
    ///
    /// [`ExecuteBundle`]: RenderCommand::ExecuteBundle
    pub(crate) fn finish<A: HalApi>(
        self,
        desc: &RenderBundleDescriptor,
        device: &Arc<Device<A>>,
        hub: &Hub<A>,
    ) -> Result<RenderBundle<A>, RenderBundleError> { todo!() }

    fn check_valid_to_use(&self, device_id: id::DeviceId) -> Result<(), RenderBundleErrorInner> { todo!() }

    pub fn set_index_buffer(
        &mut self,
        buffer_id: id::BufferId,
        index_format: wgt::IndexFormat,
        offset: wgt::BufferAddress,
        size: Option<wgt::BufferSize>,
    ) { todo!() }
}

/// Error type returned from `RenderBundleEncoder::new` if the sample count is invalid.
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum CreateRenderBundleError {
    #[error(transparent)]
    ColorAttachment(#[from] ColorAttachmentError),
    #[error("Invalid number of samples {0}")]
    InvalidSampleCount(u32),
}

/// Error type returned from `RenderBundleEncoder::new` if the sample count is invalid.
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum ExecutionError {
    #[error("Buffer {0:?} is destroyed")]
    DestroyedBuffer(id::BufferId),
    #[error("BindGroup {0:?} is invalid")]
    InvalidBindGroup(id::BindGroupId),
    #[error("Using {0} in a render bundle is not implemented")]
    Unimplemented(&'static str),
}
impl PrettyError for ExecutionError {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

pub type RenderBundleDescriptor<'a> = wgt::RenderBundleDescriptor<Label<'a>>;

//Note: here, `RenderBundle` is just wrapping a raw stream of render commands.
// The plan is to back it by an actual Vulkan secondary buffer, D3D12 Bundle,
// or Metal indirect command buffer.
#[derive(Debug)]
pub struct RenderBundle<A: HalApi> {
    // Normalized command stream. It can be executed verbatim,
    // without re-binding anything on the pipeline change.
    base: BasePass<ArcRenderCommand<A>>,
    pub(super) is_depth_read_only: bool,
    pub(super) is_stencil_read_only: bool,
    pub(crate) device: Arc<Device<A>>,
    pub(crate) used: RenderBundleScope<A>,
    pub(super) buffer_memory_init_actions: Vec<BufferInitTrackerAction<A>>,
    pub(super) texture_memory_init_actions: Vec<TextureInitTrackerAction<A>>,
    pub(super) context: RenderPassContext,
    pub(crate) info: ResourceInfo<RenderBundle<A>>,
    discard_hal_labels: bool,
}

impl<A: HalApi> Drop for RenderBundle<A> {
    fn drop(&mut self) { todo!() }
}

#[cfg(send_sync)]
unsafe impl<A: HalApi> Send for RenderBundle<A> {}
#[cfg(send_sync)]
unsafe impl<A: HalApi> Sync for RenderBundle<A> {}

impl<A: HalApi> RenderBundle<A> {
    /// Actually encode the contents into a native command buffer.
    ///
    /// This is partially duplicating the logic of `command_encoder_run_render_pass`.
    /// However the point of this function is to be lighter, since we already had
    /// a chance to go through the commands in `render_bundle_encoder_finish`.
    ///
    /// Note that the function isn't expected to fail, generally.
    /// All the validation has already been done by this point.
    /// The only failure condition is if some of the used buffers are destroyed.
    pub(super) unsafe fn execute(
        &self,
        raw: &mut A::CommandEncoder,
        snatch_guard: &SnatchGuard,
    ) -> Result<(), ExecutionError> { todo!() }
}

impl<A: HalApi> Resource for RenderBundle<A> {
    const TYPE: ResourceType = "RenderBundle";

    type Marker = crate::id::markers::RenderBundle;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }
}

/// A render bundle's current index buffer state.
///
/// [`RenderBundleEncoder::finish`] records the currently set index buffer here,
/// and calls [`State::flush_index`] before any indexed draw command to produce
/// a `SetIndexBuffer` command if one is necessary.
#[derive(Debug)]
struct IndexState<A: HalApi> {
    buffer: Arc<Buffer<A>>,
    format: wgt::IndexFormat,
    range: Range<wgt::BufferAddress>,
    is_dirty: bool,
}

impl<A: HalApi> IndexState<A> {
    /// Return the number of entries in the current index buffer.
    ///
    /// Panic if no index buffer has been set.
    fn limit(&self) -> u64 { todo!() }

    /// Generate a `SetIndexBuffer` command to prepare for an indexed draw
    /// command, if needed.
    fn flush(&mut self) -> Option<ArcRenderCommand<A>> { todo!() }
}

/// The state of a single vertex buffer slot during render bundle encoding.
///
/// [`RenderBundleEncoder::finish`] uses this to drop redundant
/// `SetVertexBuffer` commands from the final [`RenderBundle`]. It
/// records one vertex buffer slot's state changes here, and then
/// calls this type's [`flush`] method just before any draw command to
/// produce a `SetVertexBuffer` commands if one is necessary.
///
/// [`flush`]: IndexState::flush
#[derive(Debug)]
struct VertexState<A: HalApi> {
    buffer: Arc<Buffer<A>>,
    range: Range<wgt::BufferAddress>,
    is_dirty: bool,
}

impl<A: HalApi> VertexState<A> {
    fn new(buffer: Arc<Buffer<A>>, range: Range<wgt::BufferAddress>) -> Self { todo!() }

    /// Generate a `SetVertexBuffer` command for this slot, if necessary.
    ///
    /// `slot` is the index of the vertex buffer slot that `self` tracks.
    fn flush(&mut self, slot: u32) -> Option<ArcRenderCommand<A>> { todo!() }
}

/// A bind group that has been set at a particular index during render bundle encoding.
#[derive(Debug)]
struct BindState<A: HalApi> {
    /// The id of the bind group set at this index.
    bind_group: Arc<BindGroup<A>>,

    /// The layout of `group`.
    layout: Arc<BindGroupLayout<A>>,

    /// The range of dynamic offsets for this bind group, in the original
    /// command stream's `BassPass::dynamic_offsets` array.
    dynamic_offsets: Range<usize>,

    /// True if this index's contents have been changed since the last time we
    /// generated a `SetBindGroup` command.
    is_dirty: bool,
}

/// The bundle's current pipeline, and some cached information needed for validation.
struct PipelineState<A: HalApi> {
    /// The pipeline
    pipeline: Arc<RenderPipeline<A>>,

    /// How this pipeline's vertex shader traverses each vertex buffer, indexed
    /// by vertex buffer slot number.
    steps: Vec<VertexStep>,

    /// Ranges of push constants this pipeline uses, copied from the pipeline
    /// layout.
    push_constant_ranges: ArrayVec<wgt::PushConstantRange, { SHADER_STAGE_COUNT }>,

    /// The number of bind groups this pipeline uses.
    used_bind_groups: usize,
}

impl<A: HalApi> PipelineState<A> {
    fn new(pipeline: &Arc<RenderPipeline<A>>) -> Self { todo!() }

    /// Return a sequence of commands to zero the push constant ranges this
    /// pipeline uses. If no initialization is necessary, return `None`.
    fn zero_push_constants(&self) -> Option<impl Iterator<Item = ArcRenderCommand<A>>> { Some(std::iter::empty()) }
}

/// State for analyzing and cleaning up bundle command streams.
///
/// To minimize state updates, [`RenderBundleEncoder::finish`]
/// actually just applies commands like [`SetBindGroup`] and
/// [`SetIndexBuffer`] to the simulated state stored here, and then
/// calls the `flush_foo` methods before draw calls to produce the
/// update commands we actually need.
///
/// [`SetBindGroup`]: RenderCommand::SetBindGroup
/// [`SetIndexBuffer`]: RenderCommand::SetIndexBuffer
struct State<A: HalApi> {
    /// Resources used by this bundle. This will become [`RenderBundle::used`].
    trackers: RenderBundleScope<A>,

    /// The currently set pipeline, if any.
    pipeline: Option<PipelineState<A>>,

    /// The bind group set at each index, if any.
    bind: ArrayVec<Option<BindState<A>>, { hal::MAX_BIND_GROUPS }>,

    /// The state of each vertex buffer slot.
    vertex: ArrayVec<Option<VertexState<A>>, { hal::MAX_VERTEX_BUFFERS }>,

    /// The current index buffer, if one has been set. We flush this state
    /// before indexed draw commands.
    index: Option<IndexState<A>>,

    /// Dynamic offset values used by the cleaned-up command sequence.
    ///
    /// This becomes the final [`RenderBundle`]'s [`BasePass`]'s
    /// [`dynamic_offsets`] list.
    ///
    /// [`dynamic_offsets`]: BasePass::dynamic_offsets
    flat_dynamic_offsets: Vec<wgt::DynamicOffset>,
}

impl<A: HalApi> State<A> {
    /// Return the id of the current pipeline, if any.
    fn pipeline_id(&self) -> Option<id::RenderPipelineId> { todo!() }

    /// Return the current pipeline state. Return an error if none is set.
    fn pipeline(&self, scope: PassErrorScope) -> Result<&PipelineState<A>, RenderBundleError> { todo!() }

    /// Mark all non-empty bind group table entries from `index` onwards as dirty.
    fn invalidate_bind_group_from(&mut self, index: usize) { todo!() }

    fn set_bind_group(
        &mut self,
        slot: u32,
        bind_group: &Arc<BindGroup<A>>,
        layout: &Arc<BindGroupLayout<A>>,
        dynamic_offsets: Range<usize>,
    ) { todo!() }

    /// Determine which bind group slots need to be re-set after a pipeline change.
    ///
    /// Given that we are switching from the current pipeline state to `new`,
    /// whose layout is `layout`, mark all the bind group slots that we need to
    /// emit new `SetBindGroup` commands for as dirty.
    ///
    /// According to `wgpu_hal`'s rules:
    ///
    /// - If the layout of any bind group slot changes, then that slot and
    ///   all following slots must have their bind groups re-established.
    ///
    /// - Changing the push constant ranges at all requires re-establishing
    ///   all bind groups.
    fn invalidate_bind_groups(&mut self, new: &PipelineState<A>, layout: &PipelineLayout<A>) { todo!() }

    /// Set the bundle's current index buffer and its associated parameters.
    fn set_index_buffer(
        &mut self,
        buffer: Arc<Buffer<A>>,
        format: wgt::IndexFormat,
        range: Range<wgt::BufferAddress>,
    ) { todo!() }

    /// Generate a `SetIndexBuffer` command to prepare for an indexed draw
    /// command, if needed.
    fn flush_index(&mut self) -> Option<ArcRenderCommand<A>> { todo!() }

    fn flush_vertices(&mut self) -> impl Iterator<Item = ArcRenderCommand<A>> + '_ { std::iter::empty() }

    /// Generate `SetBindGroup` commands for any bind groups that need to be updated.
    fn flush_binds(
        &mut self,
        used_bind_groups: usize,
        dynamic_offsets: &[wgt::DynamicOffset],
    ) -> impl Iterator<Item = ArcRenderCommand<A>> + '_ { std::iter::empty() }
}

/// Error encountered when finishing recording a render bundle.
#[derive(Clone, Debug, Error)]
pub(super) enum RenderBundleErrorInner {
    #[error("Resource is not valid to use with this render bundle because the resource and the bundle come from different devices")]
    NotValidToUse,
    #[error(transparent)]
    Device(#[from] DeviceError),
    #[error(transparent)]
    RenderCommand(RenderCommandError),
    #[error(transparent)]
    Draw(#[from] DrawError),
    #[error(transparent)]
    MissingDownlevelFlags(#[from] MissingDownlevelFlags),
}

impl<T> From<T> for RenderBundleErrorInner
where
    T: Into<RenderCommandError>,
{
    fn from(t: T) -> Self { todo!() }
}

/// Error encountered when finishing recording a render bundle.
#[derive(Clone, Debug, Error)]
#[error("{scope}")]
pub struct RenderBundleError {
    pub scope: PassErrorScope,
    #[source]
    inner: RenderBundleErrorInner,
}

impl RenderBundleError {
    pub(crate) const INVALID_DEVICE: Self = RenderBundleError {
        scope: PassErrorScope::Bundle,
        inner: RenderBundleErrorInner::Device(DeviceError::Invalid),
    };
}
impl PrettyError for RenderBundleError {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

impl<T, E> MapPassErr<T, RenderBundleError> for Result<T, E>
where
    E: Into<RenderBundleErrorInner>,
{
    fn map_pass_err(self, scope: PassErrorScope) -> Result<T, RenderBundleError> { todo!() }
}

pub mod bundle_ffi {
    use super::{RenderBundleEncoder, RenderCommand};
    use crate::{id, RawString};
    use std::{convert::TryInto, slice};
    use wgt::{BufferAddress, BufferSize, DynamicOffset, IndexFormat};

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given pointer is
    /// valid for `offset_length` elements.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_render_bundle_set_bind_group(
        bundle: &mut RenderBundleEncoder,
        index: u32,
        bind_group_id: id::BindGroupId,
        offsets: *const DynamicOffset,
        offset_length: usize,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_bundle_set_pipeline(
        bundle: &mut RenderBundleEncoder,
        pipeline_id: id::RenderPipelineId,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_bundle_set_vertex_buffer(
        bundle: &mut RenderBundleEncoder,
        slot: u32,
        buffer_id: id::BufferId,
        offset: BufferAddress,
        size: Option<BufferSize>,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_bundle_set_index_buffer(
        encoder: &mut RenderBundleEncoder,
        buffer: id::BufferId,
        index_format: IndexFormat,
        offset: BufferAddress,
        size: Option<BufferSize>,
    ) { todo!() }

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given pointer is
    /// valid for `data` elements.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_render_bundle_set_push_constants(
        pass: &mut RenderBundleEncoder,
        stages: wgt::ShaderStages,
        offset: u32,
        size_bytes: u32,
        data: *const u8,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_bundle_draw(
        bundle: &mut RenderBundleEncoder,
        vertex_count: u32,
        instance_count: u32,
        first_vertex: u32,
        first_instance: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_bundle_draw_indexed(
        bundle: &mut RenderBundleEncoder,
        index_count: u32,
        instance_count: u32,
        first_index: u32,
        base_vertex: i32,
        first_instance: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_bundle_draw_indirect(
        bundle: &mut RenderBundleEncoder,
        buffer_id: id::BufferId,
        offset: BufferAddress,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_bundle_draw_indexed_indirect(
        bundle: &mut RenderBundleEncoder,
        buffer_id: id::BufferId,
        offset: BufferAddress,
    ) { todo!() }

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given `label`
    /// is a valid null-terminated string.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_render_bundle_push_debug_group(
        _bundle: &mut RenderBundleEncoder,
        _label: RawString,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_bundle_pop_debug_group(_bundle: &mut RenderBundleEncoder) { todo!() }

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given `label`
    /// is a valid null-terminated string.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_render_bundle_insert_debug_marker(
        _bundle: &mut RenderBundleEncoder,
        _label: RawString,
    ) { todo!() }
}
