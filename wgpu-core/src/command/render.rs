use crate::resource::Resource;
use crate::snatch::SnatchGuard;
use crate::{
    api_log,
    binding_model::BindError,
    command::{
        self,
        bind::Binder,
        BasePass, BasePassRef, BindGroupStateChange, CommandBuffer, CommandEncoderError,
        CommandEncoderStatus, DrawError, ExecutionError, MapPassErr, PassErrorScope, QueryUseError,
        RenderCommand, RenderCommandError, StateChange,
    },
    device::{
        AttachmentData, Device, DeviceError, MissingDownlevelFlags, MissingFeatures,
        RenderPassCompatibilityCheckType, RenderPassCompatibilityError, RenderPassContext,
    },
    error::{ErrorFormatter, PrettyError},
    global::Global,
    hal_api::HalApi,
    id,
    init_tracker::{MemoryInitKind, TextureInitRange, TextureInitTrackerAction},
    pipeline::{self, PipelineFlags},
    resource::{QuerySet, Texture, TextureView, TextureViewNotRenderableReason},
    storage::Storage,
    track::{TextureSelector, Tracker, UsageConflict, UsageScope},
    validation::{
        check_buffer_usage, check_texture_usage, MissingBufferUsageError, MissingTextureUsageError,
    },
    Label,
};

use arrayvec::ArrayVec;
use hal::CommandEncoder as _;
use thiserror::Error;
use wgt::{
    BufferAddress, BufferSize, BufferUsages, Color, IndexFormat, TextureUsages,
    TextureViewDimension, VertexStepMode,
};

#[cfg(feature = "serde")]
use serde::Deserialize;
#[cfg(feature = "serde")]
use serde::Serialize;

use std::sync::Arc;
use std::{borrow::Cow, fmt, iter, marker::PhantomData, mem, num::NonZeroU32, ops::Range, str};

use super::{
    memory_init::TextureSurfaceDiscard, CommandBufferTextureMemoryActions,
    QueryResetMap,
};

/// Operation to perform to the output attachment at the start of a renderpass.
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub enum LoadOp {
    /// Clear the output attachment with the clear color. Clearing is faster than loading.
    Clear = 0,
    /// Do not clear output attachment.
    Load = 1,
}

/// Operation to perform to the output attachment at the end of a renderpass.
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub enum StoreOp {
    /// Discards the content of the render target.
    ///
    /// If you don't care about the contents of the target, this can be faster.
    Discard = 0,
    /// Store the result of the renderpass.
    Store = 1,
}

/// Describes an individual channel within a render pass, such as color, depth, or stencil.
#[repr(C)]
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PassChannel<V> {
    /// Operation to perform to the output attachment at the start of a
    /// renderpass.
    ///
    /// This must be clear if it is the first renderpass rendering to a swap
    /// chain image.
    pub load_op: LoadOp,
    /// Operation to perform to the output attachment at the end of a renderpass.
    pub store_op: StoreOp,
    /// If load_op is [`LoadOp::Clear`], the attachment will be cleared to this
    /// color.
    pub clear_value: V,
    /// If true, the relevant channel is not changed by a renderpass, and the
    /// corresponding attachment can be used inside the pass by other read-only
    /// usages.
    pub read_only: bool,
}

/// Describes a color attachment to a render pass.
#[repr(C)]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct RenderPassColorAttachment {
    /// The view to use as an attachment.
    pub view: (),
    /// The view that will receive the resolved output if multisampling is used.
    pub resolve_target: Option<()>,
    /// What operations will be performed on this color attachment.
    pub channel: PassChannel<Color>,
}

/// Describes a depth/stencil attachment to a render pass.
#[repr(C)]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct RenderPassDepthStencilAttachment {
    /// The view to use as an attachment.
    pub view: (),
    /// What operations will be performed on the depth part of the attachment.
    pub depth: PassChannel<f32>,
    /// What operations will be performed on the stencil part of the attachment.
    pub stencil: PassChannel<u32>,
}

/// Location to write a timestamp to (beginning or end of the pass).
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub enum RenderPassTimestampLocation {
    Beginning = 0,
    End = 1,
}

/// Describes the writing of timestamp values in a render pass.
#[repr(C)]
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct RenderPassTimestampWrites {
    /// The query set to write the timestamp to.
    pub query_set: (),
    /// The index of the query set at which a start timestamp of this pass is written, if any.
    pub beginning_of_pass_write_index: Option<u32>,
    /// The index of the query set at which an end timestamp of this pass is written, if any.
    pub end_of_pass_write_index: Option<u32>,
}

/// Describes the attachments of a render pass.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct RenderPassDescriptor<'a> {
    pub label: Label<'a>,
    /// The color attachments of the render pass.
    pub color_attachments: Cow<'a, [Option<RenderPassColorAttachment>]>,
    /// The depth and stencil attachment of the render pass, if any.
    pub depth_stencil_attachment: Option<&'a RenderPassDepthStencilAttachment>,
    /// Defines where and when timestamp values will be written for this pass.
    pub timestamp_writes: Option<&'a RenderPassTimestampWrites>,
    /// Defines where the occlusion query results will be stored for this pass.
    pub occlusion_query_set: Option<()>,
}

#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct RenderPass {
    base: BasePass<RenderCommand>,
    parent_id: (),
    color_targets: ArrayVec<Option<RenderPassColorAttachment>, { hal::MAX_COLOR_ATTACHMENTS }>,
    depth_stencil_target: Option<RenderPassDepthStencilAttachment>,
    timestamp_writes: Option<RenderPassTimestampWrites>,
    occlusion_query_set_id: Option<()>,
}

impl fmt::Debug for RenderPass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}

#[derive(Debug, PartialEq)]
enum OptionalState {
}

#[derive(Debug, Default)]
struct IndexState {
}

#[derive(Clone, Copy, Debug)]
struct VertexBufferState {
}

#[derive(Debug, Default)]
struct VertexState {
}

#[derive(Debug)]
struct State<A: HalApi> {
    marker: PhantomData<A>,
}

/// Describes an attachment location in words.
///
/// Can be used as "the {loc} has..." or "{loc} has..."
#[derive(Debug, Copy, Clone)]
pub enum AttachmentErrorLocation {
}

impl fmt::Display for AttachmentErrorLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum ColorAttachmentError {
}

/// Error encountered when performing a render pass.
#[derive(Clone, Debug, Error)]
pub enum RenderPassErrorInner {
}

impl PrettyError for RenderPassErrorInner {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

impl From<MissingBufferUsageError> for RenderPassErrorInner {
    fn from(error: MissingBufferUsageError) -> Self { todo!() }
}

impl From<MissingTextureUsageError> for RenderPassErrorInner {
    fn from(error: MissingTextureUsageError) -> Self { todo!() }
}

impl From<DeviceError> for RenderPassErrorInner {
    fn from(error: DeviceError) -> Self { todo!() }
}

/// Error encountered when performing a render pass.
#[derive(Clone, Debug, Error)]
#[error("{scope}")]
pub struct RenderPassError {
    pub scope: PassErrorScope,
    #[source]
    inner: RenderPassErrorInner,
}
impl PrettyError for RenderPassError {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

impl<T, E> MapPassErr<T, RenderPassError> for Result<T, E>
where
    E: Into<RenderPassErrorInner>,
{
    fn map_pass_err(self, scope: PassErrorScope) -> Result<T, RenderPassError> { todo!() }
}

// Common routines between render/compute

pub mod render_ffi {
    use super::{
        super::{Rect, RenderCommand},
        RenderPass,
    };
    use crate::{id, RawString};
    use std::{convert::TryInto, ffi, num::NonZeroU32, slice};
    use wgt::{BufferAddress, BufferSize, Color, DynamicOffset, IndexFormat};

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given pointer is
    /// valid for `offset_length` elements.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_render_pass_set_bind_group(
        pass: &mut RenderPass,
        index: u32,
        bind_group_id: (),
        offsets: *const DynamicOffset,
        offset_length: usize,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_set_pipeline(
        pass: &mut RenderPass,
        pipeline_id: (),
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_set_vertex_buffer(
        pass: &mut RenderPass,
        slot: u32,
        buffer_id: (),
        offset: BufferAddress,
        size: Option<BufferSize>,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_set_index_buffer(
        pass: &mut RenderPass,
        buffer: (),
        index_format: IndexFormat,
        offset: BufferAddress,
        size: Option<BufferSize>,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_set_blend_constant(pass: &mut RenderPass, color: &Color) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_set_stencil_reference(pass: &mut RenderPass, value: u32) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_set_viewport(
        pass: &mut RenderPass,
        x: f32,
        y: f32,
        w: f32,
        h: f32,
        depth_min: f32,
        depth_max: f32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_set_scissor_rect(
        pass: &mut RenderPass,
        x: u32,
        y: u32,
        w: u32,
        h: u32,
    ) { todo!() }

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given pointer is
    /// valid for `size_bytes` bytes.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_render_pass_set_push_constants(
        pass: &mut RenderPass,
        stages: wgt::ShaderStages,
        offset: u32,
        size_bytes: u32,
        data: *const u8,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_draw(
        pass: &mut RenderPass,
        vertex_count: u32,
        instance_count: u32,
        first_vertex: u32,
        first_instance: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_draw_indexed(
        pass: &mut RenderPass,
        index_count: u32,
        instance_count: u32,
        first_index: u32,
        base_vertex: i32,
        first_instance: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_draw_indirect(
        pass: &mut RenderPass,
        buffer_id: (),
        offset: BufferAddress,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_draw_indexed_indirect(
        pass: &mut RenderPass,
        buffer_id: (),
        offset: BufferAddress,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_multi_draw_indirect(
        pass: &mut RenderPass,
        buffer_id: (),
        offset: BufferAddress,
        count: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_multi_draw_indexed_indirect(
        pass: &mut RenderPass,
        buffer_id: (),
        offset: BufferAddress,
        count: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_multi_draw_indirect_count(
        pass: &mut RenderPass,
        buffer_id: (),
        offset: BufferAddress,
        count_buffer_id: (),
        count_buffer_offset: BufferAddress,
        max_count: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_multi_draw_indexed_indirect_count(
        pass: &mut RenderPass,
        buffer_id: (),
        offset: BufferAddress,
        count_buffer_id: (),
        count_buffer_offset: BufferAddress,
        max_count: u32,
    ) { todo!() }

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given `label`
    /// is a valid null-terminated string.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_render_pass_push_debug_group(
        pass: &mut RenderPass,
        label: RawString,
        color: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_pop_debug_group(pass: &mut RenderPass) { todo!() }

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given `label`
    /// is a valid null-terminated string.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_render_pass_insert_debug_marker(
        pass: &mut RenderPass,
        label: RawString,
        color: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_write_timestamp(
        pass: &mut RenderPass,
        query_set_id: (),
        query_index: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_begin_occlusion_query(
        pass: &mut RenderPass,
        query_index: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_end_occlusion_query(pass: &mut RenderPass) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_begin_pipeline_statistics_query(
        pass: &mut RenderPass,
        query_set_id: (),
        query_index: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_end_pipeline_statistics_query(pass: &mut RenderPass) { todo!() }

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given pointer is
    /// valid for `render_bundle_ids_length` elements.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_render_pass_execute_bundles(
        pass: &mut RenderPass,
        render_bundle_ids: *const (),
        render_bundle_ids_length: usize,
    ) { todo!() }
}
