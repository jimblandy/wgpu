use crate::resource::Resource;
use crate::snatch::SnatchGuard;
use crate::{
    api_log,
    binding_model::BindError,
    command::{
        self,
        bind::Binder,
        end_occlusion_query, end_pipeline_statistics_query,
        memory_init::{fixup_discarded_surfaces, SurfacesInDiscardState},
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

impl<V> PassChannel<V> {
    fn hal_ops(&self) -> hal::AttachmentOps { todo!() }
}

/// Describes a color attachment to a render pass.
#[repr(C)]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct RenderPassColorAttachment {
    /// The view to use as an attachment.
    pub view: id::TextureViewId,
    /// The view that will receive the resolved output if multisampling is used.
    pub resolve_target: Option<id::TextureViewId>,
    /// What operations will be performed on this color attachment.
    pub channel: PassChannel<Color>,
}

/// Describes a depth/stencil attachment to a render pass.
#[repr(C)]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct RenderPassDepthStencilAttachment {
    /// The view to use as an attachment.
    pub view: id::TextureViewId,
    /// What operations will be performed on the depth part of the attachment.
    pub depth: PassChannel<f32>,
    /// What operations will be performed on the stencil part of the attachment.
    pub stencil: PassChannel<u32>,
}

impl RenderPassDepthStencilAttachment {
    /// Validate the given aspects' read-only flags against their load
    /// and store ops.
    ///
    /// When an aspect is read-only, its load and store ops must be
    /// `LoadOp::Load` and `StoreOp::Store`.
    ///
    /// On success, return a pair `(depth, stencil)` indicating
    /// whether the depth and stencil passes are read-only.
    fn depth_stencil_read_only(
        &self,
        aspects: hal::FormatAspects,
    ) -> Result<(bool, bool), RenderPassErrorInner> { todo!() }
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
    pub query_set: id::QuerySetId,
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
    pub occlusion_query_set: Option<id::QuerySetId>,
}

#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct RenderPass {
    base: BasePass<RenderCommand>,
    parent_id: id::CommandEncoderId,
    color_targets: ArrayVec<Option<RenderPassColorAttachment>, { hal::MAX_COLOR_ATTACHMENTS }>,
    depth_stencil_target: Option<RenderPassDepthStencilAttachment>,
    timestamp_writes: Option<RenderPassTimestampWrites>,
    occlusion_query_set_id: Option<id::QuerySetId>,

    // Resource binding dedupe state.
    #[cfg_attr(feature = "serde", serde(skip))]
    current_bind_groups: BindGroupStateChange,
    #[cfg_attr(feature = "serde", serde(skip))]
    current_pipeline: StateChange<id::RenderPipelineId>,
}

impl RenderPass {
    pub fn new(parent_id: id::CommandEncoderId, desc: &RenderPassDescriptor) -> Self { todo!() }

    pub fn parent_id(&self) -> id::CommandEncoderId { todo!() }

    #[cfg(feature = "trace")]
    pub fn into_command(self) -> crate::device::trace::Command { todo!() }

    pub fn set_index_buffer(
        &mut self,
        buffer_id: id::BufferId,
        index_format: IndexFormat,
        offset: BufferAddress,
        size: Option<BufferSize>,
    ) { todo!() }
}

impl fmt::Debug for RenderPass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}

#[derive(Debug, PartialEq)]
enum OptionalState {
    Unused,
    Required,
    Set,
}

impl OptionalState {
    fn require(&mut self, require: bool) { todo!() }
}

#[derive(Debug, Default)]
struct IndexState {
    bound_buffer_view: Option<(id::BufferId, Range<BufferAddress>)>,
    format: Option<IndexFormat>,
    pipeline_format: Option<IndexFormat>,
    limit: u64,
}

impl IndexState {
    fn update_limit(&mut self) { todo!() }

    fn reset(&mut self) { todo!() }
}

#[derive(Clone, Copy, Debug)]
struct VertexBufferState {
    total_size: BufferAddress,
    step: pipeline::VertexStep,
    bound: bool,
}

impl VertexBufferState {
    const EMPTY: Self = Self {
        total_size: 0,
        step: pipeline::VertexStep {
            stride: 0,
            last_stride: 0,
            mode: VertexStepMode::Vertex,
        },
        bound: false,
    };
}

#[derive(Debug, Default)]
struct VertexState {
    inputs: ArrayVec<VertexBufferState, { hal::MAX_VERTEX_BUFFERS }>,
    /// Length of the shortest vertex rate vertex buffer
    vertex_limit: u64,
    /// Buffer slot which the shortest vertex rate vertex buffer is bound to
    vertex_limit_slot: u32,
    /// Length of the shortest instance rate vertex buffer
    instance_limit: u64,
    /// Buffer slot which the shortest instance rate vertex buffer is bound to
    instance_limit_slot: u32,
    /// Total amount of buffers required by the pipeline.
    buffers_required: u32,
}

impl VertexState {
    fn update_limits(&mut self) { todo!() }

    fn reset(&mut self) { todo!() }
}

#[derive(Debug)]
struct State<A: HalApi> {
    pipeline_flags: PipelineFlags,
    binder: Binder<A>,
    blend_constant: OptionalState,
    stencil_reference: u32,
    pipeline: Option<id::RenderPipelineId>,
    index: IndexState,
    vertex: VertexState,
    debug_scope_depth: u32,
}

impl<A: HalApi> State<A> {
    fn is_ready(&self, indexed: bool) -> Result<(), DrawError> { todo!() }

    /// Reset the `RenderBundle`-related states.
    fn reset_bundle(&mut self) { todo!() }
}

/// Describes an attachment location in words.
///
/// Can be used as "the {loc} has..." or "{loc} has..."
#[derive(Debug, Copy, Clone)]
pub enum AttachmentErrorLocation {
    Color { index: usize, resolve: bool },
    Depth,
}

impl fmt::Display for AttachmentErrorLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum ColorAttachmentError {
    #[error("Attachment format {0:?} is not a color format")]
    InvalidFormat(wgt::TextureFormat),
    #[error("The number of color attachments {given} exceeds the limit {limit}")]
    TooMany { given: usize, limit: usize },
    #[error("The total number of bytes per sample in color attachments {total} exceeds the limit {limit}")]
    TooManyBytesPerSample { total: u32, limit: u32 },
}

/// Error encountered when performing a render pass.
#[derive(Clone, Debug, Error)]
pub enum RenderPassErrorInner {
    #[error(transparent)]
    Device(DeviceError),
    #[error(transparent)]
    ColorAttachment(#[from] ColorAttachmentError),
    #[error(transparent)]
    Encoder(#[from] CommandEncoderError),
    #[error("Attachment texture view {0:?} is invalid")]
    InvalidAttachment(id::TextureViewId),
    #[error("Attachment texture view {0:?} is invalid")]
    InvalidResolveTarget(id::TextureViewId),
    #[error("The format of the depth-stencil attachment ({0:?}) is not a depth-stencil format")]
    InvalidDepthStencilAttachmentFormat(wgt::TextureFormat),
    #[error("The format of the {location} ({format:?}) is not resolvable")]
    UnsupportedResolveTargetFormat {
        location: AttachmentErrorLocation,
        format: wgt::TextureFormat,
    },
    #[error("No color attachments or depth attachments were provided, at least one attachment of any kind must be provided")]
    MissingAttachments,
    #[error("The {location} is not renderable:")]
    TextureViewIsNotRenderable {
        location: AttachmentErrorLocation,
        #[source]
        reason: TextureViewNotRenderableReason,
    },
    #[error("Attachments have differing sizes: the {expected_location} has extent {expected_extent:?} but is followed by the {actual_location} which has {actual_extent:?}")]
    AttachmentsDimensionMismatch {
        expected_location: AttachmentErrorLocation,
        expected_extent: wgt::Extent3d,
        actual_location: AttachmentErrorLocation,
        actual_extent: wgt::Extent3d,
    },
    #[error("Attachments have differing sample counts: the {expected_location} has count {expected_samples:?} but is followed by the {actual_location} which has count {actual_samples:?}")]
    AttachmentSampleCountMismatch {
        expected_location: AttachmentErrorLocation,
        expected_samples: u32,
        actual_location: AttachmentErrorLocation,
        actual_samples: u32,
    },
    #[error("The resolve source, {location}, must be multi-sampled (has {src} samples) while the resolve destination must not be multisampled (has {dst} samples)")]
    InvalidResolveSampleCounts {
        location: AttachmentErrorLocation,
        src: u32,
        dst: u32,
    },
    #[error(
        "Resource source, {location}, format ({src:?}) must match the resolve destination format ({dst:?})"
    )]
    MismatchedResolveTextureFormat {
        location: AttachmentErrorLocation,
        src: wgt::TextureFormat,
        dst: wgt::TextureFormat,
    },
    #[error("Surface texture is dropped before the render pass is finished")]
    SurfaceTextureDropped,
    #[error("Not enough memory left for render pass")]
    OutOfMemory,
    #[error("The bind group at index {0:?} is invalid")]
    InvalidBindGroup(usize),
    #[error("Unable to clear non-present/read-only depth")]
    InvalidDepthOps,
    #[error("Unable to clear non-present/read-only stencil")]
    InvalidStencilOps,
    #[error("Setting `values_offset` to be `None` is only for internal use in render bundles")]
    InvalidValuesOffset,
    #[error(transparent)]
    MissingFeatures(#[from] MissingFeatures),
    #[error(transparent)]
    MissingDownlevelFlags(#[from] MissingDownlevelFlags),
    #[error("Indirect draw uses bytes {offset}..{end_offset} {} which overruns indirect buffer of size {buffer_size}",
        count.map_or_else(String::new, |v| format!("(using count {v})")))]
    IndirectBufferOverrun {
        count: Option<NonZeroU32>,
        offset: u64,
        end_offset: u64,
        buffer_size: u64,
    },
    #[error("Indirect draw uses bytes {begin_count_offset}..{end_count_offset} which overruns indirect buffer of size {count_buffer_size}")]
    IndirectCountBufferOverrun {
        begin_count_offset: u64,
        end_count_offset: u64,
        count_buffer_size: u64,
    },
    #[error("Cannot pop debug group, because number of pushed debug groups is zero")]
    InvalidPopDebugGroup,
    #[error(transparent)]
    ResourceUsageConflict(#[from] UsageConflict),
    #[error("Render bundle has incompatible targets, {0}")]
    IncompatibleBundleTargets(#[from] RenderPassCompatibilityError),
    #[error(
        "Render bundle has incompatible read-only flags: \
             bundle has flags depth = {bundle_depth} and stencil = {bundle_stencil}, \
             while the pass has flags depth = {pass_depth} and stencil = {pass_stencil}. \
             Read-only renderpasses are only compatible with read-only bundles for that aspect."
    )]
    IncompatibleBundleReadOnlyDepthStencil {
        pass_depth: bool,
        pass_stencil: bool,
        bundle_depth: bool,
        bundle_stencil: bool,
    },
    #[error(transparent)]
    RenderCommand(#[from] RenderCommandError),
    #[error(transparent)]
    Draw(#[from] DrawError),
    #[error(transparent)]
    Bind(#[from] BindError),
    #[error(transparent)]
    QueryUse(#[from] QueryUseError),
    #[error("Multiview layer count must match")]
    MultiViewMismatch,
    #[error(
        "Multiview pass texture views with more than one array layer must have D2Array dimension"
    )]
    MultiViewDimensionMismatch,
    #[error("QuerySet {0:?} is invalid")]
    InvalidQuerySet(id::QuerySetId),
    #[error("missing occlusion query set")]
    MissingOcclusionQuerySet,
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

struct RenderAttachment<'a, A: HalApi> {
    texture: Arc<Texture<A>>,
    selector: &'a TextureSelector,
    usage: hal::TextureUses,
}

impl<A: HalApi> TextureView<A> {
    fn to_render_attachment(&self, usage: hal::TextureUses) -> RenderAttachment<A> { todo!() }
}

const MAX_TOTAL_ATTACHMENTS: usize = hal::MAX_COLOR_ATTACHMENTS + hal::MAX_COLOR_ATTACHMENTS + 1;
type AttachmentDataVec<T> = ArrayVec<T, MAX_TOTAL_ATTACHMENTS>;

struct RenderPassInfo<'a, 'd, A: HalApi> {
    context: RenderPassContext,
    usage_scope: UsageScope<'d, A>,
    /// All render attachments, including depth/stencil
    render_attachments: AttachmentDataVec<RenderAttachment<'a, A>>,
    is_depth_read_only: bool,
    is_stencil_read_only: bool,
    extent: wgt::Extent3d,
    _phantom: PhantomData<A>,

    pending_discard_init_fixups: SurfacesInDiscardState<A>,
    divergent_discarded_depth_stencil_aspect: Option<(wgt::TextureAspect, &'a TextureView<A>)>,
    multiview: Option<NonZeroU32>,
}

impl<'a, 'd, A: HalApi> RenderPassInfo<'a, 'd, A> {
    fn add_pass_texture_init_actions<V>(
        channel: &PassChannel<V>,
        texture_memory_actions: &mut CommandBufferTextureMemoryActions<A>,
        view: &TextureView<A>,
        pending_discard_init_fixups: &mut SurfacesInDiscardState<A>,
    ) { todo!() }

    fn start(
        device: &'d Device<A>,
        label: Option<&str>,
        color_attachments: &[Option<RenderPassColorAttachment>],
        depth_stencil_attachment: Option<&RenderPassDepthStencilAttachment>,
        timestamp_writes: Option<&RenderPassTimestampWrites>,
        occlusion_query_set: Option<id::QuerySetId>,
        trackers: &mut Tracker<A>,
        texture_memory_actions: &mut CommandBufferTextureMemoryActions<A>,
        pending_query_resets: &mut QueryResetMap<A>,
        view_guard: &'a Storage<TextureView<A>>,
        query_set_guard: &'a Storage<QuerySet<A>>,
        snatch_guard: &SnatchGuard<'a>,
    ) -> Result<Self, RenderPassErrorInner> { todo!() }

    fn finish(
        mut self,
        raw: &mut A::CommandEncoder,
        snatch_guard: &SnatchGuard,
    ) -> Result<(UsageScope<'d, A>, SurfacesInDiscardState<A>), RenderPassErrorInner> { todo!() }
}

// Common routines between render/compute

impl Global {
    pub fn command_encoder_run_render_pass<A: HalApi>(
        &self,
        encoder_id: id::CommandEncoderId,
        pass: &RenderPass,
    ) -> Result<(), RenderPassError> { todo!() }

    #[doc(hidden)]
    pub fn command_encoder_run_render_pass_impl<A: HalApi>(
        &self,
        encoder_id: id::CommandEncoderId,
        base: BasePassRef<RenderCommand>,
        color_attachments: &[Option<RenderPassColorAttachment>],
        depth_stencil_attachment: Option<&RenderPassDepthStencilAttachment>,
        timestamp_writes: Option<&RenderPassTimestampWrites>,
        occlusion_query_set_id: Option<id::QuerySetId>,
    ) -> Result<(), RenderPassError> { todo!() }
}

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
        bind_group_id: id::BindGroupId,
        offsets: *const DynamicOffset,
        offset_length: usize,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_set_pipeline(
        pass: &mut RenderPass,
        pipeline_id: id::RenderPipelineId,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_set_vertex_buffer(
        pass: &mut RenderPass,
        slot: u32,
        buffer_id: id::BufferId,
        offset: BufferAddress,
        size: Option<BufferSize>,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_set_index_buffer(
        pass: &mut RenderPass,
        buffer: id::BufferId,
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
        buffer_id: id::BufferId,
        offset: BufferAddress,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_draw_indexed_indirect(
        pass: &mut RenderPass,
        buffer_id: id::BufferId,
        offset: BufferAddress,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_multi_draw_indirect(
        pass: &mut RenderPass,
        buffer_id: id::BufferId,
        offset: BufferAddress,
        count: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_multi_draw_indexed_indirect(
        pass: &mut RenderPass,
        buffer_id: id::BufferId,
        offset: BufferAddress,
        count: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_multi_draw_indirect_count(
        pass: &mut RenderPass,
        buffer_id: id::BufferId,
        offset: BufferAddress,
        count_buffer_id: id::BufferId,
        count_buffer_offset: BufferAddress,
        max_count: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_render_pass_multi_draw_indexed_indirect_count(
        pass: &mut RenderPass,
        buffer_id: id::BufferId,
        offset: BufferAddress,
        count_buffer_id: id::BufferId,
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
        query_set_id: id::QuerySetId,
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
        query_set_id: id::QuerySetId,
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
        render_bundle_ids: *const id::RenderBundleId,
        render_bundle_ids_length: usize,
    ) { todo!() }
}
