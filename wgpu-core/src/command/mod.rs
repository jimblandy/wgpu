mod allocator;
mod bind;
mod bundle;
mod clear;
mod compute;
mod draw;
mod memory_init;
mod query;
mod render;
mod transfer;

use std::slice;
use std::sync::Arc;

pub(crate) use self::clear::clear_texture;
pub use self::{
    bundle::*, clear::ClearError, compute::*, draw::*, query::*, render::*, transfer::*,
};
pub(crate) use allocator::CommandAllocator;

use self::memory_init::CommandBufferTextureMemoryActions;

use crate::device::{Device, DeviceError};
use crate::error::{ErrorFormatter, PrettyError};
use crate::hub::Hub;
use crate::id::CommandBufferId;
use crate::snatch::SnatchGuard;

use crate::init_tracker::BufferInitTrackerAction;
use crate::resource::{Resource, ResourceInfo, ResourceType};
use crate::track::{Tracker, UsageScope};
use crate::{api_log, global::Global, hal_api::HalApi, id, resource_log, Label};

use hal::CommandEncoder as _;
use parking_lot::Mutex;
use thiserror::Error;

#[cfg(feature = "trace")]
use crate::device::trace::Command as TraceCommand;

const PUSH_CONSTANT_CLEAR_ARRAY: &[u32] = &[0_u32; 64];

/// The current state of a [`CommandBuffer`].
#[derive(Debug)]
pub(crate) enum CommandEncoderStatus {
    /// Ready to record commands. An encoder's initial state.
    ///
    /// Command building methods like [`command_encoder_clear_buffer`] and
    /// [`command_encoder_run_compute_pass`] require the encoder to be in this
    /// state.
    ///
    /// [`command_encoder_clear_buffer`]: Global::command_encoder_clear_buffer
    /// [`command_encoder_run_compute_pass`]: Global::command_encoder_run_compute_pass
    Recording,

    /// Command recording is complete, and the buffer is ready for submission.
    ///
    /// [`Global::command_encoder_finish`] transitions a
    /// `CommandBuffer` from the `Recording` state into this state.
    ///
    /// [`Global::queue_submit`] drops command buffers unless they are
    /// in this state.
    Finished,

    /// An error occurred while recording a compute or render pass.
    ///
    /// When a `CommandEncoder` is left in this state, we have also
    /// returned an error result from the function that encountered
    /// the problem. Future attempts to use the encoder (that is,
    /// calls to [`CommandBuffer::get_encoder`]) will also return
    /// errors.
    ///
    /// Calling [`Global::command_encoder_finish`] in this state
    /// discards the command buffer under construction.
    Error,
}

/// A raw [`CommandEncoder`][rce], and the raw [`CommandBuffer`][rcb]s built from it.
///
/// Each wgpu-core [`CommandBuffer`] owns an instance of this type, which is
/// where the commands are actually stored.
///
/// This holds a `Vec` of raw [`CommandBuffer`][rcb]s, not just one. We are not
/// always able to record commands in the order in which they must ultimately be
/// submitted to the queue, but raw command buffers don't permit inserting new
/// commands into the middle of a recorded stream. However, hal queue submission
/// accepts a series of command buffers at once, so we can simply break the
/// stream up into multiple buffers, and then reorder the buffers. See
/// [`CommandEncoder::close_and_swap`] for a specific example of this.
///
/// Note that a [`CommandEncoderId`] actually refers to a [`CommandBuffer`].
/// Methods that take a command encoder id actually look up the command buffer,
/// and then use its encoder.
///
/// [rce]: wgpu_hal::Api::CommandEncoder
/// [rcb]: wgpu_hal::Api::CommandBuffer
pub(crate) struct CommandEncoder<A: HalApi> {
    /// The underlying `wgpu_hal` [`CommandEncoder`].
    ///
    /// Successfully executed command buffers' encoders are saved in a
    /// [`wgpu_hal::device::CommandAllocator`] for recycling.
    ///
    /// [`CommandEncoder`]: wgpu_hal::Api::CommandEncoder
    raw: A::CommandEncoder,

    /// All the raw command buffers for our owning [`CommandBuffer`], in
    /// submission order.
    ///
    /// These command buffers were all constructed with `raw`. The
    /// [`wgpu_hal::CommandEncoder`] trait forbids these from outliving `raw`,
    /// and requires that we provide all of these when we call
    /// [`raw.reset_all()`][CE::ra], so the encoder and its buffers travel
    /// together.
    ///
    /// [CE::ra]: wgpu_hal::CommandEncoder::reset_all
    list: Vec<A::CommandBuffer>,

    /// True if `raw` is in the "recording" state.
    ///
    /// See the documentation for [`wgpu_hal::CommandEncoder`] for
    /// details on the states `raw` can be in.
    is_open: bool,

    label: Option<String>,
}

//TODO: handle errors better
impl<A: HalApi> CommandEncoder<A> {
    /// Finish the current command buffer, if any, and place it
    /// at the second-to-last position in our list.
    ///
    /// If we have opened this command encoder, finish its current
    /// command buffer, and insert it just before the last element in
    /// [`self.list`][l]. If this command buffer is closed, do nothing.
    ///
    /// On return, the underlying hal encoder is closed.
    ///
    /// What is this for?
    ///
    /// The `wgpu_hal` contract requires that each render or compute pass's
    /// commands be preceded by calls to [`transition_buffers`] and
    /// [`transition_textures`], to put the resources the pass operates on in
    /// the appropriate state. Unfortunately, we don't know which transitions
    /// are needed until we're done recording the pass itself. Rather than
    /// iterating over the pass twice, we note the necessary transitions as we
    /// record its commands, finish the raw command buffer for the actual pass,
    /// record a new raw command buffer for the transitions, and jam that buffer
    /// in just before the pass's. This is the function that jams in the
    /// transitions' command buffer.
    ///
    /// [l]: CommandEncoder::list
    fn close_and_swap(&mut self) -> Result<(), DeviceError> { todo!() }

    /// Finish the current command buffer, if any, and add it to the
    /// end of [`self.list`][l].
    ///
    /// If we have opened this command encoder, finish its current
    /// command buffer, and push it onto the end of [`self.list`][l].
    /// If this command buffer is closed, do nothing.
    ///
    /// On return, the underlying hal encoder is closed.
    ///
    /// [l]: CommandEncoder::list
    fn close(&mut self) -> Result<(), DeviceError> { todo!() }

    /// Discard the command buffer under construction, if any.
    ///
    /// The underlying hal encoder is closed, if it was recording.
    pub(crate) fn discard(&mut self) { todo!() }

    /// Begin recording a new command buffer, if we haven't already.
    ///
    /// The underlying hal encoder is put in the "recording" state.
    pub(crate) fn open(&mut self) -> Result<&mut A::CommandEncoder, DeviceError> { todo!() }

    /// Begin recording a new command buffer for a render pass, with
    /// its own label.
    ///
    /// The underlying hal encoder is put in the "recording" state.
    fn open_pass(&mut self, label: Option<&str>) -> Result<(), DeviceError> { todo!() }
}

pub(crate) struct BakedCommands<A: HalApi> {
    pub(crate) encoder: A::CommandEncoder,
    pub(crate) list: Vec<A::CommandBuffer>,
    pub(crate) trackers: Tracker<A>,
    buffer_memory_init_actions: Vec<BufferInitTrackerAction<A>>,
    texture_memory_actions: CommandBufferTextureMemoryActions<A>,
}

pub(crate) struct DestroyedBufferError(pub id::BufferId);
pub(crate) struct DestroyedTextureError(pub id::TextureId);

/// The mutable state of a [`CommandBuffer`].
pub struct CommandBufferMutable<A: HalApi> {
    /// The [`wgpu_hal::Api::CommandBuffer`]s we've built so far, and the encoder
    /// they belong to.
    pub(crate) encoder: CommandEncoder<A>,

    /// The current state of this command buffer's encoder.
    status: CommandEncoderStatus,

    /// All the resources that the commands recorded so far have referred to.
    pub(crate) trackers: Tracker<A>,

    /// The regions of buffers and textures these commands will read and write.
    ///
    /// This is used to determine which portions of which
    /// buffers/textures we actually need to initialize. If we're
    /// definitely going to write to something before we read from it,
    /// we don't need to clear its contents.
    buffer_memory_init_actions: Vec<BufferInitTrackerAction<A>>,
    texture_memory_actions: CommandBufferTextureMemoryActions<A>,

    pub(crate) pending_query_resets: QueryResetMap<A>,
    #[cfg(feature = "trace")]
    pub(crate) commands: Option<Vec<TraceCommand>>,
}

impl<A: HalApi> CommandBufferMutable<A> {
    pub(crate) fn open_encoder_and_tracker(
        &mut self,
    ) -> Result<(&mut A::CommandEncoder, &mut Tracker<A>), DeviceError> { todo!() }
}

/// A buffer of commands to be submitted to the GPU for execution.
///
/// Whereas the WebGPU API uses two separate types for command buffers and
/// encoders, this type is a fusion of the two:
///
/// - During command recording, this holds a [`CommandEncoder`] accepting this
///   buffer's commands. In this state, the [`CommandBuffer`] type behaves like
///   a WebGPU `GPUCommandEncoder`.
///
/// - Once command recording is finished by calling
///   [`Global::command_encoder_finish`], no further recording is allowed. The
///   internal [`CommandEncoder`] is retained solely as a storage pool for the
///   raw command buffers. In this state, the value behaves like a WebGPU
///   `GPUCommandBuffer`.
///
/// - Once a command buffer is submitted to the queue, it is removed from the id
///   registry, and its contents are taken to construct a [`BakedCommands`],
///   whose contents eventually become the property of the submission queue.
pub struct CommandBuffer<A: HalApi> {
    pub(crate) device: Arc<Device<A>>,
    limits: wgt::Limits,
    support_clear_texture: bool,
    pub(crate) info: ResourceInfo<CommandBuffer<A>>,

    /// The mutable state of this command buffer.
    ///
    /// This `Option` is populated when the command buffer is first created.
    /// When this is submitted, dropped, or destroyed, its contents are
    /// extracted into a [`BakedCommands`] by
    /// [`CommandBuffer::extract_baked_commands`].
    pub(crate) data: Mutex<Option<CommandBufferMutable<A>>>,
}

impl<A: HalApi> Drop for CommandBuffer<A> {
    fn drop(&mut self) { todo!() }
}

impl<A: HalApi> CommandBuffer<A> {
    pub(crate) fn new(
        encoder: A::CommandEncoder,
        device: &Arc<Device<A>>,
        #[cfg(feature = "trace")] enable_tracing: bool,
        label: Option<String>,
    ) -> Self { todo!() }

    pub(crate) fn insert_barriers_from_tracker(
        raw: &mut A::CommandEncoder,
        base: &mut Tracker<A>,
        head: &Tracker<A>,
        snatch_guard: &SnatchGuard,
    ) { todo!() }

    pub(crate) fn insert_barriers_from_scope(
        raw: &mut A::CommandEncoder,
        base: &mut Tracker<A>,
        head: &UsageScope<A>,
        snatch_guard: &SnatchGuard,
    ) { todo!() }

    pub(crate) fn drain_barriers(
        raw: &mut A::CommandEncoder,
        base: &mut Tracker<A>,
        snatch_guard: &SnatchGuard,
    ) { todo!() }
}

impl<A: HalApi> CommandBuffer<A> {
    /// Return the [`CommandBuffer`] for `id`, for recording new commands.
    ///
    /// In `wgpu_core`, the [`CommandBuffer`] type serves both as encoder and
    /// buffer, which is why this function takes an [`id::CommandEncoderId`] but
    /// returns a [`CommandBuffer`]. The returned command buffer must be in the
    /// "recording" state. Otherwise, an error is returned.
    fn get_encoder(
        hub: &Hub<A>,
        id: id::CommandEncoderId,
    ) -> Result<Arc<Self>, CommandEncoderError> { todo!() }

    pub fn is_finished(&self) -> bool { todo!() }

    pub(crate) fn extract_baked_commands(&mut self) -> BakedCommands<A> { todo!() }

    pub(crate) fn from_arc_into_baked(self: Arc<Self>) -> BakedCommands<A> { todo!() }
}

impl<A: HalApi> Resource for CommandBuffer<A> {
    const TYPE: ResourceType = "CommandBuffer";

    type Marker = crate::id::markers::CommandBuffer;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }

    fn label(&self) -> String { todo!() }
}

#[derive(Copy, Clone, Debug)]
pub struct BasePassRef<'a, C> {
    pub label: Option<&'a str>,
    pub commands: &'a [C],
    pub dynamic_offsets: &'a [wgt::DynamicOffset],
    pub string_data: &'a [u8],
    pub push_constant_data: &'a [u32],
}

/// A stream of commands for a render pass or compute pass.
///
/// This also contains side tables referred to by certain commands,
/// like dynamic offsets for [`SetBindGroup`] or string data for
/// [`InsertDebugMarker`].
///
/// Render passes use `BasePass<RenderCommand>`, whereas compute
/// passes use `BasePass<ComputeCommand>`.
///
/// [`SetBindGroup`]: RenderCommand::SetBindGroup
/// [`InsertDebugMarker`]: RenderCommand::InsertDebugMarker
#[doc(hidden)]
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BasePass<C> {
    pub label: Option<String>,

    /// The stream of commands.
    pub commands: Vec<C>,

    /// Dynamic offsets consumed by [`SetBindGroup`] commands in `commands`.
    ///
    /// Each successive `SetBindGroup` consumes the next
    /// [`num_dynamic_offsets`] values from this list.
    pub dynamic_offsets: Vec<wgt::DynamicOffset>,

    /// Strings used by debug instructions.
    ///
    /// Each successive [`PushDebugGroup`] or [`InsertDebugMarker`]
    /// instruction consumes the next `len` bytes from this vector.
    pub string_data: Vec<u8>,

    /// Data used by `SetPushConstant` instructions.
    ///
    /// See the documentation for [`RenderCommand::SetPushConstant`]
    /// and [`ComputeCommand::SetPushConstant`] for details.
    pub push_constant_data: Vec<u32>,
}

impl<C: Clone> BasePass<C> {
    fn new(label: &Label) -> Self { todo!() }

    #[cfg(feature = "trace")]
    fn from_ref(base: BasePassRef<C>) -> Self { todo!() }

    pub fn as_ref(&self) -> BasePassRef<C> { todo!() }
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum CommandEncoderError {
    #[error("Command encoder is invalid")]
    Invalid,
    #[error("Command encoder must be active")]
    NotRecording,
    #[error(transparent)]
    Device(#[from] DeviceError),
}

impl Global {
    pub fn command_encoder_finish<A: HalApi>(
        &self,
        encoder_id: id::CommandEncoderId,
        _desc: &wgt::CommandBufferDescriptor<Label>,
    ) -> (CommandBufferId, Option<CommandEncoderError>) { todo!() }

    pub fn command_encoder_push_debug_group<A: HalApi>(
        &self,
        encoder_id: id::CommandEncoderId,
        label: &str,
    ) -> Result<(), CommandEncoderError> { todo!() }

    pub fn command_encoder_insert_debug_marker<A: HalApi>(
        &self,
        encoder_id: id::CommandEncoderId,
        label: &str,
    ) -> Result<(), CommandEncoderError> { todo!() }

    pub fn command_encoder_pop_debug_group<A: HalApi>(
        &self,
        encoder_id: id::CommandEncoderId,
    ) -> Result<(), CommandEncoderError> { todo!() }
}

fn push_constant_clear<PushFn>(offset: u32, size_bytes: u32, mut push_fn: PushFn)
where
    PushFn: FnMut(u32, &[u32]),
{ todo!() }

#[derive(Debug, Copy, Clone)]
struct StateChange<T> {
    last_state: Option<T>,
}

impl<T: Copy + PartialEq> StateChange<T> {
    fn new() -> Self { todo!() }
    fn set_and_check_redundant(&mut self, new_state: T) -> bool { todo!() }
    fn reset(&mut self) { todo!() }
}

impl<T: Copy + PartialEq> Default for StateChange<T> {
    fn default() -> Self { todo!() }
}

#[derive(Debug)]
struct BindGroupStateChange {
    last_states: [StateChange<id::BindGroupId>; hal::MAX_BIND_GROUPS],
}

impl BindGroupStateChange {
    fn new() -> Self { todo!() }

    unsafe fn set_and_check_redundant(
        &mut self,
        bind_group_id: id::BindGroupId,
        index: u32,
        dynamic_offsets: &mut Vec<u32>,
        offsets: *const wgt::DynamicOffset,
        offset_length: usize,
    ) -> bool { todo!() }
    fn reset(&mut self) { todo!() }
}

impl Default for BindGroupStateChange {
    fn default() -> Self { todo!() }
}

trait MapPassErr<T, O> {
    fn map_pass_err(self, scope: PassErrorScope) -> Result<T, O>;
}

#[derive(Clone, Copy, Debug, Error)]
pub enum PassErrorScope {
    #[error("In a bundle parameter")]
    Bundle,
    #[error("In a pass parameter")]
    Pass(id::CommandEncoderId),
    #[error("In a set_bind_group command")]
    SetBindGroup(id::BindGroupId),
    #[error("In a set_pipeline command")]
    SetPipelineRender(id::RenderPipelineId),
    #[error("In a set_pipeline command")]
    SetPipelineCompute(id::ComputePipelineId),
    #[error("In a set_push_constant command")]
    SetPushConstant,
    #[error("In a set_vertex_buffer command")]
    SetVertexBuffer(id::BufferId),
    #[error("In a set_index_buffer command")]
    SetIndexBuffer(id::BufferId),
    #[error("In a set_viewport command")]
    SetViewport,
    #[error("In a set_scissor_rect command")]
    SetScissorRect,
    #[error("In a draw command, indexed:{indexed} indirect:{indirect}")]
    Draw {
        indexed: bool,
        indirect: bool,
        pipeline: Option<id::RenderPipelineId>,
    },
    #[error("While resetting queries after the renderpass was ran")]
    QueryReset,
    #[error("In a write_timestamp command")]
    WriteTimestamp,
    #[error("In a begin_occlusion_query command")]
    BeginOcclusionQuery,
    #[error("In a end_occlusion_query command")]
    EndOcclusionQuery,
    #[error("In a begin_pipeline_statistics_query command")]
    BeginPipelineStatisticsQuery,
    #[error("In a end_pipeline_statistics_query command")]
    EndPipelineStatisticsQuery,
    #[error("In a execute_bundle command")]
    ExecuteBundle,
    #[error("In a dispatch command, indirect:{indirect}")]
    Dispatch {
        indirect: bool,
        pipeline: Option<id::ComputePipelineId>,
    },
    #[error("In a pop_debug_group command")]
    PopDebugGroup,
}

impl PrettyError for PassErrorScope {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}
