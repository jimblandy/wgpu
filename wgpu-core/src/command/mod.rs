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

/// The current state of a [`CommandBuffer`].
#[derive(Debug)]
pub(crate) enum CommandEncoderStatus {
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
    marker: std::marker::PhantomData<A>,
}

impl<A: HalApi> Drop for CommandBuffer<A> {
    fn drop(&mut self) { todo!() }
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

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum CommandEncoderError {
    #[error(transparent)]
    Device(#[from] DeviceError),
}

#[derive(Debug, Copy, Clone)]
struct StateChange<T> {
    marker: std::marker::PhantomData<T>,
}

impl<T: Copy + PartialEq> Default for StateChange<T> {
    fn default() -> Self { todo!() }
}

#[derive(Debug)]
struct BindGroupStateChange {
}

impl Default for BindGroupStateChange {
    fn default() -> Self { todo!() }
}

trait MapPassErr<T, O> {
    fn map_pass_err(self, scope: PassErrorScope) -> Result<T, O>;
}

#[derive(Clone, Copy, Debug, Error)]
pub enum PassErrorScope {
}

impl PrettyError for PassErrorScope {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}
