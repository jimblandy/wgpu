use crate::device::DeviceError;
use crate::resource::Resource;
use crate::snatch::SnatchGuard;
use crate::track::TrackerIndex;
use crate::{
    binding_model::{
        BindError, BindGroup, LateMinBufferBindingSizeMismatch, PushConstantUploadError,
    },
    command::{
        bind::Binder,
        end_pipeline_statistics_query,
        memory_init::{fixup_discarded_surfaces, SurfacesInDiscardState},
        BasePass, BasePassRef, BindGroupStateChange, CommandBuffer, CommandEncoderError,
        CommandEncoderStatus, MapPassErr, PassErrorScope, QueryUseError, StateChange,
    },
    device::{MissingDownlevelFlags, MissingFeatures},
    error::{ErrorFormatter, PrettyError},
    global::Global,
    hal_api::HalApi,
    id,
    id::DeviceId,
    init_tracker::MemoryInitKind,
    pipeline,
    resource::{self},
    storage::Storage,
    track::{Tracker, UsageConflict, UsageScope},
    validation::{check_buffer_usage, MissingBufferUsageError},
    Label,
};

use hal::CommandEncoder as _;
#[cfg(feature = "serde")]
use serde::Deserialize;
#[cfg(feature = "serde")]
use serde::Serialize;

use thiserror::Error;

use std::sync::Arc;
use std::{fmt, mem, str};

#[doc(hidden)]
#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ComputeCommand {
    SetBindGroup {
        index: u32,
        num_dynamic_offsets: usize,
        bind_group_id: id::BindGroupId,
    },
    SetPipeline(id::ComputePipelineId),

    /// Set a range of push constants to values stored in [`BasePass::push_constant_data`].
    SetPushConstant {
        /// The byte offset within the push constant storage to write to. This
        /// must be a multiple of four.
        offset: u32,

        /// The number of bytes to write. This must be a multiple of four.
        size_bytes: u32,

        /// Index in [`BasePass::push_constant_data`] of the start of the data
        /// to be written.
        ///
        /// Note: this is not a byte offset like `offset`. Rather, it is the
        /// index of the first `u32` element in `push_constant_data` to read.
        values_offset: u32,
    },

    Dispatch([u32; 3]),
    DispatchIndirect {
        buffer_id: id::BufferId,
        offset: wgt::BufferAddress,
    },
    PushDebugGroup {
        color: u32,
        len: usize,
    },
    PopDebugGroup,
    InsertDebugMarker {
        color: u32,
        len: usize,
    },
    WriteTimestamp {
        query_set_id: id::QuerySetId,
        query_index: u32,
    },
    BeginPipelineStatisticsQuery {
        query_set_id: id::QuerySetId,
        query_index: u32,
    },
    EndPipelineStatisticsQuery,
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub struct ComputePass {
    base: BasePass<ComputeCommand>,
    parent_id: id::CommandEncoderId,
    timestamp_writes: Option<ComputePassTimestampWrites>,

    // Resource binding dedupe state.
    #[cfg_attr(feature = "serde", serde(skip))]
    current_bind_groups: BindGroupStateChange,
    #[cfg_attr(feature = "serde", serde(skip))]
    current_pipeline: StateChange<id::ComputePipelineId>,
}

impl ComputePass {
    pub fn new(parent_id: id::CommandEncoderId, desc: &ComputePassDescriptor) -> Self { todo!() }

    pub fn parent_id(&self) -> id::CommandEncoderId { todo!() }

    #[cfg(feature = "trace")]
    pub fn into_command(self) -> crate::device::trace::Command { todo!() }
}

impl fmt::Debug for ComputePass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}

/// Describes the writing of timestamp values in a compute pass.
#[repr(C)]
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ComputePassTimestampWrites {
    /// The query set to write the timestamps to.
    pub query_set: id::QuerySetId,
    /// The index of the query set at which a start timestamp of this pass is written, if any.
    pub beginning_of_pass_write_index: Option<u32>,
    /// The index of the query set at which an end timestamp of this pass is written, if any.
    pub end_of_pass_write_index: Option<u32>,
}

#[derive(Clone, Debug, Default)]
pub struct ComputePassDescriptor<'a> {
    pub label: Label<'a>,
    /// Defines where and when timestamp values will be written for this pass.
    pub timestamp_writes: Option<&'a ComputePassTimestampWrites>,
}

#[derive(Clone, Debug, Error, Eq, PartialEq)]
#[non_exhaustive]
pub enum DispatchError {
    #[error("Compute pipeline must be set")]
    MissingPipeline,
    #[error("Incompatible bind group at index {index} in the current compute pipeline")]
    IncompatibleBindGroup { index: u32, diff: Vec<String> },
    #[error(
        "Each current dispatch group size dimension ({current:?}) must be less or equal to {limit}"
    )]
    InvalidGroupSize { current: [u32; 3], limit: u32 },
    #[error(transparent)]
    BindingSizeTooSmall(#[from] LateMinBufferBindingSizeMismatch),
}

/// Error encountered when performing a compute pass.
#[derive(Clone, Debug, Error)]
pub enum ComputePassErrorInner {
    #[error(transparent)]
    Device(#[from] DeviceError),
    #[error(transparent)]
    Encoder(#[from] CommandEncoderError),
    #[error("Bind group at index {0:?} is invalid")]
    InvalidBindGroup(usize),
    #[error("Device {0:?} is invalid")]
    InvalidDevice(DeviceId),
    #[error("Bind group index {index} is greater than the device's requested `max_bind_group` limit {max}")]
    BindGroupIndexOutOfRange { index: u32, max: u32 },
    #[error("Compute pipeline {0:?} is invalid")]
    InvalidPipeline(id::ComputePipelineId),
    #[error("QuerySet {0:?} is invalid")]
    InvalidQuerySet(id::QuerySetId),
    #[error("Indirect buffer {0:?} is invalid or destroyed")]
    InvalidIndirectBuffer(id::BufferId),
    #[error("Indirect buffer uses bytes {offset}..{end_offset} which overruns indirect buffer of size {buffer_size}")]
    IndirectBufferOverrun {
        offset: u64,
        end_offset: u64,
        buffer_size: u64,
    },
    #[error("Buffer {0:?} is invalid or destroyed")]
    InvalidBuffer(id::BufferId),
    #[error(transparent)]
    ResourceUsageConflict(#[from] UsageConflict),
    #[error(transparent)]
    MissingBufferUsage(#[from] MissingBufferUsageError),
    #[error("Cannot pop debug group, because number of pushed debug groups is zero")]
    InvalidPopDebugGroup,
    #[error(transparent)]
    Dispatch(#[from] DispatchError),
    #[error(transparent)]
    Bind(#[from] BindError),
    #[error(transparent)]
    PushConstants(#[from] PushConstantUploadError),
    #[error(transparent)]
    QueryUse(#[from] QueryUseError),
    #[error(transparent)]
    MissingFeatures(#[from] MissingFeatures),
    #[error(transparent)]
    MissingDownlevelFlags(#[from] MissingDownlevelFlags),
}

impl PrettyError for ComputePassErrorInner {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

/// Error encountered when performing a compute pass.
#[derive(Clone, Debug, Error)]
#[error("{scope}")]
pub struct ComputePassError {
    pub scope: PassErrorScope,
    #[source]
    inner: ComputePassErrorInner,
}
impl PrettyError for ComputePassError {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

impl<T, E> MapPassErr<T, ComputePassError> for Result<T, E>
where
    E: Into<ComputePassErrorInner>,
{
    fn map_pass_err(self, scope: PassErrorScope) -> Result<T, ComputePassError> { todo!() }
}

struct State<'a, A: HalApi> {
    binder: Binder<A>,
    pipeline: Option<id::ComputePipelineId>,
    scope: UsageScope<'a, A>,
    debug_scope_depth: u32,
}

impl<'a, A: HalApi> State<'a, A> {
    fn is_ready(&self) -> Result<(), DispatchError> { todo!() }

    // `extra_buffer` is there to represent the indirect buffer that is also
    // part of the usage scope.
    fn flush_states(
        &mut self,
        raw_encoder: &mut A::CommandEncoder,
        base_trackers: &mut Tracker<A>,
        bind_group_guard: &Storage<BindGroup<A>>,
        indirect_buffer: Option<TrackerIndex>,
        snatch_guard: &SnatchGuard,
    ) -> Result<(), UsageConflict> { todo!() }
}

// Common routines between render/compute

impl Global {
    pub fn command_encoder_run_compute_pass<A: HalApi>(
        &self,
        encoder_id: id::CommandEncoderId,
        pass: &ComputePass,
    ) -> Result<(), ComputePassError> { todo!() }

    #[doc(hidden)]
    pub fn command_encoder_run_compute_pass_impl<A: HalApi>(
        &self,
        encoder_id: id::CommandEncoderId,
        base: BasePassRef<ComputeCommand>,
        timestamp_writes: Option<&ComputePassTimestampWrites>,
    ) -> Result<(), ComputePassError> { todo!() }
}

pub mod compute_ffi {
    use super::{ComputeCommand, ComputePass};
    use crate::{id, RawString};
    use std::{convert::TryInto, ffi, slice};
    use wgt::{BufferAddress, DynamicOffset};

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given pointer is
    /// valid for `offset_length` elements.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_compute_pass_set_bind_group(
        pass: &mut ComputePass,
        index: u32,
        bind_group_id: id::BindGroupId,
        offsets: *const DynamicOffset,
        offset_length: usize,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_compute_pass_set_pipeline(
        pass: &mut ComputePass,
        pipeline_id: id::ComputePipelineId,
    ) { todo!() }

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given pointer is
    /// valid for `size_bytes` bytes.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_compute_pass_set_push_constant(
        pass: &mut ComputePass,
        offset: u32,
        size_bytes: u32,
        data: *const u8,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_compute_pass_dispatch_workgroups(
        pass: &mut ComputePass,
        groups_x: u32,
        groups_y: u32,
        groups_z: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_compute_pass_dispatch_workgroups_indirect(
        pass: &mut ComputePass,
        buffer_id: id::BufferId,
        offset: BufferAddress,
    ) { todo!() }

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given `label`
    /// is a valid null-terminated string.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_compute_pass_push_debug_group(
        pass: &mut ComputePass,
        label: RawString,
        color: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_compute_pass_pop_debug_group(pass: &mut ComputePass) { todo!() }

    /// # Safety
    ///
    /// This function is unsafe as there is no guarantee that the given `label`
    /// is a valid null-terminated string.
    #[no_mangle]
    pub unsafe extern "C" fn wgpu_compute_pass_insert_debug_marker(
        pass: &mut ComputePass,
        label: RawString,
        color: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_compute_pass_write_timestamp(
        pass: &mut ComputePass,
        query_set_id: id::QuerySetId,
        query_index: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_compute_pass_begin_pipeline_statistics_query(
        pass: &mut ComputePass,
        query_set_id: id::QuerySetId,
        query_index: u32,
    ) { todo!() }

    #[no_mangle]
    pub extern "C" fn wgpu_compute_pass_end_pipeline_statistics_query(pass: &mut ComputePass) { todo!() }
}
