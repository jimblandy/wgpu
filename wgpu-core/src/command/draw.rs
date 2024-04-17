/*! Draw structures - shared between render passes and bundles.
!*/

use crate::{
    binding_model::{BindGroup, LateMinBufferBindingSizeMismatch, PushConstantUploadError},
    error::ErrorFormatter,
    hal_api::HalApi,
    id,
    pipeline::RenderPipeline,
    resource::{Buffer, QuerySet},
    track::UsageConflict,
    validation::{MissingBufferUsageError, MissingTextureUsageError},
};
use wgt::{BufferAddress, BufferSize, Color, VertexStepMode};

use std::{num::NonZeroU32, sync::Arc};
use thiserror::Error;

use super::RenderBundle;

/// Error validating a draw call.
#[derive(Clone, Debug, Error, Eq, PartialEq)]
#[non_exhaustive]
pub enum DrawError {
}

/// Error encountered when encoding a render command.
/// This is the shared error set between render bundles and passes.
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum RenderCommandError {
}
impl crate::error::PrettyError for RenderCommandError {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

#[derive(Clone, Copy, Debug, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Rect<T> {
    pub x: T,
    pub y: T,
    pub w: T,
    pub h: T,
}

#[doc(hidden)]
#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum RenderCommand {
    SetBindGroup {
        index: u32,
        num_dynamic_offsets: usize,
        bind_group_id: (),
    },
    SetPipeline(()),
    SetIndexBuffer {
        buffer_id: (),
        index_format: wgt::IndexFormat,
        offset: BufferAddress,
        size: Option<BufferSize>,
    },
    SetVertexBuffer {
        slot: u32,
        buffer_id: (),
        offset: BufferAddress,
        size: Option<BufferSize>,
    },
    SetBlendConstant(Color),
    SetStencilReference(u32),
    SetViewport {
        rect: Rect<f32>,
        //TODO: use half-float to reduce the size?
        depth_min: f32,
        depth_max: f32,
    },
    SetScissor(Rect<u32>),

    /// Set a range of push constants to values stored in [`BasePass::push_constant_data`].
    ///
    /// See [`wgpu::RenderPass::set_push_constants`] for a detailed explanation
    /// of the restrictions these commands must satisfy.
    SetPushConstant {
        /// Which stages we are setting push constant values for.
        stages: wgt::ShaderStages,

        /// The byte offset within the push constant storage to write to.  This
        /// must be a multiple of four.
        offset: u32,

        /// The number of bytes to write. This must be a multiple of four.
        size_bytes: u32,

        /// Index in [`BasePass::push_constant_data`] of the start of the data
        /// to be written.
        ///
        /// Note: this is not a byte offset like `offset`. Rather, it is the
        /// index of the first `u32` element in `push_constant_data` to read.
        ///
        /// `None` means zeros should be written to the destination range, and
        /// there is no corresponding data in `push_constant_data`. This is used
        /// by render bundles, which explicitly clear out any state that
        /// post-bundle code might see.
        values_offset: Option<u32>,
    },
    Draw {
        vertex_count: u32,
        instance_count: u32,
        first_vertex: u32,
        first_instance: u32,
    },
    DrawIndexed {
        index_count: u32,
        instance_count: u32,
        first_index: u32,
        base_vertex: i32,
        first_instance: u32,
    },
    MultiDrawIndirect {
        buffer_id: (),
        offset: BufferAddress,
        /// Count of `None` represents a non-multi call.
        count: Option<NonZeroU32>,
        indexed: bool,
    },
    MultiDrawIndirectCount {
        buffer_id: (),
        offset: BufferAddress,
        count_buffer_id: (),
        count_buffer_offset: BufferAddress,
        max_count: u32,
        indexed: bool,
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
        query_set_id: (),
        query_index: u32,
    },
    BeginOcclusionQuery {
        query_index: u32,
    },
    EndOcclusionQuery,
    BeginPipelineStatisticsQuery {
        query_set_id: (),
        query_index: u32,
    },
    EndPipelineStatisticsQuery,
    ExecuteBundle(()),
}

/// Equivalent to `RenderCommand` with the Ids resolved into resource Arcs.
#[doc(hidden)]
#[derive(Clone, Debug)]
pub enum ArcRenderCommand {
}
