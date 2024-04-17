use crate::id;
use std::ops::Range;
#[cfg(feature = "trace")]
use std::{borrow::Cow, io::Write as _};

//TODO: consider a readable Id that doesn't include the backend

type FileName = String;

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Action<'a> {
    Init {
        desc: crate::device::DeviceDescriptor<'a>,
        backend: wgt::Backend,
    },
    ConfigureSurface(
        id::SurfaceId,
        wgt::SurfaceConfiguration<Vec<wgt::TextureFormat>>,
    ),
    CreateBuffer(id::BufferId, crate::resource::BufferDescriptor<'a>),
    FreeBuffer(id::BufferId),
    DestroyBuffer(id::BufferId),
    CreateTexture(id::TextureId, crate::resource::TextureDescriptor<'a>),
    FreeTexture(id::TextureId),
    DestroyTexture(id::TextureId),
    CreateTextureView {
        id: id::TextureViewId,
        parent_id: id::TextureId,
        desc: crate::resource::TextureViewDescriptor<'a>,
    },
    DestroyTextureView(id::TextureViewId),
    CreateSampler(id::SamplerId, crate::resource::SamplerDescriptor<'a>),
    DestroySampler(id::SamplerId),
    GetSurfaceTexture {
        id: id::TextureId,
        parent_id: id::SurfaceId,
    },
    Present(id::SurfaceId),
    DiscardSurfaceTexture(id::SurfaceId),
    CreateBindGroupLayout(
        id::BindGroupLayoutId,
        crate::binding_model::BindGroupLayoutDescriptor<'a>,
    ),
    DestroyBindGroupLayout(id::BindGroupLayoutId),
    CreatePipelineLayout(
        id::PipelineLayoutId,
        crate::binding_model::PipelineLayoutDescriptor<'a>,
    ),
    DestroyPipelineLayout(id::PipelineLayoutId),
    CreateBindGroup(
        id::BindGroupId,
        crate::binding_model::BindGroupDescriptor<'a>,
    ),
    DestroyBindGroup(id::BindGroupId),
    CreateShaderModule {
        id: id::ShaderModuleId,
        desc: crate::pipeline::ShaderModuleDescriptor<'a>,
        data: FileName,
    },
    DestroyShaderModule(id::ShaderModuleId),
    CreateComputePipeline {
        id: id::ComputePipelineId,
        desc: crate::pipeline::ComputePipelineDescriptor<'a>,
        #[cfg_attr(feature = "replay", serde(default))]
        implicit_context: Option<super::ImplicitPipelineContext>,
    },
    DestroyComputePipeline(id::ComputePipelineId),
    CreateRenderPipeline {
        id: id::RenderPipelineId,
        desc: crate::pipeline::RenderPipelineDescriptor<'a>,
        #[cfg_attr(feature = "replay", serde(default))]
        implicit_context: Option<super::ImplicitPipelineContext>,
    },
    DestroyRenderPipeline(id::RenderPipelineId),
    CreateRenderBundle {
        id: id::RenderBundleId,
        desc: crate::command::RenderBundleEncoderDescriptor<'a>,
        base: crate::command::BasePass<crate::command::RenderCommand>,
    },
    DestroyRenderBundle(id::RenderBundleId),
    CreateQuerySet {
        id: id::QuerySetId,
        desc: crate::resource::QuerySetDescriptor<'a>,
    },
    DestroyQuerySet(id::QuerySetId),
    WriteBuffer {
        id: id::BufferId,
        data: FileName,
        range: Range<wgt::BufferAddress>,
        queued: bool,
    },
    WriteTexture {
        to: crate::command::ImageCopyTexture,
        data: FileName,
        layout: wgt::ImageDataLayout,
        size: wgt::Extent3d,
    },
    Submit(crate::SubmissionIndex, Vec<Command>),
}

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Command {
    CopyBufferToBuffer {
        src: id::BufferId,
        src_offset: wgt::BufferAddress,
        dst: id::BufferId,
        dst_offset: wgt::BufferAddress,
        size: wgt::BufferAddress,
    },
    CopyBufferToTexture {
        src: crate::command::ImageCopyBuffer,
        dst: crate::command::ImageCopyTexture,
        size: wgt::Extent3d,
    },
    CopyTextureToBuffer {
        src: crate::command::ImageCopyTexture,
        dst: crate::command::ImageCopyBuffer,
        size: wgt::Extent3d,
    },
    CopyTextureToTexture {
        src: crate::command::ImageCopyTexture,
        dst: crate::command::ImageCopyTexture,
        size: wgt::Extent3d,
    },
    ClearBuffer {
        dst: id::BufferId,
        offset: wgt::BufferAddress,
        size: Option<wgt::BufferAddress>,
    },
    ClearTexture {
        dst: id::TextureId,
        subresource_range: wgt::ImageSubresourceRange,
    },
    WriteTimestamp {
        query_set_id: id::QuerySetId,
        query_index: u32,
    },
    ResolveQuerySet {
        query_set_id: id::QuerySetId,
        start_query: u32,
        query_count: u32,
        destination: id::BufferId,
        destination_offset: wgt::BufferAddress,
    },
    PushDebugGroup(String),
    PopDebugGroup,
    InsertDebugMarker(String),
    RunComputePass {
        base: crate::command::BasePass<crate::command::ComputeCommand>,
        timestamp_writes: Option<crate::command::ComputePassTimestampWrites>,
    },
    RunRenderPass {
        base: crate::command::BasePass<crate::command::RenderCommand>,
        target_colors: Vec<Option<crate::command::RenderPassColorAttachment>>,
        target_depth_stencil: Option<crate::command::RenderPassDepthStencilAttachment>,
        timestamp_writes: Option<crate::command::RenderPassTimestampWrites>,
        occlusion_query_set_id: Option<id::QuerySetId>,
    },
}

#[cfg(feature = "trace")]
#[derive(Debug)]
pub struct Trace {
}

