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
        (),
        wgt::SurfaceConfiguration<Vec<wgt::TextureFormat>>,
    ),
    CreateBuffer((), crate::resource::BufferDescriptor<'a>),
    FreeBuffer(()),
    DestroyBuffer(()),
    CreateTexture((), crate::resource::TextureDescriptor<'a>),
    FreeTexture(()),
    DestroyTexture(()),
    CreateTextureView {
        id: (),
        parent_id: (),
        desc: crate::resource::TextureViewDescriptor<'a>,
    },
    DestroyTextureView(()),
    CreateSampler((), crate::resource::SamplerDescriptor<'a>),
    DestroySampler(()),
    GetSurfaceTexture {
        id: (),
        parent_id: (),
    },
    Present(()),
    DiscardSurfaceTexture(()),
    CreateBindGroupLayout(
        (),
        crate::binding_model::BindGroupLayoutDescriptor<'a>,
    ),
    DestroyBindGroupLayout(()),
    CreatePipelineLayout(
        (),
        crate::binding_model::PipelineLayoutDescriptor<'a>,
    ),
    DestroyPipelineLayout(()),
    CreateBindGroup(
        (),
        crate::binding_model::BindGroupDescriptor<'a>,
    ),
    DestroyBindGroup(()),
    CreateShaderModule {
        id: (),
        desc: crate::pipeline::ShaderModuleDescriptor<'a>,
        data: FileName,
    },
    DestroyShaderModule(()),
    CreateComputePipeline {
        id: (),
        desc: crate::pipeline::ComputePipelineDescriptor<'a>,
        #[cfg_attr(feature = "replay", serde(default))]
        implicit_context: Option<super::ImplicitPipelineContext>,
    },
    DestroyComputePipeline(()),
    CreateRenderPipeline {
        id: (),
        desc: crate::pipeline::RenderPipelineDescriptor<'a>,
        #[cfg_attr(feature = "replay", serde(default))]
        implicit_context: Option<super::ImplicitPipelineContext>,
    },
    DestroyRenderPipeline(()),
    CreateRenderBundle {
        id: (),
        desc: crate::command::RenderBundleEncoderDescriptor<'a>,
        base: crate::command::BasePass<crate::command::RenderCommand>,
    },
    DestroyRenderBundle(()),
    CreateQuerySet {
        id: (),
        desc: crate::resource::QuerySetDescriptor<'a>,
    },
    DestroyQuerySet(()),
    WriteBuffer {
        id: (),
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
        src: (),
        src_offset: wgt::BufferAddress,
        dst: (),
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
        dst: (),
        offset: wgt::BufferAddress,
        size: Option<wgt::BufferAddress>,
    },
    ClearTexture {
        dst: (),
        subresource_range: wgt::ImageSubresourceRange,
    },
    WriteTimestamp {
        query_set_id: (),
        query_index: u32,
    },
    ResolveQuerySet {
        query_set_id: (),
        start_query: u32,
        query_count: u32,
        destination: (),
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
        occlusion_query_set_id: Option<()>,
    },
}

#[cfg(feature = "trace")]
#[derive(Debug)]
pub struct Trace {
}

