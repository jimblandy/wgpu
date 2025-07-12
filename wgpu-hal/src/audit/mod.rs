/*! A layer to check that a user is meeting wgpu-hal's requirements.

This module's [`Api`] type is an implementation of [`wgpu_hal::Api`] that checks
that its user is satisfying the API's safety constraints, and issues diagnostics
if it does not. This `Api` wraps some other `Api` implementation, passing
requests through to it to be actually executed. Think Vulkan's validation layer,
but for `wgpu-hal`.

This module is named `audit` because we have a lot of other things
named "validation".

[`wgpu_hal::Api`]: crate::Api

*/
#![allow(dead_code)]

mod adapter;
mod command_encoder;
mod device;
mod instance;
mod queue;
mod state;
mod surface;

use core::fmt;

use state::State;

use crate::{
    DynAccelerationStructure,
    DynAdapter,
    DynBindGroup,
    DynBindGroupLayout,
    DynBuffer,
    DynCommandBuffer,
    DynCommandEncoder,
    DynComputePipeline,
    DynDevice,
    DynFence,
    DynInstance,
    DynPipelineCache,
    DynPipelineLayout,
    DynQuerySet,
    DynQueue,
    DynRenderPipeline,
    DynSampler,
    DynShaderModule,
    DynSurface,
    DynSurfaceTexture,
    DynTexture,
    DynTextureView,
};

use alloc::boxed::Box;
use alloc::sync::Arc;

#[derive(Clone, Debug)]
pub struct Api;

impl crate::Api for Api {
    type Instance = Instance;
    type Surface = Surface;
    type Adapter = Adapter;
    type Device = Device;
    type Queue = Queue;
    type CommandEncoder = CommandEncoder;
    type CommandBuffer = CommandBuffer;
    type Buffer = Buffer;
    type Texture = Texture;
    type SurfaceTexture = SurfaceTexture;
    type TextureView = TextureView;
    type Sampler = Sampler;
    type QuerySet = QuerySet;
    type Fence = Fence;
    type BindGroupLayout = BindGroupLayout;
    type BindGroup = BindGroup;
    type PipelineLayout = PipelineLayout;
    type ShaderModule = ShaderModule;
    type RenderPipeline = RenderPipeline;
    type ComputePipeline = ComputePipeline;
    type PipelineCache = PipelineCache;
    type AccelerationStructure = AccelerationStructure;
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct AuditId(pub u64);

pub struct Audited<T: ?Sized> {
    inner: Box<T>,
    id: AuditId,
    state: Arc<State>
}

pub type Instance = Audited<dyn DynInstance>;
pub type Surface = Audited<dyn DynSurface>;
pub type Adapter = Audited<dyn DynAdapter>;
pub type Device = Audited<dyn DynDevice>;
pub type Queue = Audited<dyn DynQueue>;
pub type CommandEncoder = Audited<dyn DynCommandEncoder>;
pub type CommandBuffer = Audited<dyn DynCommandBuffer>;
pub type Buffer = Audited<dyn DynBuffer>;
pub type Texture = Audited<dyn DynTexture>;
pub type TextureView = Audited<dyn DynTextureView>;
pub type Sampler = Audited<dyn DynSampler>;
pub type QuerySet = Audited<dyn DynQuerySet>;
pub type Fence = Audited<dyn DynFence>;
pub type BindGroupLayout = Audited<dyn DynBindGroupLayout>;
pub type BindGroup = Audited<dyn DynBindGroup>;
pub type PipelineLayout = Audited<dyn DynPipelineLayout>;
pub type ShaderModule = Audited<dyn DynShaderModule>;
pub type RenderPipeline = Audited<dyn DynRenderPipeline>;
pub type ComputePipeline = Audited<dyn DynComputePipeline>;
pub type PipelineCache = Audited<dyn DynPipelineCache>;
pub type AccelerationStructure = Audited<dyn DynAccelerationStructure>;

pub struct SurfaceTexture {
    inner: Box<dyn DynSurfaceTexture>,
    id: AuditId,
    state: Arc<State>,
    texture: Arc<Texture>,
}

crate::impl_dyn_resource!(
    Instance,
    Surface,
    Adapter,
    Device,
    Queue,
    CommandEncoder,
    CommandBuffer,
    Buffer,
    Texture,
    SurfaceTexture,
    TextureView,
    Sampler,
    QuerySet,
    Fence,
    BindGroupLayout,
    BindGroup,
    PipelineLayout,
    ShaderModule,
    RenderPipeline,
    ComputePipeline,
    PipelineCache,
    AccelerationStructure
);

impl DynAccelerationStructure for AccelerationStructure {}
impl DynBindGroup for BindGroup {}
impl DynBindGroupLayout for BindGroupLayout {}
impl DynBuffer for Buffer {}
impl DynCommandBuffer for CommandBuffer {}
impl DynComputePipeline for ComputePipeline {}
impl DynFence for Fence {}
impl DynPipelineCache for PipelineCache {}
impl DynPipelineLayout for PipelineLayout {}
impl DynQuerySet for QuerySet {}
impl DynRenderPipeline for RenderPipeline {}
impl DynSampler for Sampler {}
impl DynShaderModule for ShaderModule {}
impl DynSurfaceTexture for SurfaceTexture {}
impl DynTexture for Texture {}
impl DynTextureView for TextureView {}

impl fmt::Display for AuditId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl fmt::Debug for AuditId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl<T: ?Sized> fmt::Debug for Audited<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}#{}", self.state.id_type_name(self.id), self.id)
    }
}

impl fmt::Debug for SurfaceTexture {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}#{}", self.state.id_type_name(self.id), self.id)
    }
}
