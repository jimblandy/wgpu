#![allow(unused_variables)]

#[derive(Clone, Debug)]
pub struct Api;
pub struct Context;
#[derive(Debug)]
pub struct Encoder;
#[derive(Debug)]
pub struct Resource;

impl crate::Api for Api {
    type Instance = Context;
    type Surface = Context;
    type Adapter = Context;
    type Device = Context;

    type Queue = Context;
    type CommandEncoder = Encoder;
    type CommandBuffer = Resource;

    type Buffer = Resource;
    type Texture = Resource;
    type SurfaceTexture = Resource;
    type TextureView = Resource;
    type Sampler = Resource;
    type QuerySet = Resource;
    type Fence = Resource;
    type AccelerationStructure = Resource;

    type BindGroupLayout = Resource;
    type BindGroup = Resource;
    type PipelineLayout = Resource;
    type ShaderModule = Resource;
    type RenderPipeline = Resource;
    type ComputePipeline = Resource;
}

impl crate::Instance for Context {
    type A = Api;

}

impl crate::Surface for Context {
    type A = Api;

}

impl crate::Adapter for Context {
    type A = Api;
}

impl crate::Queue for Context {
    type A = Api;
}

impl crate::Device for Context {
    type A = Api;
}

impl crate::CommandEncoder for Encoder {
    type A = Api;
}
