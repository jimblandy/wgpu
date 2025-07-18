/*! A layer to check that a user is meeting wgpu-hal's requirements.

This module's [`Api`] type is an implementation of [`wgpu_hal::Api`] that checks
that its user is satisfying the API's safety constraints, and issues diagnostics
if it does not. This `Api` wraps some other `Api` implementation, passing
requests through to it to be actually executed. Think Vulkan's validation layer,
but for `wgpu-hal`.

This auditing layer is a work in progress: most of `wgpu_hal`'s safety
requirements are not yet checked. Contributions of new checks are
welcome.

To get started, call [`new_auditing_instance`]. This takes an ordinary
[`DynInstance`], called the "inner instance", and returns a new
instance, called the "auditing instance", that passes all calls
through to the inner instance, after first checking that `wgpu_hal`'s
safety requirements are upheld. Violations are reported to a callback
which you provide.

An auditing instance enumerates "auditing adapters", which open
"auditing devices", which create "auditing buffers", and so on. Each
auditing resource checks that `wgpu_hal`'s rules are being followed,
reports any volations, and then passes the call through to its
corresponding "inner resource".

When a violation of `wgpu_hal`'s rules is reported, if your callback
function returns, then the call is passed through to `inner` anyway,
and the program proceeds as normal. If you want violations to stop
program execution, you must provide a callback that panics.

There are a few callbacks implemented for you:

- [`report_by_panic`] returns a callback that reports the
  violation and panics.

- [`report_by_log`] returns a callback that logs violations using
  the `log` crate.

This module is named `audit` because we have a lot of other things
named "validation".

[`wgpu_hal::Api`]: crate::Api

*/
#![allow(dead_code)]

mod adapter;
mod command_encoder;
mod device;
mod instance;
mod location;
mod queue;
mod report;
mod state;
mod surface;

use state::State;

use crate::{
    DynAccelerationStructure, DynAdapter, DynBindGroup, DynBindGroupLayout, DynBuffer,
    DynCommandBuffer, DynCommandEncoder, DynComputePipeline, DynDevice, DynFence, DynInstance,
    DynPipelineCache, DynPipelineLayout, DynQuerySet, DynQueue, DynRenderPipeline, DynSampler,
    DynShaderModule, DynSurface, DynSurfaceTexture, DynTexture, DynTextureView,
};

use alloc::boxed::Box;
use alloc::string::String;
use alloc::sync::Arc;
use core::fmt;

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

/// A callback function for handling reports of violations.
pub type ReportCallback = dyn FnMut(String) + Send + Sync + 'static;

/// Return a new [`DynInstance`] that audits usage of `inner`.
///
/// Report violations of `wgpu_hal`'s safety requirements to `callback`.
///
/// The `backend` value should indicate what kind of backend `inner`
/// is. This is used for diagnostics.
pub fn new_auditing_instance(
    inner: Box<dyn DynInstance>,
    backend: wgpu_types::Backend,
    callback: Box<ReportCallback>,
) -> Box<dyn DynInstance> {
    Box::new(Instance::new(inner, backend, callback))
}

/// Build a [`ReportCallback`] that logs violations at `level`.
pub fn report_by_log(level: log::Level) -> Box<ReportCallback> {
    Box::new(move |message| log::log!(level, "{message}"))
}

/// Build a [`ReportCallback`] that prints the violation and panics.
pub fn report_by_panic() -> Box<ReportCallback> {
    Box::new(|message| panic!("wgpu_hal::audit violation:\n{message}"))
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct AuditId(pub u64);

pub struct Audited<T: ?Sized> {
    inner: Box<T>,
    id: AuditId,
    state: Arc<State>,
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

// Ideally this would just be another `Audited` type, but we need the
// `texture` field.
pub struct SurfaceTexture {
    inner: Box<dyn DynSurfaceTexture>,
    id: AuditId,
    state: Arc<State>,

    /// We need to be able to `std::borrow::Borrow` the original
    /// texture from a `SurfaceTexture`, and we can't just recreate a
    /// fresh `audit::Texture` each time, so we cache it here.
    texture: Texture,
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
