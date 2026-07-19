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

## Implementation note

The current implementation of hal auditing uses a single mutex to
synchronize all the audit state for a given instance, which will
probably cause contention in highly concurrent applications. 

However, our urgent use case for the auditing layer is to fuzz
wgpu-core, which will not exercise concurrent access, and a single
lock is much simpler: there is no need to assign an ordering to
per-resource locks, for example.

In either case, this decision doesn't affect the external interface of
the auditing layer, so if it becomes necessary, it should be possible
to rearchitect the auditing layer without affecting users.

[`wgpu_hal::Api`]: crate::Api

*/
#![allow(dead_code)]

mod adapter;
mod command_encoder;
mod device;
mod instance;
mod location;
mod op;
mod queue;
mod report;
mod state;
mod surface;

use state::State;

use crate::{
    DynAccelerationStructure, DynAdapter, DynBindGroup, DynBindGroupLayout, DynBuffer,
    DynCommandBuffer, DynCommandEncoder, DynComputePipeline, DynDevice, DynFence, DynInstance,
    DynPipelineCache, DynPipelineLayout, DynQuerySet, DynQueue, DynRayTracingPipeline,
    DynRenderPipeline, DynSampler, DynShaderModule, DynSurface, DynSurfaceTexture, DynTexture,
    DynTextureView,
};

use alloc::boxed::Box;
use alloc::sync::Arc;
use core::fmt;
use core::marker::PhantomData;

#[derive(Clone, Debug)]
pub struct Api;

impl crate::Api for Api {
    // This wrapper works with any backend; there is no single `Backend`
    // value that correctly describes it. This constant is required by the
    // `Api` trait but is not meaningfully used for this type.
    const VARIANT: wgt::Backend = wgt::Backend::Noop;

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
    type RayTracingPipeline = RayTracingPipeline;
    type PipelineCache = PipelineCache;
    type AccelerationStructure = AccelerationStructure;
}

/// A callback function to which all hal activity is reported.
///
/// The first callback will always be a call to [`result`], passing
/// [`Finished::NewInstance`] to report the successful creation of the
/// instance.
///
/// Since both `operation` and `result` take `&mut self`, an `Auditor`
/// implementations may assume that only one thread is invoking its
/// methods at a time. However, `wgpu_hal` objects can generally be
/// used from any thread, so if an auditor needs to pair up results
/// with their operations, it will need to track operations in
/// progress separately for each thread.
///
/// [`result`]: Self::result
/// [`Finished::NewInstance`]: op::Finished::NewInstance
pub trait Auditor: Send {
    /// An operation has been performed.
    ///
    /// An operation has been performed on the instance or some
    /// resource created from it, as described by `op`.
    ///
    /// If the `wgpu_hal` operation has an interesting result, this
    /// callback will be followed by a call to [`result`] on the same
    /// thread, reporting how things turned out.
    ///
    /// [`result`]: Self::result
    fn operation(&mut self, op: op::Op);

    /// The underlying instance has completed an operation.
    ///
    /// Successful results provide an [`op::Finished`] value that has
    /// the details.
    ///
    /// If the operation failed, this returns `Err(err)`. See
    /// [`op::Error`] for details.
    fn result(&mut self, result: Result<op::Finished, op::Error>);
}

/// Return an [`Auditor`] that logs every operation and result via the
/// `log` crate, at the given level.
pub fn report_by_log(level: log::Level) -> Box<dyn Auditor> {
    struct LogAuditor {
        level: log::Level,
    }

    impl Auditor for LogAuditor {
        fn operation(&mut self, op: op::Op) {
            log::debug!("{op:?}");
        }

        fn result(&mut self, result: Result<op::Finished, op::Error>) {
            match result {
                Ok(finished) => log::debug!("{finished:?}"),
                Err(err) => log::log!(self.level, "wgpu_hal::audit: {err}"),
            }
        }
    }

    Box::new(LogAuditor { level })
}

/// Return an [`Auditor`] that panics as soon as a `wgpu_hal` operation
/// reports an error.
pub fn report_by_panic() -> Box<dyn Auditor> {
    struct PanicAuditor;

    impl Auditor for PanicAuditor {
        fn operation(&mut self, _op: op::Op) {}

        fn result(&mut self, result: Result<op::Finished, op::Error>) {
            if let Err(err) = result {
                panic!("wgpu_hal::audit: {err}");
            }
        }
    }

    Box::new(PanicAuditor)
}

/// Return a new [`DynInstance`] that audits usage of `inner`.
///
/// Every operation on the returned instance or any resource created
/// from it is passed to `auditor`.
///
/// The `backend` value should indicate what kind of backend `inner`
/// is. This is used for diagnostics.
pub fn new_auditing_instance(
    inner: Box<dyn DynInstance>,
    backend: wgpu_types::Backend,
    auditor: Box<dyn Auditor>,
) -> Box<dyn DynInstance> {
    Box::new(Instance::new(inner, backend, auditor))
}

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Id<T: ?Sized> {
    pub num: u64,
    _marker: PhantomData<T>,
}

impl<T: ?Sized> Id<T> {
    fn new(num: u64) -> Self {
        Self {
            num,
            _marker: PhantomData,
        }
    }
}

impl<T: ?Sized> Copy for Id<T> {}
impl<T: ?Sized> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
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
pub type RayTracingPipeline = Audited<dyn DynRayTracingPipeline>;
pub type PipelineCache = Audited<dyn DynPipelineCache>;
pub type AccelerationStructure = Audited<dyn DynAccelerationStructure>;

pub struct Audited<T: ?Sized> {
    inner: Box<T>,
    id: Id<Self>,
    shared: Arc<State>,
}

// Ideally this would just be another `Audited` type, but we need the
// `texture` field.
pub struct SurfaceTexture {
    inner: Box<dyn DynSurfaceTexture>,
    id: Id<Self>,
    shared: Arc<State>,

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
    RayTracingPipeline,
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
impl DynRayTracingPipeline for RayTracingPipeline {}
impl DynPipelineCache for PipelineCache {}
impl DynPipelineLayout for PipelineLayout {}
impl DynQuerySet for QuerySet {}
impl DynRenderPipeline for RenderPipeline {}
impl DynSampler for Sampler {}
impl DynShaderModule for ShaderModule {}
impl DynSurfaceTexture for SurfaceTexture {}
impl DynTexture for Texture {}
impl DynTextureView for TextureView {}

impl<T: ?Sized> fmt::Display for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}#{}", core::any::type_name::<T>(), self.num)
    }
}

impl<T: ?Sized> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}#{}", core::any::type_name::<T>(), self.num)
    }
}

impl<T: ?Sized> fmt::Debug for Audited<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.id.fmt(f)
    }
}

impl fmt::Debug for SurfaceTexture {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.id.fmt(f)
    }
}
