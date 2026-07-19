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
reports any violations, and then passes the call through to its
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
mod queue;
mod report;
mod state;
mod surface;

use state::State;

use crate::{
    DynAccelerationStructure, DynAdapter, DynBindGroup, DynBindGroupLayout, DynBuffer,
    DynCommandBuffer, DynCommandEncoder, DynComputePipeline, DynFence, DynInstance,
    DynPipelineCache, DynPipelineLayout, DynQuerySet, DynQueue, DynRayTracingPipeline,
    DynRenderPipeline, DynResource, DynSampler, DynShaderModule, DynSurface, DynSurfaceTexture,
    DynTexture, DynTextureView,
};

use alloc::boxed::Box;
use alloc::collections::BTreeSet;
use alloc::sync::Arc;
use core::fmt;
use core::marker::PhantomData;
use parking_lot::Mutex;

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

/// A callback to which `wgpu_hal` safety violations are reported.
///
/// See the [module documentation](self) for the callbacks provided for
/// you: [`report_by_panic`] and [`report_by_log`].
pub trait Auditor: Send {
    /// `wgpu_hal` was used in a way that violates its documented safety
    /// requirements, as described by `violation`.
    ///
    /// If this method returns, the call that triggered the violation is
    /// passed through to the real backend anyway, and the program
    /// proceeds as normal. If you want violations to stop program
    /// execution, panic here instead of returning.
    fn violation(&mut self, violation: report::Violation);
}

/// Return an [`Auditor`] that logs violations via the `log` crate, at
/// the given level.
pub fn report_by_log(level: log::Level) -> Box<dyn Auditor> {
    struct LogAuditor {
        level: log::Level,
    }

    impl Auditor for LogAuditor {
        fn violation(&mut self, violation: report::Violation) {
            log::log!(self.level, "{violation}");
        }
    }

    Box::new(LogAuditor { level })
}

/// Return an [`Auditor`] that panics as soon as a violation is reported.
pub fn report_by_panic() -> Box<dyn Auditor> {
    struct PanicAuditor;

    impl Auditor for PanicAuditor {
        fn violation(&mut self, violation: report::Violation) {
            panic!("wgpu_hal::audit: {violation}");
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

// These are all written by hand, rather than derived, because deriving
// them would add a spurious `T: Trait` bound: equality, ordering, and
// hashing for an `Id<T>` only ever depend on its `num`, never on `T`
// itself, which `T` (e.g. `Audited<dyn DynBuffer>`) usually doesn't
// implement anyway.
impl<T: ?Sized> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.num == other.num
    }
}
impl<T: ?Sized> Eq for Id<T> {}
impl<T: ?Sized> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<T: ?Sized> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.num.cmp(&other.num)
    }
}
impl<T: ?Sized> core::hash::Hash for Id<T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.num.hash(state);
    }
}


pub type Instance = Audited<dyn DynInstance>;
pub type Surface = Audited<dyn DynSurface>;
pub type Adapter = Audited<dyn DynAdapter>;

// `Device` is defined in `device.rs`, not as an `Audited` alias here —
// see the note on `Audited` for why.
pub use device::Device;

pub type Queue = Audited<dyn DynQueue>;
pub type CommandEncoder = Audited<dyn DynCommandEncoder>;
pub type CommandBuffer = Audited<dyn DynCommandBuffer>;

/// A `Buffer` remembers which `Device` created it, so that any method
/// it's passed to can check it's being used with the right one. See
/// [`OwnedByDevice`].
pub type Buffer = Audited<dyn DynBuffer, OwnedByDevice>;

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

/// An audited `wgpu_hal` resource of some `dyn DynX` type `T`.
///
/// `M` is metadata specific to this *kind* of resource, recording
/// whatever relationships to other resources are worth checking on
/// every use. `Buffer`'s `M` is [`OwnedByDevice`], recording the single
/// `Device` that created it. Resources with nothing to check yet just
/// use `M = ()`.
///
/// `Device` is *not* one of these: it needs a `Drop` impl (see
/// [`DeviceResources`]), and every `destroy_*` method on every other
/// resource kind needs to move its `inner` out of `self` by value —
/// which Rust forbids for any type with a `Drop` impl. Since `Drop`
/// can't be specialized to just one instantiation of a generic type
/// either, `Audited<T, M>` as a whole has to stay `Drop`-free, and
/// `Device` has to be its own struct, defined in `device.rs`.
pub struct Audited<T: ?Sized, M = ()> {
    inner: Box<T>,
    id: Id<Self>,
    shared: Arc<State>,
    metadata: M,
}

impl<T: ?Sized, M> Audited<T, M> {
    /// Wrap a freshly created inner resource, allocating a new id for it.
    fn wrap_with(inner: Box<T>, shared: Arc<State>, metadata: M) -> Self {
        let id = shared.new_id();
        Self {
            inner,
            id,
            shared,
            metadata,
        }
    }

    /// Borrow the inner resource as its erased dynamic type.
    ///
    /// This never fails: `inner` is already stored as the appropriate
    /// `dyn DynX` type, so no downcasting is needed.
    fn as_dyn(&self) -> &T {
        &self.inner
    }

    /// This resource's id, with its specific type erased.
    ///
    /// Useful for embedding in a [`report::Violation`], which shouldn't
    /// need a type parameter for every resource kind it can mention.
    fn erased_id(&self) -> Id<dyn DynResource> {
        Id::new(self.id.num)
    }
}

impl<T: ?Sized> Audited<T> {
    /// Wrap a freshly created inner resource that has no metadata to
    /// track, allocating a new id for it.
    ///
    /// This is the standard way to construct an [`Audited`] resource
    /// that transparently passes calls through to `inner`.
    fn wrap(inner: Box<T>, shared: Arc<State>) -> Self {
        Self::wrap_with(inner, shared, ())
    }
}

impl<T: ?Sized> Audited<T, OwnedByDevice> {
    /// The device that created this resource.
    fn device(&self) -> Id<Device> {
        self.metadata.device
    }
}

/// Metadata for a resource created by, and owned by, a single
/// [`Device`].
///
/// Most device-created resources (buffers, textures, pipelines, ...)
/// will eventually want this.
#[derive(Debug)]
pub struct OwnedByDevice {
    device: Id<Device>,
}

impl OwnedByDevice {
    fn new(device: Id<Device>) -> Self {
        Self { device }
    }
}

/// The resources a [`Device`] has created that have not
/// yet been destroyed.
///
/// `wgpu_hal` requires that a `Device` not be dropped while any
/// resource it created still exists; `Device`'s `Drop` impl in
/// `device.rs` checks this. Nothing else needs to consult this
/// directly: `Device::create_buffer` and friends register here, and
/// their `destroy_*` counterparts unregister.
#[derive(Debug, Default)]
pub struct DeviceResources {
    live: Mutex<BTreeSet<Id<dyn DynResource>>>,
}

impl DeviceResources {
    fn register(&self, id: Id<dyn DynResource>) {
        self.live.lock().insert(id);
    }

    fn unregister(&self, id: Id<dyn DynResource>) {
        self.live.lock().remove(&id);
    }
}

/// Convert acceleration-structure build entries referring to audited
/// buffers into ones referring to the erased dynamic type expected by
/// `self.inner`. Shared by [`Device`] and
/// [`command_encoder::CommandEncoder`].
fn convert_entries<'a>(
    entries: &crate::AccelerationStructureEntries<'a, Buffer>,
) -> crate::AccelerationStructureEntries<'a, dyn DynBuffer> {
    match entries {
        crate::AccelerationStructureEntries::Instances(instances) => {
            crate::AccelerationStructureEntries::Instances(crate::AccelerationStructureInstances {
                buffer: instances.buffer.map(|b| b.as_dyn()),
                offset: instances.offset,
                count: instances.count,
            })
        }
        crate::AccelerationStructureEntries::Triangles(triangles) => {
            crate::AccelerationStructureEntries::Triangles(
                triangles
                    .iter()
                    .map(|t| crate::AccelerationStructureTriangles {
                        vertex_buffer: t.vertex_buffer.map(|b| b.as_dyn()),
                        vertex_format: t.vertex_format,
                        first_vertex: t.first_vertex,
                        vertex_count: t.vertex_count,
                        vertex_stride: t.vertex_stride,
                        indices: t.indices.as_ref().map(|i| {
                            crate::AccelerationStructureTriangleIndices {
                                buffer: i.buffer.map(|b| b.as_dyn()),
                                format: i.format,
                                offset: i.offset,
                                count: i.count,
                            }
                        }),
                        transform: t.transform.as_ref().map(|t| {
                            crate::AccelerationStructureTriangleTransform {
                                buffer: t.buffer.as_dyn(),
                                offset: t.offset,
                            }
                        }),
                        flags: t.flags,
                    })
                    .collect(),
            )
        }
        crate::AccelerationStructureEntries::AABBs(entries) => {
            crate::AccelerationStructureEntries::AABBs(
                entries
                    .iter()
                    .map(|e| crate::AccelerationStructureAABBs {
                        buffer: e.buffer.map(|b| b.as_dyn()),
                        offset: e.offset,
                        count: e.count,
                        stride: e.stride,
                        flags: e.flags,
                    })
                    .collect(),
            )
        }
    }
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

impl<T: ?Sized, M> fmt::Debug for Audited<T, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.id.fmt(f)
    }
}

impl fmt::Debug for SurfaceTexture {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.id.fmt(f)
    }
}
