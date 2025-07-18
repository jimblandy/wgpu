/*! Types for tracking hal state, for auditing.

This module defines the [`State`] type, which holds a database of
state about objects created under the [`audit::Instance`].

[`audit::Instance`]: super::Instance

*/

use crate::audit::location::Location;
use crate::audit::AuditId;
use alloc::boxed::Box;
use core::sync::atomic;
use hashbrown::HashMap;
use parking_lot::Mutex;

/// All audit state for a single `wgpu_hal::Instance`.
///
/// All audit wrappers hold an `Arc` to this.
pub struct State {
    backend: wgpu_types::Backend,

    /// All resources allocated in this instance.
    resources: Mutex<HashMap<AuditId, Resource>>,

    /// The callback to report violations to.
    report_callback: Box<crate::audit::ReportCallback>,

    /// The id to allocate to the next resource allocated.
    next_id: atomic::AtomicU64,
}

impl State {
    pub fn new(
        backend: wgpu_types::Backend,
        report_callback: Box<crate::audit::ReportCallback>,
    ) -> Self {
        Self {
            backend,
            resources: Mutex::new(HashMap::new()),
            report_callback,
            next_id: atomic::AtomicU64::new(0),
        }
    }

    pub fn new_id(&self) -> AuditId {
        AuditId(self.next_id.fetch_add(1, atomic::Ordering::SeqCst))
    }

    pub fn id_type_name(&self, id: AuditId) -> &'static str {
        match self.resources.lock().get(&id) {
            Some(resource) => resource.kind.type_name(),
            None => "(unknown audit id)",
        }
    }

    pub fn register_resource(&self, kind: ResourceKind, parent: AuditId) -> AuditId {
        let id = self.new_id();
        self.register_resource_with_id(id, kind, parent);
        id
    }

    pub fn register_resource_with_id(&self, id: AuditId, kind: ResourceKind, parent: AuditId) {
        let resource = Resource::new(parent, kind);
        if let Some(prior) = self.resources.lock().insert(id, resource) {
            panic!("attempt to insert resource under duplicate id {id}: prior resource {prior:?}");
        }
    }
}

#[derive(Debug)]
struct Resource {
    parent: AuditId,
    allocated_at: Location,
    destroyed_at: Option<Location>,
    kind: ResourceKind,
}

#[derive(Debug)]
pub enum ResourceKind {
    Instance,
    Surface,
    Adapter,
    Device(crate::audit::device::Detail),
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
    AccelerationStructure,
}

impl Resource {
    fn new(parent: AuditId, kind: ResourceKind) -> Self {
        Self {
            parent,
            allocated_at: Location::force_capture(),
            destroyed_at: None,
            kind,
        }
    }

    fn is_alive(&self) -> bool {
        self.destroyed_at.is_none()
    }
}

impl ResourceKind {
    fn type_name(&self) -> &'static str {
        match self {
            ResourceKind::Instance => "Instance",
            ResourceKind::Surface => "Surface",
            ResourceKind::Adapter => "Adapter",
            ResourceKind::Device(_) => "Device",
            ResourceKind::Queue => "Queue",
            ResourceKind::CommandEncoder => "CommandEncoder",
            ResourceKind::CommandBuffer => "CommandBuffer",
            ResourceKind::Buffer => "Buffer",
            ResourceKind::Texture => "Texture",
            ResourceKind::SurfaceTexture => "SurfaceTexture",
            ResourceKind::TextureView => "TextureView",
            ResourceKind::Sampler => "Sampler",
            ResourceKind::QuerySet => "QuerySet",
            ResourceKind::Fence => "Fence",
            ResourceKind::BindGroupLayout => "BindGroupLayout",
            ResourceKind::BindGroup => "BindGroup",
            ResourceKind::PipelineLayout => "PipelineLayout",
            ResourceKind::ShaderModule => "ShaderModule",
            ResourceKind::RenderPipeline => "RenderPipeline",
            ResourceKind::ComputePipeline => "ComputePipeline",
            ResourceKind::PipelineCache => "PipelineCache",
            ResourceKind::AccelerationStructure => "AccelerationStructure",
        }
    }
}
