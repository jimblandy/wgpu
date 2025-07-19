/*! Types for tracking hal state, for auditing.

This module defines the [`State`] type, which holds a database of
state about objects created under the [`audit::Instance`].

[`audit::Instance`]: super::Instance

*/

use crate::audit::AuditId;
use crate::audit::location::Location;
use crate::audit::report;
use alloc::boxed::Box;
use alloc::sync::Arc;
use hashbrown::HashMap;
use parking_lot::Mutex;

/// All audit state for a single `wgpu_hal::Instance`.
#[derive(Clone)]
pub struct Shared(pub Arc<Mutex<State>>);

impl Shared {
    pub fn new(
        backend: wgpu_types::Backend,
        report_callback: Box<crate::audit::ReportCallback>,
    ) -> Self {
        let state = State::new(backend, report_callback);
        Self(Arc::new(Mutex::new(state)))
    }

    pub fn lock(&self) -> parking_lot::MutexGuard<State> {
        self.0.lock()
    }
}

pub struct State {
    pub backend: wgpu_types::Backend,

    /// All resources allocated in this instance.
    ///
    /// Since we don't remove old resources, 
    pub resources: HashMap<AuditId, Resource>,

    /// The callback to report violations to.
    ///
    /// This is a refcell because many `State` methods for checking
    /// some safety condition have no need to change the `State`,
    /// unless they detect a violation. It seems best for such methods
    /// to require only a `&State`, but `ReportCallback` is a `FnMut`.
    pub report_callback: core::cell::RefCell<Box<crate::audit::ReportCallback>>,

    /// The id to allocate to the next resource allocated.
    pub next_id: u64,
}

impl State {
    pub fn new(
        backend: wgpu_types::Backend,
        report_callback: Box<crate::audit::ReportCallback>,
    ) -> Self {
        Self {
            backend,
            resources: HashMap::new(),
            report_callback: report_callback.into(),
            next_id: 0,
        }
    }

    pub fn new_id(&mut self) -> AuditId {
        let id = AuditId(self.next_id);
        self.next_id += 1;
        id
    }

    pub fn id_type_name(&self, id: AuditId) -> &'static str {
        match self.resources.get(&id) {
            Some(resource) => resource.kind.type_name(),
            None => "(unknown audit id)",
        }
    }

    pub fn register_resource(&mut self, kind: ResourceKind, parent: AuditId) -> AuditId {
        let id = self.new_id();
        self.register_resource_with_id(id, kind, parent);
        id
    }

    pub fn register_resource_with_id(&mut self, id: AuditId, kind: ResourceKind, parent: AuditId) {
        let resource = Resource::new(parent, kind);
        if let Some(prior) = self.resources.insert(id, resource) {
            panic!("attempt to insert resource under duplicate id {id}: prior resource {prior:?}");
        }
    }

    pub fn report(&self, op: &report::Operation, kind: report::ReportKind) {
        let location = Location::force_capture();
        let text = kind.to_string(op, &location);
        (self.report_callback.borrow_mut())(text);
    }

    pub fn check_alive(&self, id: AuditId) {
        let Some(resource) = self.resources.get(&id) else {
            return;
        };
        if let Some(ref destroyed_at) = resource.destroyed_at {
        }
    }
}

#[derive(Debug)]
pub struct Resource {
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
