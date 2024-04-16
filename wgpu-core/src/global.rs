use std::sync::Arc;

use wgt::Backend;

use crate::{
    hal_api::HalApi,
    hub::{HubReport, Hubs},
    instance::{Instance, Surface},
    registry::{Registry, RegistryReport},
    resource_log,
    storage::Element,
};

#[derive(Debug, PartialEq, Eq)]
pub struct GlobalReport {
    pub surfaces: RegistryReport,
    #[cfg(vulkan)]
    pub vulkan: Option<HubReport>,
    #[cfg(metal)]
    pub metal: Option<HubReport>,
    #[cfg(dx12)]
    pub dx12: Option<HubReport>,
    #[cfg(gles)]
    pub gl: Option<HubReport>,
}

impl GlobalReport {
    pub fn surfaces(&self) -> &RegistryReport { todo!() }
    pub fn hub_report(&self, backend: Backend) -> &HubReport { todo!() }
}

pub struct Global {
    pub instance: Instance,
    pub(crate) surfaces: Registry<Surface>,
    pub(crate) hubs: Hubs,
}

impl Global {
    pub fn new(name: &str, instance_desc: wgt::InstanceDescriptor) -> Self { todo!() }

    /// # Safety
    ///
    /// Refer to the creation of wgpu-hal Instance for every backend.
    pub unsafe fn from_hal_instance<A: HalApi>(name: &str, hal_instance: A::Instance) -> Self { todo!() }

    /// # Safety
    ///
    /// - The raw instance handle returned must not be manually destroyed.
    pub unsafe fn instance_as_hal<A: HalApi>(&self) -> Option<&A::Instance> { todo!() }

    /// # Safety
    ///
    /// - The raw handles obtained from the Instance must not be manually destroyed
    pub unsafe fn from_instance(instance: Instance) -> Self { todo!() }

    pub fn clear_backend<A: HalApi>(&self, _dummy: ()) { todo!() }

    pub fn generate_report(&self) -> GlobalReport { todo!() }
}

impl Drop for Global {
    fn drop(&mut self) { todo!() }
}

#[cfg(send_sync)]
fn _test_send_sync(global: &Global) { todo!() }
