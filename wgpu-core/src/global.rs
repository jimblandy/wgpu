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

pub struct Global {
    pub instance: Instance,
}
