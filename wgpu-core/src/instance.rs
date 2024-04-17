use std::sync::Arc;

use crate::{
    any_surface::AnySurface,
    api_log,
    device::{queue::Queue, resource::Device, DeviceDescriptor},
    global::Global,
    hal_api::HalApi,
    id::markers,
    id::{Id, Marker},
    present::Presentation,
    resource::{Resource, ResourceInfo, ResourceType},
    resource_log, LabelHelpers,
};

use parking_lot::Mutex;
use wgt::{Backend, Backends, PowerPreference};

use hal::{Adapter as _, Instance as _, OpenDevice};
use thiserror::Error;

#[derive(Clone, Debug, Error)]
#[error("Limit '{name}' value {requested} is better than allowed {allowed}")]
pub struct FailedLimit {
    name: &'static str,
    requested: u64,
    allowed: u64,
}

#[test]
fn downlevel_default_limits_less_than_default_limits() { todo!() }

#[derive(Default)]
pub struct Instance {
    #[allow(dead_code)]
    pub name: String,
    pub flags: wgt::InstanceFlags,
}

pub struct Surface {
}

impl Resource for Surface {
    const TYPE: ResourceType = "Surface";

    type Marker = markers::Surface;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }

    fn label(&self) -> String { todo!() }
}

pub struct Adapter<A: HalApi> {
    marker: std::marker::PhantomData<A>,
}

impl<A: HalApi> Resource for Adapter<A> {
    const TYPE: ResourceType = "Adapter";

    type Marker = markers::Adapter;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum IsSurfaceSupportedError {
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum GetSurfaceSupportError {
}

#[derive(Clone, Debug, Error)]
/// Error when requesting a device from the adaptor
#[non_exhaustive]
pub enum RequestDeviceError {
}

#[derive(Clone, Debug, Error)]
#[error("Adapter is invalid")]
pub struct InvalidAdapter;

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum RequestAdapterError {
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum CreateSurfaceError {
}
