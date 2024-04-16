use wgt::{Backend, WasmNotSendSync};

use crate::{
    global::Global,
    hub::Hub,
    instance::{Instance, Surface},
};

pub trait HalApi: hal::Api + 'static + WasmNotSendSync {
    const VARIANT: Backend;
    fn create_instance_from_hal(name: &str, hal_instance: Self::Instance) -> Instance;
    fn instance_as_hal(instance: &Instance) -> Option<&Self::Instance>;
    fn hub(global: &Global) -> &Hub<Self>;
    fn get_surface(surface: &Surface) -> Option<&Self::Surface>;
}

impl HalApi for hal::api::Empty {
    const VARIANT: Backend = Backend::Empty;
    fn create_instance_from_hal(_: &str, _: Self::Instance) -> Instance { todo!() }
    fn instance_as_hal(_: &Instance) -> Option<&Self::Instance> { todo!() }
    fn hub(_: &Global) -> &Hub<Self> { todo!() }
    fn get_surface(_: &Surface) -> Option<&Self::Surface> { todo!() }
}

#[cfg(vulkan)]
impl HalApi for hal::api::Vulkan {
    const VARIANT: Backend = Backend::Vulkan;
    fn create_instance_from_hal(name: &str, hal_instance: Self::Instance) -> Instance { todo!() }
    fn instance_as_hal(instance: &Instance) -> Option<&Self::Instance> { todo!() }
    fn hub(global: &Global) -> &Hub<Self> { todo!() }
    fn get_surface(surface: &Surface) -> Option<&Self::Surface> { todo!() }
}

#[cfg(metal)]
impl HalApi for hal::api::Metal {
    const VARIANT: Backend = Backend::Metal;
    fn create_instance_from_hal(name: &str, hal_instance: Self::Instance) -> Instance { todo!() }
    fn instance_as_hal(instance: &Instance) -> Option<&Self::Instance> { todo!() }
    fn hub(global: &Global) -> &Hub<Self> { todo!() }
    fn get_surface(surface: &Surface) -> Option<&Self::Surface> { todo!() }
}

#[cfg(dx12)]
impl HalApi for hal::api::Dx12 {
    const VARIANT: Backend = Backend::Dx12;
    fn create_instance_from_hal(name: &str, hal_instance: Self::Instance) -> Instance { todo!() }
    fn instance_as_hal(instance: &Instance) -> Option<&Self::Instance> { todo!() }
    fn hub(global: &Global) -> &Hub<Self> { todo!() }
    fn get_surface(surface: &Surface) -> Option<&Self::Surface> { todo!() }
}

#[cfg(gles)]
impl HalApi for hal::api::Gles {
    const VARIANT: Backend = Backend::Gl;
    fn create_instance_from_hal(name: &str, hal_instance: Self::Instance) -> Instance { todo!() }
    fn instance_as_hal(instance: &Instance) -> Option<&Self::Instance> { todo!() }
    fn hub(global: &Global) -> &Hub<Self> { todo!() }
    fn get_surface(surface: &Surface) -> Option<&Self::Surface> { todo!() }
}
