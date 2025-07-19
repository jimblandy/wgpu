/*! Implementation of [`validation_layer::Adapter`]. */
#![allow(unused_variables)]

use crate::audit::device;
use crate::audit::state;

impl crate::Adapter for super::Adapter {
    type A = super::Api;

    unsafe fn open(
        &self,
        features: wgt::Features,
        limits: &wgt::Limits,
        memory_hints: &wgt::MemoryHints,
    ) -> Result<crate::OpenDevice<super::Api>, crate::DeviceError> {
        self.check_alive();
        let crate::DynOpenDevice { device, queue } =
            unsafe { self.inner.open(features, limits, memory_hints)? };
        let mut guard = self.shared.0.lock();
        let queue_id = guard.new_id();
        let device_kind = state::ResourceKind::Device(device::Detail { queue: queue_id });
        let device_id = guard.register_resource(device_kind, self.id);
        guard
            .register_resource_with_id(queue_id, state::ResourceKind::Queue, device_id);
        Ok(crate::OpenDevice {
            device: super::Device {
                inner: device,
                id: device_id,
                shared: self.shared.clone(),
            },
            queue: super::Queue {
                inner: queue,
                id: queue_id,
                shared: self.shared.clone(),
            },
        })
    }

    unsafe fn texture_format_capabilities(
        &self,
        format: wgt::TextureFormat,
    ) -> crate::TextureFormatCapabilities {
        self.check_alive();
        todo!()
    }

    unsafe fn surface_capabilities(
        &self,
        surface: &<super::Api as crate::Api>::Surface,
    ) -> Option<crate::SurfaceCapabilities> {
        todo!()
    }

    unsafe fn get_presentation_timestamp(&self) -> wgt::PresentationTimestamp {
        todo!()
    }
}
