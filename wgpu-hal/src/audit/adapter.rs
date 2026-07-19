/*! Implementation of [`validation_layer::Adapter`]. */
#![allow(unused_variables)]

impl crate::Adapter for super::Adapter {
    type A = super::Api;

    unsafe fn open(
        &self,
        features: wgt::Features,
        limits: &wgt::Limits,
        memory_hints: &wgt::MemoryHints,
    ) -> Result<crate::OpenDevice<super::Api>, crate::DeviceError> {
        let crate::DynOpenDevice { device, queue } =
            unsafe { self.inner.open(features, limits, memory_hints)? };
        let device_id = self.shared.new_id();
        let queue_id = self.shared.new_id();
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

    fn get_ordered_buffer_usages(&self) -> wgt::BufferUses {
        todo!()
    }

    fn get_ordered_texture_usages(&self) -> wgt::TextureUses {
        todo!()
    }
}
