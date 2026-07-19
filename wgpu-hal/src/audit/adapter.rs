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
        Ok(crate::OpenDevice {
            device: super::Device::wrap(device, self.shared.clone()),
            queue: super::Queue::wrap(queue, self.shared.clone()),
        })
    }

    unsafe fn texture_format_capabilities(
        &self,
        format: wgt::TextureFormat,
    ) -> crate::TextureFormatCapabilities {
        unsafe { self.inner.texture_format_capabilities(format) }
    }

    unsafe fn surface_capabilities(
        &self,
        surface: &<super::Api as crate::Api>::Surface,
    ) -> Option<crate::SurfaceCapabilities> {
        unsafe { self.inner.surface_capabilities(surface.as_dyn()) }
    }

    unsafe fn get_presentation_timestamp(&self) -> wgt::PresentationTimestamp {
        unsafe { self.inner.get_presentation_timestamp() }
    }

    fn get_ordered_buffer_usages(&self) -> wgt::BufferUses {
        self.inner.get_ordered_buffer_usages()
    }

    fn get_ordered_texture_usages(&self) -> wgt::TextureUses {
        self.inner.get_ordered_texture_usages()
    }
}
