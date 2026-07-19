/*! Implementation of [`validation_layer::Queue`]. */
#![allow(unused_variables)]

use crate::DynCommandBuffer;
use crate::DynSurfaceTexture;
use alloc::vec::Vec;

impl crate::Queue for super::Queue {
    type A = super::Api;

    unsafe fn submit(
        &self,
        command_buffers: &[&super::CommandBuffer],
        surface_textures: &[&super::SurfaceTexture],
        signal_fence: (&super::Fence, crate::FenceValue),
    ) -> Result<(), crate::DeviceError> {
        let command_buffers: Vec<&dyn DynCommandBuffer> =
            command_buffers.iter().map(|cb| cb.as_dyn()).collect();
        let surface_textures: Vec<&dyn DynSurfaceTexture> = surface_textures
            .iter()
            .map(|st| &*st.inner)
            .collect();
        let signal_fence = (signal_fence.0.as_dyn(), signal_fence.1);
        unsafe {
            self.inner
                .submit(&command_buffers, &surface_textures, signal_fence)
        }
    }

    unsafe fn present(
        &self,
        surface: &super::Surface,
        texture: super::SurfaceTexture,
    ) -> Result<(), crate::SurfaceError> {
        unsafe { self.inner.present(surface.as_dyn(), texture.inner) }
    }

    unsafe fn wait_for_idle(&self) -> Result<(), crate::DeviceError> {
        unsafe { self.inner.wait_for_idle() }
    }

    unsafe fn get_timestamp_period(&self) -> f32 {
        unsafe { self.inner.get_timestamp_period() }
    }
}
