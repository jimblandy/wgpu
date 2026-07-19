/*! Implementation of [`validation_layer::Queue`]. */
#![allow(unused_variables)]

impl crate::Queue for super::Queue {
    type A = super::Api;

    unsafe fn submit(
        &self,
        command_buffers: &[&super::CommandBuffer],
        surface_textures: &[&super::SurfaceTexture],
        signal_fence: (&super::Fence, crate::FenceValue),
    ) -> Result<(), crate::DeviceError> {
        todo!()
    }

    unsafe fn present(
        &self,
        surface: &super::Surface,
        texture: super::SurfaceTexture,
    ) -> Result<(), crate::SurfaceError> {
        todo!()
    }

    unsafe fn wait_for_idle(&self) -> Result<(), crate::DeviceError> {
        todo!()
    }

    unsafe fn get_timestamp_period(&self) -> f32 {
        todo!()
    }
}
