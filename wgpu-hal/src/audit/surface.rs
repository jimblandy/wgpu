/*! Implementation of [`validation_layer::Surface`]. */
#![allow(unused_variables)]

use crate::audit;
use crate::DynTexture;
use core::borrow::Borrow;

impl crate::Surface for audit::Surface {
    type A = audit::Api;

    unsafe fn configure(
        &self,
        device: &audit::Device,
        config: &crate::SurfaceConfiguration,
    ) -> Result<(), crate::SurfaceError> {
        todo!()
    }

    unsafe fn unconfigure(&self, device: &audit::Device) {
        todo!()
    }

    unsafe fn acquire_texture(
        &self,
        timeout: Option<core::time::Duration>,
        fence: &audit::Fence,
    ) -> Result<crate::AcquiredSurfaceTexture<audit::Api>, crate::SurfaceError> {
        todo!()
    }

    unsafe fn discard_texture(&self, texture: audit::SurfaceTexture) {
        todo!()
    }
}

impl Borrow<audit::Texture> for audit::SurfaceTexture {
    fn borrow(&self) -> &audit::Texture {
        &self.texture
    }
}

impl Borrow<dyn DynTexture> for audit::SurfaceTexture {
    fn borrow(&self) -> &dyn DynTexture {
        &self.texture
    }
}
