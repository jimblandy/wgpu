/*! Implementation of [`validation_layer::Surface`]. */
#![allow(unused_variables)]

use crate::DynTexture;
use core::borrow::Borrow;

impl crate::Surface for super::Surface {
    type A = super::Api;

    unsafe fn configure(
        &self,
        device: &super::Device,
        config: &crate::SurfaceConfiguration,
    ) -> Result<(), crate::SurfaceError> {
        todo!()
    }

    unsafe fn unconfigure(&self, device: &super::Device) {
        todo!()
    }

    unsafe fn acquire_texture(
        &self,
        timeout: Option<core::time::Duration>,
        fence: &super::Fence,
    ) -> Result<Option<crate::AcquiredSurfaceTexture<Self::A>>, crate::SurfaceError> {
        todo!()
    }

    unsafe fn discard_texture(&self, texture: super::SurfaceTexture) {
        todo!()
    }
}

impl Borrow<super::Texture> for super::SurfaceTexture {
    fn borrow(&self) -> &super::Texture {
        &self.texture
    }
}

impl Borrow<dyn DynTexture> for super::SurfaceTexture {
    fn borrow(&self) -> &dyn DynTexture {
        &*self.texture.inner
    }
}
