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
        unsafe { self.inner.configure(device.as_dyn(), config) }
    }

    unsafe fn unconfigure(&self, device: &audit::Device) {
        unsafe { self.inner.unconfigure(device.as_dyn()) }
    }

    unsafe fn acquire_texture(
        &self,
        timeout: Option<core::time::Duration>,
        fence: &audit::Fence,
    ) -> Result<crate::AcquiredSurfaceTexture<audit::Api>, crate::SurfaceError> {
        // We cannot pass this through transparently: `audit::SurfaceTexture`
        // must cache an owned `audit::Texture` so that
        // `Borrow<audit::Texture>` has something to point to (see the
        // comment on `SurfaceTexture::texture`), but the only texture we
        // have here is a *borrowed* view of `inner`'s surface texture
        // (`dyn DynSurfaceTexture: Borrow<dyn DynTexture>`), not an owned
        // one. Manufacturing an owned `Box<dyn DynTexture>` from that
        // borrow would either require downcasting to a concrete,
        // backend-specific type (which this backend-agnostic layer cannot
        // do) or aliasing `inner`'s allocation, which would double-free
        // when both boxes are dropped. This needs a real design change
        // (e.g. giving `SurfaceTexture` a non-owning way to satisfy
        // `Borrow<audit::Texture>`) before it can be implemented soundly.
        todo!()
    }

    unsafe fn discard_texture(&self, texture: audit::SurfaceTexture) {
        unsafe { self.inner.discard_texture(texture.inner) }
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
