/*! Implementation of [`validation_layer::Instance`]. */

use crate::audit;
use crate::audit::state;

use crate::{DynInstance, DynSurface};
use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;

impl audit::Instance {
    pub(crate) fn new(
        inner: Box<dyn DynInstance>,
        backend: wgpu_types::Backend,
        auditor: Box<dyn audit::Auditor>,
    ) -> Self {
        let state = state::State::new(auditor, backend);
        let id = state.new_id();
        Self {
            inner,
            id,
            shared: Arc::new(state),
            metadata: (),
        }
    }
}

impl crate::Instance for audit::Instance {
    type A = super::Api;

    unsafe fn init(_desc: &crate::InstanceDescriptor<'_>) -> Result<Self, crate::InstanceError> {
        panic!("Call `wgpu_hal::audit::Instance::new` instead");
    }

    unsafe fn create_surface(
        &self,
        display_handle: raw_window_handle::RawDisplayHandle,
        window_handle: raw_window_handle::RawWindowHandle,
    ) -> Result<audit::Surface, crate::InstanceError> {
        let surface = unsafe { self.inner.create_surface(display_handle, window_handle)? };
        Ok(audit::Surface::wrap(surface, self.shared.clone()))
    }

    unsafe fn enumerate_adapters(
        &self,
        surface_hint: Option<&super::Surface>,
    ) -> Vec<crate::ExposedAdapter<super::Api>> {
        let surface_hint: Option<&dyn DynSurface> = surface_hint.map(|surface| &*surface.inner);
        let inner_adapters = unsafe { self.inner.enumerate_adapters(surface_hint) };
        inner_adapters
            .into_iter()
            .map(|inner_adapter| {
                let crate::DynExposedAdapter {
                    adapter,
                    info,
                    features,
                    capabilities,
                } = inner_adapter;

                crate::ExposedAdapter {
                    adapter: audit::Adapter::wrap(adapter, self.shared.clone()),
                    info,
                    features,
                    capabilities,
                }
            })
            .collect()
    }
}
