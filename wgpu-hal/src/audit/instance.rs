/*! Implementation of [`validation_layer::Instance`]. */

use crate::audit;
use crate::audit::state;

use crate::{DynInstance, DynSurface};
use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;

impl audit::Instance {
    fn new(backend: wgpu_types::Backend, inner: Box<dyn DynInstance>) -> Self {
        let state = state::State::new(backend);
        let id = state.new_id();
        // Instances are their own parents.
        state.register_resource_with_id(id, state::ResourceKind::Instance, id);
        Self {
            inner,
            id,
            state: Arc::new(state),
        }
    }
}

impl crate::Instance for audit::Instance {
    type A = super::Api;

    unsafe fn init(_desc: &crate::InstanceDescriptor) -> Result<Self, crate::InstanceError> {
        panic!("Call `validation_layer::Instance::` new instead");
    }

    unsafe fn create_surface(
        &self,
        display_handle: raw_window_handle::RawDisplayHandle,
        window_handle: raw_window_handle::RawWindowHandle,
    ) -> Result<audit::Surface, crate::InstanceError> {
        let state = Arc::clone(&self.state);
        let inner = unsafe { self.inner.create_surface(display_handle, window_handle)? };
        let id = state.register_resource(state::ResourceKind::Surface, self.id);
        Ok(audit::Surface { inner, id, state })
    }

    unsafe fn enumerate_adapters(
        &self,
        surface_hint: Option<&super::Surface>,
    ) -> Vec<crate::ExposedAdapter<super::Api>> {
        let surface_hint: Option<&dyn DynSurface> = match surface_hint {
            None => None,
            // This looks like a no-op, but coerces to `&dyn Dynsurface`.
            Some(surface) => Some(surface),
        };
        let adapters = unsafe { self.inner.enumerate_adapters(surface_hint) };
        adapters
            .into_iter()
            .map(|exposed_adapter| {
                let crate::DynExposedAdapter {
                    adapter,
                    info,
                    features,
                    capabilities,
                } = exposed_adapter;
                let state = Arc::clone(&self.state);
                let id = state.register_resource(state::ResourceKind::Adapter, self.id);
                let adapter = super::Adapter {
                    inner: adapter,
                    id,
                    state,
                };
                crate::ExposedAdapter {
                    adapter,
                    info,
                    features,
                    capabilities,
                }
            })
            .collect()
    }
}
