/*! Implementation of [`validation_layer::Instance`]. */

use crate::audit;
use crate::audit::op;
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
        let state = state::State::new(auditor);
        let id = state.new_id();
        state.result(Ok(op::Finished::NewInstance { id, backend }));
        Self {
            inner,
            id,
            shared: Arc::new(state),
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
        self.shared.operation(op::Op::Instance {
            id: self.id,
            op: op::InstanceOp::CreateSurface { display_handle, window_handle }
        });
        let result = unsafe { self.inner.create_surface(display_handle, window_handle) };
        match result {
            Ok(surface) => {
                let surface_id = self.shared.new_id();
                self.shared.result(Ok(op::Finished::CreateSurface(surface_id)));
                Ok(audit::Surface {
                    inner: surface,
                    id: surface_id,
                    shared: self.shared.clone()
                })
            }
            Err(error) => {
                self.shared.result(Err(error.clone().into()));
                Err(error)
            }
        }
    }

    unsafe fn enumerate_adapters(
        &self,
        surface_hint: Option<&super::Surface>,
    ) -> Vec<crate::ExposedAdapter<super::Api>> {
        self.shared.operation(op::Op::Instance {
            id: self.id,
            op: op::InstanceOp::EnumerateAdapters {
                surface_hint: surface_hint.map(|surface| surface.id),
            }
        });
        let surface_hint: Option<&dyn DynSurface> = match surface_hint {
            None => None,
            Some(surface) => Some(&*surface.inner),
        };
        let inner_adapters = unsafe { self.inner.enumerate_adapters(surface_hint) };
        let mut auditing_adapters = Vec::with_capacity(inner_adapters.len());
        let mut finished_adapters = Vec::with_capacity(inner_adapters.len());
        for inner_adapter in inner_adapters {
            let crate::DynExposedAdapter {
                adapter,
                info,
                features,
                capabilities,
            } = inner_adapter;

            let shared = self.shared.clone();
            let id = shared.new_id();
            let auditing_adapter = audit::Adapter {
                inner: adapter,
                id,
                shared,
            };
            auditing_adapters.push(crate::ExposedAdapter {
                adapter: auditing_adapter,
                info: info.clone(),
                features: features.clone(),
                capabilities: capabilities.clone(),
            });
            finished_adapters.push(op::OpExposedAdapter {
                adapter: id,
                info,
                features,
                capabilities
            })
        }
        self.shared.result(Ok(op::Finished::EnumerateAdapters(finished_adapters)));
        auditing_adapters
    }
}
