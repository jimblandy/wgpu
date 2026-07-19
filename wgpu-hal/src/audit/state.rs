/*! Types for reporting hal activity to an auditor.

This module defines the [`State`] type, which holds the auditor
to which all operations on an [`audit::Instance`] are reported,
and allocates ids for resources.

[`audit::Instance`]: super::Instance

*/

use crate::audit::location::Location;
use crate::audit::report::Violation;
use crate::audit::{Audited, Auditor, Device, Id, OwnedByDevice};
use alloc::boxed::Box;
use core::sync::atomic;
use parking_lot::Mutex;

/// The id to assign to the next audited resource created.
///
/// This is process-global, rather than associated with a given
/// instance,
static NEXT_ID: atomic::AtomicU64 = atomic::AtomicU64::new(0);

pub struct State {
    /// The backend that the audited instance is wrapping.
    pub backend: wgpu_types::Backend,

    /// The auditor to report violations to.
    pub auditor: Mutex<Box<dyn Auditor>>,
}

impl State {
    pub fn new(auditor: Box<dyn Auditor>, backend: wgpu_types::Backend) -> Self {
        Self {
            backend,
            auditor: Mutex::new(auditor),
        }
    }

    pub fn new_id<T: ?Sized>(&self) -> Id<T> {
        let next_id = NEXT_ID.fetch_add(1, atomic::Ordering::SeqCst);
        Id::new(next_id)
    }

    pub fn violation(&self, violation: Violation) {
        self.auditor.lock().violation(violation);
    }

    /// Report a violation if `resource` was not created by
    /// `expected_device`.
    ///
    /// `method` names the `wgpu_hal` trait and method being called,
    /// e.g. `"Device::destroy_buffer"`, for diagnostics.
    pub fn check_owned<T: ?Sized>(
        &self,
        method: &'static str,
        expected_device: Id<Device>,
        resource: &Audited<T, OwnedByDevice>,
    ) {
        let actual_device = resource.device();
        if actual_device != expected_device {
            self.violation(Violation::WrongDevice {
                method,
                resource: resource.erased_id(),
                actual_device,
                expected_device,
                location: Location::force_capture(),
            });
        }
    }
}
