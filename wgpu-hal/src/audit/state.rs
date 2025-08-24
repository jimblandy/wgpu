/*! Types for reporting hal activity to an auditor.

This module defines the [`State`] type, which holds the auditor
to which all operations on an [`audit::Instance`] are reported,
and allocates ids for resources.

[`audit::Instance`]: super::Instance

*/

use crate::audit::{Auditor, Id};
use crate::audit::op;
use alloc::boxed::Box;
use core::result::Result;
use core::sync::atomic;
use parking_lot::Mutex;

/// The id to assign to the next audited resource created.
///
/// This is process-global, rather than associated with a given
/// instance, 
static NEXT_ID: atomic::AtomicU64 = atomic::AtomicU64::new(0);

pub struct State {
    /// The auditor to report operations to.
    pub auditor: Mutex<Box<dyn Auditor>>,
}

impl State {
    pub fn new(auditor: Box<dyn Auditor>) -> Self {
        Self {
            auditor: Mutex::new(auditor),
        }
    }

    pub fn new_id<T: ?Sized>(&self) -> Id<T> {
        let next_id = NEXT_ID.fetch_add(1, atomic::Ordering::SeqCst);
        Id::new(next_id)
    }

    pub fn operation(&self, op: op::Op) {
        self.auditor.lock().operation(op);
    }

    pub fn result(&self, result: Result<op::Finished, op::Error>) {
        self.auditor.lock().result(result);
    }
}
