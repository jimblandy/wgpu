//! Implementation of `core::fmt::Display` for [`wgpu_hal::audit::Report`].

use core::fmt;

use crate::audit::report::Report;

use super::ReportKind;

impl fmt::Display for Report {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            ReportKind::DoubleFree(_audit_id) => todo!(),
        }
    }
}
