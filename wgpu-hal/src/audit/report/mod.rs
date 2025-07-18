//! Reporting violations of wgpu_hal safety requirements.

mod display;

use crate::audit::{AuditId, State};
use alloc::sync::Arc;

/// A specific violation of a `wgpu_hal` safety requirement.
pub struct Report {
    state: Arc<State>,
    kind: ReportKind,
}

pub enum ReportKind {
    DoubleFree(AuditId),
}
