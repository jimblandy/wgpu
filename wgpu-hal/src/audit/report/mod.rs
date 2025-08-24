//! Reporting violations of wgpu_hal safety requirements.

use crate::audit::Id;
use crate::audit::location::Location;
use crate::audit::state::State;

use alloc::string::String;
use core::writeln;
use core::fmt::Write as _;

/// A specific violation of a `wgpu_hal` safety requirement.
pub struct Operation<'s> {
    /// The audit state for the instance.
    state: &'s State,

    /// The `wgpu_hal` trait and method name being attempted.
    ///
    /// This is something like `Device::create_buffer`.
    pub name: &'static str,

    /// The object the operation was being applied to.
    pub this: Id,
}

pub enum ReportKind {
    UseAfterFree(Id),
    DoubleFree(Id),
}

impl ReportKind {
    pub fn to_string(&self, op: &Operation, location: &Location) -> String {
        use ReportKind as Rk;

        let mut buf = format!("wgpu_hal::audit {:?} instance: ", op.state.backend);
        let out = &mut buf;
        match self {
            Rk::InvalidId(id) => {
                writeln!(out, "tried to use invalid resource id {id}").unwrap();
            }
            Rk::DoubleFree(_audit_id) => todo!(),
            Rk::UseAfterFree(_audit_id) => todo!(),
        }
        writeln!(out, "Stack:").unwrap();
        writeln!(out, "{location}").unwrap();

        buf
    }
}
