//! A stub call stacks type for `no_std` builds.

use std::fmt;

#[derive(Debug)]
pub struct Location;

impl Location {
    pub fn force_capture() -> Self {
        Self
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("(no backtrace)")
    }
}
