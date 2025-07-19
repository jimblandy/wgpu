//! Capturing call stacks for `wgpu_hal` audits with `std::backtrace`.

use alloc::string::ToString;
use core::fmt;

#[derive(Debug)]
pub struct Location(std::backtrace::Backtrace);

impl Location {
    pub fn force_capture() -> Self {
        Self(std::backtrace::Backtrace::force_capture())
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let output = self.0.to_string();
        write_trimmed_backtrace(f, &output)
    }
}

/// Discard boring frames from a backtrace.
fn write_trimmed_backtrace(f: &mut fmt::Formatter<'_>, full: &str) -> fmt::Result {
    let mut lines = full.lines().fuse();

    // Drop frames in `wgpu_hal::audit` from the young end of the stack.
    while let Some(line) = lines.next() {
        if !line.contains("wgpu_hal::audit") {
            writeln!(f, "{line}")?;
            break;
        }

        // Skip the frame's second line.
        lines.next();
    }

    // Copy over lines until we see something that indicates that the
    // rest are all just test harness.
    for line in lines {
        if line.contains("wgpu_test::run::execute_test") {
            break;
        }

        writeln!(f, "{line}")?;
    }

    Ok(())
}
