//! Recording where in the program events occurred: backtraces, etc.

cfg_if::cfg_if! {
    if #[cfg(vulkan)] { // JIMB: lame
        mod std_backtrace;
        pub use std_backtrace::*;
    } else {
        mod stub_location;
        pub use stub_location::*;
    }
}
