/*! Benchmarking utilities. */

#[cfg(feature = "perf-event")]
mod perf_event;
#[cfg(feature = "perf-event")]
use perf_event as chosen;

#[cfg(not(any(feature = "perf-event")))]
mod dummy;
#[cfg(not(any(feature = "perf-event")))]
use dummy as chosen;

#[allow(unused_imports)]
pub use chosen::Counters;

#[allow(unused_macros)]
macro_rules! stage {
    { $name:path $body:block } => {
        {
            use $crate::benchmark::Counters;
            let mut counters = Counters::create();
            counters.start();
            let value = { $body };
            counters.stop();
            counters.report(stringify!($name));
            value
        }
    }
}

#[allow(unused_imports)]
pub(crate) use stage;
