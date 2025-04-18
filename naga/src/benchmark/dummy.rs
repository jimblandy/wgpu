/*! No-op implementations of benchmarking macros. */

pub struct Counters;

impl Counters {
    pub fn create() -> Self { Counters }
    pub fn start(&mut self) {}
    pub fn stop(&mut self) {}
    pub fn report(&self, stage: &str) {}
}
