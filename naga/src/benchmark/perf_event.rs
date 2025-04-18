/*! Benchmarking stages with the [`perf_event`] crate.

[perf_event]: https://docs.rs/perf-event/latest/perf_event
 */

pub struct Counters {
    group: perf_event::Group,
    cycles: perf_event::Counter,
    insns: perf_event::Counter,
}

impl Counters {
    pub fn create() -> Self {
        use perf_event::{Group, Builder};
        use perf_event::events::{Event, Hardware};
        let mut group = Group::new()
            .expect("benchmark::perf_event: failed to create Group");
        let cycles = Builder::new()
            .group(&mut group)
            .kind(Event::Hardware(Hardware::CPU_CYCLES))
            .build()
            .expect("benchmark::perf_event: failed to build `cycles` counter");
        let insns = Builder::new()
            .group(&mut group)
            .kind(Event::Hardware(Hardware::INSTRUCTIONS))
            .build()
            .expect("benchmark::perf_event: failed to build `insns` counter");
            
        Counters {
            group,
            cycles,
            insns,
        }
    }
    pub fn start(&mut self) {
        self.group.enable()
            .expect("benchmark::perf_event: failed to start Group");
    }
    pub fn stop(&mut self) {
        self.group.disable()
            .expect("benchmark::perf_event: failed to stop Group");
    }
    pub fn report(&mut self, stage: &str) {
        let counts = self.group.read()
            .expect("benchmark::perf_event: failed to read Group counts");
        let cycles = counts[&self.cycles];
        let insns = counts[&self.insns];
        let cycles_per_insn = cycles as f64 / insns as f64;
        let m_cycles = cycles as f64 / 1e6;
        let m_insns = insns as f64 / 1e6;
        log::info!("{stage}: {m_cycles:.3} Mcycles / {m_insns:.3} Minsns = {cycles_per_insn:5.2} cycles/insn");
    }
}
