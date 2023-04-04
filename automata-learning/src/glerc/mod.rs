mod constraint;
mod state;

use automata::{Deterministic, TransitionSystem};

pub enum GlercOutput<TS: TransitionSystem = Deterministic, Inc = String> {
    Transition(TS::Q, TS::S, TS::Q),
    NewStateTransition(TS::Q, TS::S, TS::Q),
    Inconsistent(TS::Q, TS::S, TS::Q, Inc),
}

#[cfg(test)]
mod tests {
    use crate::sample::Sample;

    pub fn sample_two() -> Sample<&'static str> {
        Sample::from_parts(["a", "ab"], ["", "b", "aa"])
    }
}
