mod constraint;
mod state;

use automata::{Deterministic, TransitionSystem};

pub enum GlercOutput<TS: TransitionSystem = Deterministic, Inc = String> {
    Transition(TS::Q, TS::S, TS::Q),
    NewStateTransition(TS::Q, TS::S, TS::Q),
    Inconsistent(TS::Q, TS::S, TS::Q, Inc),
}
