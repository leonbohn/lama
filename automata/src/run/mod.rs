mod escaping;
mod take_transition;
pub use take_transition::TakeTransition;
mod walker;

use std::collections::HashSet;

pub use walker::Walker;

use crate::words::{FiniteWord, PeriodicWord, UltimatelyPeriodicWord, Word};

use crate::ts::{Pointed, SymbolFor, TransitionSystem};

pub enum RunOutput<TS: TransitionSystem> {
    Trigger(TS::Trigger),
    WordEnd(TS::Q),
    Missing(TS::Q, SymbolFor<TS>),
    FailedBefore,
}

impl<TS: TransitionSystem> RunOutput<TS> {
    pub fn is_trigger(&self) -> bool {
        matches!(self, RunOutput::Trigger(_))
    }
}

impl<TS: TransitionSystem> RunOutput<TS> {
    pub fn trigger(&self) -> Option<&TS::Trigger> {
        match self {
            RunOutput::Trigger(t) => Some(t),
            _ => None,
        }
    }
}

pub trait Walk<'ts, 'w, W: Word>: TransitionSystem<S = W::S> + Sized {
    type Walker;

    fn walk(&'ts self, from: Self::Q, word: &'w W) -> Self::Walker;
}

impl<'t, 'w, W: Word + 'w, TS: TransitionSystem<S = W::S> + 't> Walk<'t, 'w, W> for TS {
    type Walker = Walker<'t, 'w, W, TS>;

    fn walk(&'t self, from: Self::Q, word: &'w W) -> Self::Walker {
        Walker {
            word,
            ts: self,
            state: Some(from.clone()),
            position: 0,
            seq: vec![from],
        }
    }
}

pub trait Runnable<TS: TransitionSystem<S = Self::S>>: Sized + Word {
    type Induces;

    fn run<'w, 'ts>(&'w self, word: &'ts TS) -> Walker<'ts, 'w, Self, TS>;
}

impl<TS: TransitionSystem + Pointed> Runnable<TS> for FiniteWord<TS::S> {
    type Induces = TS::Q;

    fn run<'w, 'ts>(&'w self, ts: &'ts TS) -> Walker<'ts, 'w, Self, TS> {
        Walker {
            ts,
            state: Some(ts.initial()),
            position: 0,
            seq: vec![ts.initial()],
            word: self,
        }
    }
}

impl<TS: TransitionSystem + Pointed> Runnable<TS> for PeriodicWord<TS::S> {
    type Induces = HashSet<TS::Q>;

    fn run<'w, 'ts>(&'w self, ts: &'ts TS) -> Walker<'ts, 'w, Self, TS> {
        Walker {
            ts,
            state: Some(ts.initial()),
            position: 0,
            seq: vec![ts.initial()],
            word: self,
        }
    }
}

impl<TS: TransitionSystem + Pointed> Runnable<TS> for UltimatelyPeriodicWord<TS::S> {
    type Induces = HashSet<TS::Q>;

    fn run<'w, 'ts>(&'w self, ts: &'ts TS) -> Walker<'ts, 'w, Self, TS> {
        Walker {
            ts,
            state: Some(ts.initial()),
            position: 0,
            seq: vec![ts.initial()],
            word: self,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ts::{deterministic::Deterministic, Growable};

    use super::{walker::RunResult, *};

    #[test]
    fn it_works() {
        let mut ts = Deterministic::new();
        let q0 = ts.add_state();
        let q1 = ts.add_state();
        let q2 = ts.add_state();
        ts.add_transition(q0, 'a', q1);
        ts.add_transition(q0, 'b', q0);
        ts.add_transition(q1, 'a', q2);
        ts.add_transition(q1, 'b', q0);
        ts.add_transition(q2, 'a', q2);
        ts.add_transition(q2, 'b', q0);

        let w = FiniteWord::from("abba");
        assert_eq!(w.run(&ts.with_initial(q0)).result(), Ok(q1));
    }
}
