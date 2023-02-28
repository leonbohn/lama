mod escaping;
mod walker;

/// Allows the evaluation of a run.
pub mod result;
pub use result::Run;

pub use walker::Walker;

use crate::{
    ts::{SymbolFor, TransitionSystem},
    words::Word,
};

#[derive(Debug, Clone, Eq, PartialEq)]
/// Encapsulates the possible outputs of a run when a symbol is consumed.
pub enum RunOutput<TS: TransitionSystem> {
    /// A transition is taken, gives the trigger.
    Trigger(TS::Trigger),
    /// The word has ended, returns the reached state.
    WordEnd(TS::Q),
    /// No transition for the given symbol is found, returns the state we are in as well as the missing symbol.
    Missing(TS::Q, SymbolFor<TS>),
    /// The run has failed previously and thus cannot be continued.
    FailedBefore,
}

impl<TS: TransitionSystem> RunOutput<TS> {
    /// Returns true iff the run output is a trigger.
    pub fn is_trigger(&self) -> bool {
        matches!(self, RunOutput::Trigger(_))
    }

    /// Creates a new `RunOutput::Trigger` from the given state symbol pair.
    pub fn trigger(from: TS::Q, on: TS::S) -> Self {
        Self::Trigger((from, on).into())
    }

    /// Creates a new `RunOutput::WordEnd` with the given reached state.
    pub fn end(state: TS::Q) -> Self {
        Self::WordEnd(state)
    }

    /// Creates a new `RunOutput::Missing` with the given state and missing symbol.
    pub fn missing(state: TS::Q, missing: SymbolFor<TS>) -> Self {
        Self::Missing(state, missing)
    }

    /// Returns the trigger if `self` is of type `RunOutput::Trigger` and `None` otherwise.
    pub fn get_trigger(&self) -> Option<&TS::Trigger> {
        match self {
            RunOutput::Trigger(t) => Some(t),
            _ => None,
        }
    }
}

/// Abstracts the ability to run a word on a transition system step by step, producing a [`RunOutput`] for each consumed symbol of the input word. See also [`WalkIn`].
pub trait Walk<'ts, 'w, W: 'w>: TransitionSystem + Sized {
    /// The walker type, which is used to iterate over the run, usually a [`Walker`].
    type Walker;

    /// Creates a new [`Self::Walker`] that starts at the given state and consumes the given word.
    fn walk(&'ts self, from: Self::Q, word: &'w W) -> Self::Walker;
}

impl<'ts, 'w, TS: TransitionSystem + 'ts, W: Word<S = TS::S> + 'w> Walk<'ts, 'w, W> for TS {
    type Walker = Walker<'ts, 'w, W, TS>;

    fn walk(&'ts self, from: Self::Q, word: &'w W) -> Self::Walker {
        Walker::new(self, word, from)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ts::{deterministic::Deterministic, Growable},
        words::FiniteWord,
    };

    use super::*;

    #[test]
    fn basic_run() {
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
        assert_eq!(w.run(&ts, q0), Ok(q1));
    }

    #[test]
    fn basic_run_with_missing() {
        let mut ts = Deterministic::new();
        let q0 = ts.add_state();
        let q1 = ts.add_state();
        let q2 = ts.add_state();
        ts.add_transition(q0, 'a', q1);
        ts.add_transition(q0, 'b', q0);
        ts.add_transition(q1, 'a', q2);
        ts.add_transition(q1, 'b', q0);
        ts.add_transition(q2, 'b', q0);

        let w = FiniteWord::from("abaaa");
        {
            let mut run = ts.walk(q0, &w);
            assert_eq!(run.next(), Some(RunOutput::trigger(q0, 'a')));
            assert_eq!(run.next(), Some(RunOutput::trigger(q1, 'b')));
            assert_eq!(run.next(), Some(RunOutput::trigger(q0, 'a')));
            assert_eq!(run.next(), Some(RunOutput::trigger(q1, 'a')));
            assert_eq!(run.next(), Some(RunOutput::missing(q2, 'a')));
        }

        ts.add_transition(q2, 'a', q0);
        assert_eq!(w.run(&ts, q0), Ok(q0));
    }

    #[test]
    fn input_to_run() {
        let mut ts = Deterministic::new();
        let q0 = ts.add_state();
        let q1 = ts.add_state();
        let q2 = ts.add_state();
        ts.add_transition(q0, 'a', q1);
        ts.add_transition(q0, 'b', q0);
        ts.add_transition(q1, 'a', q2);
        ts.add_transition(q1, 'b', q0);
        ts.add_transition(q2, 'b', q0);

        assert_eq!("abba".run(&ts, q0), Ok(q1));
        assert_eq!("abb".run(&ts, q0), Ok(q0));
    }
}
