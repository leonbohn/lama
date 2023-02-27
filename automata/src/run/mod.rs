mod escaping;
mod take_transition;
pub use take_transition::TakeTransition;
mod walker;

/// Allows the evaluation of a run.
pub mod result;
pub use result::RunResult;

pub use walker::Walker;

use crate::words::Word;

use crate::ts::{SymbolFor, TransitionSystem};

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
}

impl<TS: TransitionSystem> RunOutput<TS> {
    /// Returns the trigger if `self` is of type `RunOutput::Trigger` and `None` otherwise.
    pub fn get_trigger(&self) -> Option<&TS::Trigger> {
        match self {
            RunOutput::Trigger(t) => Some(t),
            _ => None,
        }
    }
}

/// Abstracts the ability to run a word on a transition system step by step, producing a [`RunOutput`] for each consumed symbol of the input word. See also [`WalkIn`].
pub trait Walk<'ts, 'w, W: Word>: TransitionSystem<S = W::S> + Sized {
    /// The walker type, which is used to iterate over the run, usually a [`Walker`].
    type Walker;

    /// Creates a new [`Self::Walker`] that starts at the given state and consumes the given word.
    fn walk_from_on(&'ts self, from: Self::Q, word: &'w W) -> Self::Walker;
}

impl<'t, 'w, W: Word + 'w, TS: TransitionSystem<S = W::S> + 't> Walk<'t, 'w, W> for TS {
    type Walker = Walker<'t, 'w, W, TS>;

    fn walk_from_on(&'t self, from: Self::Q, word: &'w W) -> Self::Walker {
        Walker {
            word,
            ts: self,
            state: Some(from.clone()),
            position: 0,
            seq: vec![from],
        }
    }
}

/// Abstracts the ability to run a word on a transition system in a similar fashion to the [`Walk`] trait, but instead of being called on the transition system, it is called on the word. Further, this trait assumes that the given transition system is pointed, i.e. has an initial state.
pub trait WalkIn<'ts, 'w, TS: TransitionSystem<S = Self::S>>: Sized + Word {
    /// The walker type, which is used to iterate over the run, usually a [`Walker`].
    type Walker;

    /// Creates a new [`Self::Walker`] that starts at the given state and consumes the given word.
    fn walk_in_from(&'w self, ts: &'ts TS, from: TS::Q) -> Self::Walker;
}

impl<'ts, 'w, TS: TransitionSystem + 'ts, W: Word + 'w> WalkIn<'ts, 'w, TS> for W
where
    TS: Walk<'ts, 'w, W>,
{
    type Walker = TS::Walker;

    fn walk_in_from(&'w self, ts: &'ts TS, from: TS::Q) -> Self::Walker {
        ts.walk_from_on(from, self)
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
        assert_eq!(w.walk_in_from(&ts, q0).result(), Ok(q1));
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
            let mut run = w.walk_in_from(&ts, q0);
            assert_eq!(run.next(), Some(RunOutput::trigger(q0, 'a')));
            assert_eq!(run.next(), Some(RunOutput::trigger(q1, 'b')));
            assert_eq!(run.next(), Some(RunOutput::trigger(q0, 'a')));
            assert_eq!(run.next(), Some(RunOutput::trigger(q1, 'a')));
            assert_eq!(run.next(), Some(RunOutput::missing(q2, 'a')));
        }

        ts.add_transition(q2, 'a', q0);
        assert_eq!(w.walk_in_from(&ts, q0).result(), Ok(q0));
    }
}
