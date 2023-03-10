mod walker;

/// Allows the evaluation of a run.
mod result;
pub use result::Run;

pub use walker::Walker;

use crate::{ts::TransitionSystem, words::Word};

/// An escape prefix for a transition system is a triple `(u, q, a)`, where `u` is a finite sequence of triggers for the transition system, `q` is a state of the transition system and `a` is a symbol such that:
/// - the last trigger in `u` brings the transition system into the state `q`
/// - no transition is defined for the symbol `a` in the state `q`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EscapePrefix<Q, S>(pub Vec<(Q, S)>, pub Q, pub S);

impl<Q, S> EscapePrefix<Q, S> {
    /// Creates a new escape prefix from the given prefix, state and symbol.
    pub fn new(prefix: Vec<(Q, S)>, state: Q, symbol: S) -> Self {
        Self(prefix, state, symbol)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
/// Encapsulates the possible outputs of a run when a symbol is consumed.
pub enum RunOutput<Q, S> {
    /// A transition is taken, gives the trigger.
    Trigger(Q, S),
    /// The word has ended, returns the reached state.
    WordEnd(Q),
    /// No transition for the given symbol is found, returns the state we are in as well as the missing symbol.
    Missing(Q, S),
    /// The run has failed previously and thus cannot be continued.
    FailedBefore,
}

impl<Q: Clone, S: Clone> RunOutput<Q, S> {
    /// Returns true iff the run output is a trigger.
    pub fn is_trigger(&self) -> bool {
        matches!(self, RunOutput::Trigger(_, _))
    }

    /// Creates a new `RunOutput::Trigger` from the given state symbol pair.
    pub fn trigger(from: Q, on: S) -> Self {
        Self::Trigger(from, on)
    }

    /// Creates a new `RunOutput::WordEnd` with the given reached state.
    pub fn end(state: Q) -> Self {
        Self::WordEnd(state)
    }

    /// Creates a new `RunOutput::Missing` with the given state and missing symbol.
    pub fn missing(state: Q, missing: S) -> Self {
        Self::Missing(state, missing)
    }

    /// Returns the trigger if `self` is of type `RunOutput::Trigger` and `None` otherwise.
    pub fn get_trigger(&self) -> Option<(Q, S)> {
        match self {
            RunOutput::Trigger(q, a) => Some((q.clone(), a.clone())),
            _ => None,
        }
    }
}

/// Abstracts the ability to run a word on a transition system step by step, producing a [`RunOutput`] for each consumed symbol of the input word.
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
