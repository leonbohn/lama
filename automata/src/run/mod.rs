mod walker;

mod configuration;
mod run;
pub use run::{Evaluate, Run};
mod escape_prefix;
pub use escape_prefix::EscapePrefix;
mod output;
/// Allows the evaluation of a run.
mod result;
use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
};

pub use result::{Induces, InitialRun};

pub use walker::Walker;

pub use configuration::Configuration;

use crate::{ts::Successor, words::Word};

#[derive(Debug, Clone, Eq, PartialEq)]
/// Encapsulates the possible outputs of a run when a symbol is consumed.
pub enum RunOutput<Q, S> {
    /// A transition is taken.
    Transition(Q, S, Q),
    /// The word has ended, returns the reached state.
    WordEnd(Q),
    /// No transition for the given symbol is found, returns the state we are in as well as the missing symbol.
    Missing(Q, S),
    /// The run has failed previously and thus cannot be continued.
    FailedBefore,
}

impl<Q: Display, S: Display> Display for RunOutput<Q, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                RunOutput::Transition(q, s, p) => format!("transition({},{},{})", q, s, p),
                RunOutput::WordEnd(q) => format!("end({})", q),
                RunOutput::Missing(q, s) => format!("missing({},{})", q, s),
                RunOutput::FailedBefore => "failed before".to_string(),
            }
        )
    }
}

impl<Q: Clone, S: Clone> RunOutput<Q, S> {
    pub fn is_transition(&self) -> bool {
        matches!(self, RunOutput::Transition(_, _, _))
    }

    pub fn transition(from: Q, on: S, to: Q) -> Self {
        Self::Transition(from, on, to)
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
    pub fn get_transition(&self) -> Option<(Q, S, Q)> {
        match self {
            RunOutput::Transition(q, a, p) => Some((q.clone(), a.clone(), p.clone())),
            _ => None,
        }
    }
}

/// Abstracts the ability to run a word on a transition system step by step, producing a [`RunOutput`] for each consumed symbol of the input word.
pub trait Walk<'ts, 'w, W: 'w>: Successor + Sized {
    /// The walker type, which is used to iterate over the run, usually a [`Walker`].
    type Walker;

    /// Creates a new [`Self::Walker`] that starts at the given state and consumes the given word.
    fn walk(&'ts self, from: Self::Q, word: &'w W) -> Self::Walker;
}

impl<'ts, 'w, TS: Successor + 'ts, W: Word<S = TS::Sigma> + 'w> Walk<'ts, 'w, W> for TS {
    type Walker = Walker<'ts, 'w, W, TS>;

    fn walk(&'ts self, from: Self::Q, word: &'w W) -> Self::Walker {
        Walker::new(self, word, from)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ts::{transitionsystem::TransitionSystem, AnonymousGrowable, Growable},
        words::Str,
    };

    use super::*;

    #[test]
    fn basic_run() {
        let mut ts: TransitionSystem<u32> = TransitionSystem::new();
        let q0 = ts.add_new_state();
        let q1 = ts.add_new_state();
        let q2 = ts.add_new_state();
        ts.add_transition(&q0, 'a', &q1);
        ts.add_transition(&q0, 'b', &q0);
        ts.add_transition(&q1, 'a', &q2);
        ts.add_transition(&q1, 'b', &q0);
        ts.add_transition(&q2, 'a', &q2);
        ts.add_transition(&q2, 'b', &q0);

        let w = Str::from("abba");
        todo!()
        // assert_eq!(ts.run_word_from(&w, q0).evaluate(), Ok(q1));
    }

    #[test]
    fn basic_run_with_missing() {
        let mut ts: TransitionSystem<u32> = TransitionSystem::new();
        let q0 = ts.add_new_state();
        let q1 = ts.add_new_state();
        let q2 = ts.add_new_state();
        ts.add_transition(&q0, 'a', &q1);
        ts.add_transition(&q0, 'b', &q0);
        ts.add_transition(&q1, 'a', &q2);
        ts.add_transition(&q1, 'b', &q0);
        ts.add_transition(&q2, 'b', &q0);

        let w = Str::from("abaaa");
        {
            let mut run = ts.walk(q0, &w);
            assert_eq!(run.next(), Some(RunOutput::transition(q0, 'a', q1)));
            assert_eq!(run.next(), Some(RunOutput::transition(q1, 'b', q0)));
            assert_eq!(run.next(), Some(RunOutput::transition(q0, 'a', q1)));
            assert_eq!(run.next(), Some(RunOutput::transition(q1, 'a', q2)));
            assert_eq!(run.next(), Some(RunOutput::missing(q2, 'a')));
        }

        ts.add_transition(&q2, 'a', &q0);
        todo!()
        // assert_eq!(ts.run_word_from(&w, q0).evaluate(), Ok(q0));
    }

    #[test]
    fn input_to_run() {
        let mut ts: TransitionSystem<u32> = TransitionSystem::new();
        let q0 = ts.add_new_state();
        let q1 = ts.add_new_state();
        let q2 = ts.add_new_state();
        ts.add_transition(&q0, 'a', &q1);
        ts.add_transition(&q0, 'b', &q0);
        ts.add_transition(&q1, 'a', &q2);
        ts.add_transition(&q1, 'b', &q0);
        ts.add_transition(&q2, 'b', &q0);

        todo!()
        // assert_eq!(ts.run_word_from("abba", q0).evaluate(), Ok(q1));
    }
}
