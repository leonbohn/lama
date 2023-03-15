mod walker;

/// Allows the evaluation of a run.
// mod result;
mod configuration;
use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
};

// pub use result::{InitialRun, Run};

pub use walker::Walker;

pub use configuration::{Configuration, Evaluate};

use crate::{
    ts::TransitionSystem,
    words::{IsFinite, Word},
    Equivalent, StateIndex, Subword,
};
use itertools::Itertools;

pub trait InducesIn<TS> {
    type Output;
    fn test(&self) -> bool {
        true
    }
}

impl<W, TS> InducesIn<TS> for W
where
    W: Subword,
    TS: TransitionSystem<S = W::S>,
    Configuration<TS, W>: Evaluate,
{
    type Output = <Configuration<TS, W> as Evaluate>::Output;
}

/// An escape prefix for a transition system is a triple `(u, q, a)`, where `u` is a finite sequence of triggers for the transition system, `q` is a state of the transition system and `a` is a symbol such that:
/// - the last trigger in `u` brings the transition system into the state `q`
/// - no transition is defined for the symbol `a` in the state `q`.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct EscapePrefix<Q, W: Subword> {
    /// The prefix on which a run was possible, consists of state symbol pairs.
    pub prefix: Vec<(Q, W::S)>,
    /// The state at which the transition system is left.
    pub state: Q,
    /// The symbol on which the transition system is left.
    pub symbol: W::S,
    /// The remaining suffix of the word.
    pub suffix: W::SuffixType,
}

impl<Q, W> PartialOrd for EscapePrefix<Q, W>
where
    Q: StateIndex,
    W: Subword,
    W::S: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.prefix.len().partial_cmp(&other.prefix.len()) {
            Some(Ordering::Less) => Some(Ordering::Less),
            Some(Ordering::Greater) => Some(Ordering::Greater),
            _ => self
                .prefix
                .iter()
                .zip(other.prefix.iter())
                .find_map(|((_, a), (_, b))| {
                    let compared = a.partial_cmp(b);
                    if compared.is_none() || compared.unwrap() == Ordering::Equal {
                        None
                    } else {
                        compared
                    }
                }),
        }
    }
}

impl<Q, W> Ord for EscapePrefix<Q, W>
where
    Q: StateIndex,
    W: Subword,
    W::S: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        match self.partial_cmp(other) {
            Some(o) => o,
            None => unreachable!("This must be a total order"),
        }
    }
}

impl<Q: Eq, W: Subword> Equivalent for EscapePrefix<Q, W> {
    fn equivalent(&self, other: &Self) -> bool {
        self.state == other.state && self.symbol == other.symbol && self.suffix == other.suffix
    }
}

impl<Q: StateIndex, W: Word + Subword> EscapePrefix<Q, W> {
    /// Creates a new escape prefix from the given prefix, state and symbol.
    pub fn new(word: &W, prefix: Vec<(Q, W::S)>, state: Q, symbol: W::S) -> Self {
        let length = prefix.len();
        Self {
            prefix,
            state,
            symbol,
            suffix: word.skip(length),
        }
    }

    /// Helper function for converting a finite escape prefix into an infinite one.
    pub fn from_finite<F: Subword + IsFinite<S = W::S>>(
        word: &W,
        escape_prefix: EscapePrefix<Q, F>,
    ) -> Self {
        let length = escape_prefix.prefix.len();
        Self {
            prefix: escape_prefix.prefix,
            state: escape_prefix.state,
            symbol: escape_prefix.symbol,
            suffix: word.skip(length),
        }
    }

    /// Creates an owned trigger.
    pub fn trigger(&self) -> (Q, W::S) {
        (self.state.clone(), self.symbol.clone())
    }
}

impl<Q: Debug, W: Subword> Debug for EscapePrefix<Q, W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} then {:?} misses {} with rest {:?}",
            self.prefix
                .iter()
                .map(|(q, s)| format!("({:?},{:?})", q, s))
                .join(""),
            self.state,
            self.symbol,
            self.suffix
        )
    }
}

impl<Q: Display, W: Subword + Display> Display for EscapePrefix<Q, W>
where
    <W as Subword>::SuffixType: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} then {} misses {} with rest {}",
            self.prefix
                .iter()
                .map(|(q, s)| format!("({},{})", q, s))
                .join(""),
            self.state,
            self.symbol,
            self.suffix
        )
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

impl<Q: Display, S: Display> Display for RunOutput<Q, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                RunOutput::Trigger(q, s) => format!("trigger({},{})", q, s),
                RunOutput::WordEnd(q) => format!("end({})", q),
                RunOutput::Missing(q, s) => format!("missing({},{})", q, s),
                RunOutput::FailedBefore => "failed before".to_string(),
            }
        )
    }
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
        ts::{deterministic::Deterministic, AnonymousGrowable, Growable},
        words::Str,
    };

    use super::*;

    #[test]
    fn basic_run() {
        let mut ts: Deterministic<u32> = Deterministic::new();
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
        assert_eq!(ts.run_word_from(&w, q0).evaluate(), Ok(q1));
    }

    #[test]
    fn basic_run_with_missing() {
        let mut ts: Deterministic<u32> = Deterministic::new();
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
            assert_eq!(run.next(), Some(RunOutput::trigger(q0, 'a')));
            assert_eq!(run.next(), Some(RunOutput::trigger(q1, 'b')));
            assert_eq!(run.next(), Some(RunOutput::trigger(q0, 'a')));
            assert_eq!(run.next(), Some(RunOutput::trigger(q1, 'a')));
            assert_eq!(run.next(), Some(RunOutput::missing(q2, 'a')));
        }

        ts.add_transition(&q2, 'a', &q0);
        assert_eq!(ts.run_word_from(&w, q0).evaluate(), Ok(q0));
    }

    #[test]
    fn input_to_run() {
        let mut ts: Deterministic<u32> = Deterministic::new();
        let q0 = ts.add_new_state();
        let q1 = ts.add_new_state();
        let q2 = ts.add_new_state();
        ts.add_transition(&q0, 'a', &q1);
        ts.add_transition(&q0, 'b', &q0);
        ts.add_transition(&q1, 'a', &q2);
        ts.add_transition(&q1, 'b', &q0);
        ts.add_transition(&q2, 'b', &q0);

        assert_eq!(ts.run_word_from("abba", q0).evaluate(), Ok(q1));
        assert_eq!(ts.run_word_from("abba", q0).evaluate(), Ok(q0));
    }
}
