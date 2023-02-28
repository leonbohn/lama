use crate::{
    ts::{SymbolFor, TransitionSystem},
    words::Word,
};

use super::RunOutput;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EscapePrefix<TS: TransitionSystem>(pub Vec<TS::Trigger>, pub TS::Q, pub SymbolFor<TS>);

impl<TS: TransitionSystem> EscapePrefix<TS> {
    pub fn new(prefix: Vec<TS::Trigger>, state: TS::Q, symbol: TS::S) -> Self {
        Self(prefix, state, symbol)
    }
}

/// Allows to iterate over the individual events that occur along a run of a transition system on some input. Stores a reference to a transition system and a word which serves as input.
/// A `Walker` keeps track of the current state and position in the word as well as the sequence of states produces so far.
#[derive(Clone, Debug)]
pub struct Walker<'ts, 'w, W: Word, TS: TransitionSystem<S = W::S>> {
    pub(crate) word: &'w W,
    pub(crate) ts: &'ts TS,
    pub(crate) state: Option<TS::Q>,
    pub(crate) position: usize,
    pub(crate) seq: Vec<TS::Q>,
}

impl<'t, 'w, W: Word, TS: TransitionSystem<S = W::S>> Iterator for Walker<'t, 'w, W, TS> {
    type Item = RunOutput<TS>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.take_transition() {
            RunOutput::FailedBefore => None,
            otherwise => Some(otherwise),
        }
    }
}

impl<'t, 'w, W: Word, TS: TransitionSystem<S = W::S>> Walker<'t, 'w, W, TS> {
    /// Creates a new `Walker` with the given transition system, word and initial state.
    pub fn new<I: Into<&'w W>>(ts: &'t TS, word: I, from: TS::Q) -> Self {
        Self {
            word: word.into(),
            ts,
            state: Some(from.clone()),
            position: 0,
            seq: vec![from],
        }
    }

    /// Takes a single transition, returning the corresponding [`RunOutput`].
    pub fn take_transition(&mut self) -> RunOutput<TS> {
        if let Some(state) = self.state.clone() {
            if let Some(symbol) = self.word.nth(self.position) {
                if let Some(successor) = self.ts.succ(&state, &symbol) {
                    self.position += 1;
                    self.state = Some(successor.clone());
                    self.seq.push(successor);
                    RunOutput::Trigger((state, symbol).into())
                } else {
                    RunOutput::Missing(state, symbol)
                }
            } else {
                RunOutput::WordEnd(state)
            }
        } else {
            RunOutput::FailedBefore
        }
    }
}
