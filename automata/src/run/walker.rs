use crate::{
    run::TakeTransition,
    ts::{SymbolFor, TransitionSystem},
    words::{FiniteWord, Word},
};

use super::RunOutput;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EscapePrefix<TS: TransitionSystem>(
    pub FiniteWord<SymbolFor<TS>>,
    pub TS::Q,
    pub SymbolFor<TS>,
);

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
