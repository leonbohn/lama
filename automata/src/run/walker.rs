use crate::{
    ts::{Path, StateOf, Successor, TransitionOf},
    words::Word,
};

use super::RunOutput;

/// Allows to iterate over the individual events that occur along a run of a transition system on some input. Stores a reference to a transition system and a word which serves as input.
/// A `Walker` keeps track of the current state and position in the word as well as the sequence of states produces so far.
#[derive(Clone, Debug)]
pub struct Walker<'ts, 'w, W: Word, TS: Successor<Sigma = W::S>> {
    pub(crate) word: &'w W,
    pub(crate) ts: &'ts TS,
    pub(crate) state: Option<StateOf<TS>>,
    pub(crate) position: usize,
    pub(crate) seq: Vec<TransitionOf<TS>>,
}

impl<'t, 'w, W: Word, TS: Successor<Sigma = W::S>> Iterator for Walker<'t, 'w, W, TS> {
    type Item = RunOutput<StateOf<TS>, TS::Sigma>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.take_transition() {
            RunOutput::FailedBefore => None,
            otherwise => Some(otherwise),
        }
    }
}

impl<'t, 'w, W: Word, TS: Successor<Sigma = W::S>> Walker<'t, 'w, W, TS> {
    /// Creates a new `Walker` with the given transition system, word and initial state.
    pub fn new<I: Into<&'w W>>(ts: &'t TS, word: I, from: StateOf<TS>) -> Self {
        Self {
            word: word.into(),
            ts,
            state: Some(from),
            position: 0,
            seq: vec![],
        }
    }

    /// Returns the [`Path`] that was taken thus far. If no transitions were taken yet,
    /// this path consists of only the source vertex.
    pub fn taken_path(&self) -> Path<TS::Q, W::S> {
        if self.seq.is_empty() {
            Path::empty(
                self.state
                    .clone()
                    .expect("Must exist if no transition was taken yet!"),
            )
        } else {
            Path::from(self.seq.clone())
        }
    }

    /// Returns the current position of the `Walker`.
    pub fn position(&self) -> usize {
        self.position
    }

    /// Takes a single transition, returning the corresponding [`RunOutput`].
    pub fn take_transition(&mut self) -> RunOutput<StateOf<TS>, TS::Sigma> {
        if let Some(state) = self.state.clone() {
            if let Some(symbol) = self.word.nth(self.position) {
                if let Some(successor) = self.ts.successor(&state, &symbol) {
                    self.position += 1;
                    self.state = Some(successor.clone());
                    self.seq
                        .push((state.clone(), symbol.clone(), successor.clone()));
                    RunOutput::transition(state, symbol, successor)
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

    /// Try to take `n` transitions. If successful, returns the state reached after the `n` transitions. Otherwise, returns the [`RunOutput`] that caused the failure.
    pub fn try_take_n(
        &mut self,
        n: usize,
    ) -> Result<StateOf<TS>, RunOutput<StateOf<TS>, TS::Sigma>> {
        for _ in 1..n {
            self.take_transition();
        }
        match self.take_transition() {
            RunOutput::Transition(q, _, _) => Ok(q),
            otherwise => Err(otherwise),
        }
    }
}
