use crate::{ts::TransitionSystem, words::Word};

use super::RunOutput;

/// Allows to iterate over the individual events that occur along a run of a transition system on some input. Stores a reference to a transition system and a word which serves as input.
/// A `Walker` keeps track of the current state and position in the word as well as the sequence of states produces so far.
#[derive(Clone, Debug)]
pub struct Walker<'ts, 'w, W: Word, TS: TransitionSystem<Input = W::S>> {
    pub(crate) word: &'w W,
    pub(crate) ts: &'ts TS,
    pub(crate) state: Option<TS::State>,
    pub(crate) position: usize,
    pub(crate) seq: Vec<(TS::State, TS::Input)>,
}

impl<'t, 'w, W: Word, TS: TransitionSystem<Input = W::S>> Iterator for Walker<'t, 'w, W, TS> {
    type Item = RunOutput<TS::State, TS::Input>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.take_transition() {
            RunOutput::FailedBefore => None,
            otherwise => Some(otherwise),
        }
    }
}

impl<'t, 'w, W: Word, TS: TransitionSystem<Input = W::S>> Walker<'t, 'w, W, TS> {
    /// Creates a new `Walker` with the given transition system, word and initial state.
    pub fn new<I: Into<&'w W>>(ts: &'t TS, word: I, from: TS::State) -> Self {
        Self {
            word: word.into(),
            ts,
            state: Some(from),
            position: 0,
            seq: vec![],
        }
    }

    /// Takes a single transition, returning the corresponding [`RunOutput`].
    pub fn take_transition(&mut self) -> RunOutput<TS::State, TS::Input> {
        if let Some(state) = self.state.clone() {
            if let Some(symbol) = self.word.nth(self.position) {
                if let Some(successor) = self.ts.succ(&state, &symbol) {
                    self.position += 1;
                    self.state = Some(successor);
                    self.seq.push((state.clone(), symbol.clone()));
                    RunOutput::trigger(state, symbol)
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
    pub fn try_take_n(&mut self, n: usize) -> Result<TS::State, RunOutput<TS::State, TS::Input>> {
        for _ in 1..n {
            self.take_transition();
        }
        match self.take_transition() {
            RunOutput::Trigger(q, _) => Ok(q),
            otherwise => Err(otherwise),
        }
    }
}
