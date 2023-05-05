use std::borrow::Borrow;

use crate::{Successor, Word};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Run<TS: Successor, W> {
    ts: TS,
    input: W,
    origin: TS::Q,
}

impl<TS: Successor, W: Word<S = TS::Sigma>> Run<TS, W> {
    /// Creates a new run on the given transition system with the given input.
    pub fn new<X: Borrow<TS::Q>>(on: TS, from: X, word: W) -> Self {
        Self {
            ts: on,
            input: word,
            origin: from.borrow().clone(),
        }
    }

    /// Returns the transition system on which the run is performed.
    pub fn ts(&self) -> &TS {
        &self.ts
    }

    /// Returns the input word of the run.
    pub fn input(&self) -> &W {
        &self.input
    }
}
