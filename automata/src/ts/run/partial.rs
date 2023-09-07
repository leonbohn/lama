use crate::{
    alphabet::{Alphabet, Symbol},
    ts::{EdgeColor, Path, StateColor},
    word::OmegaWord,
    Color, FiniteLength,
};

use crate::ts::TransitionSystem;

#[derive(Debug, Clone, PartialEq)]
pub struct Partial<'a, 'b, R, Ts: TransitionSystem> {
    word: &'b R,
    ts: &'a Ts,
    position: usize,
    path: Path<Ts::Alphabet, Ts::StateIndex>,
}

impl<'a, 'b, R, Ts: TransitionSystem> Partial<'a, 'b, R, Ts> {
    pub fn path(self) -> Path<Ts::Alphabet, Ts::StateIndex> {
        self.path
    }

    pub fn new(
        word: &'b R,
        ts: &'a Ts,
        position: usize,
        path: Path<Ts::Alphabet, Ts::StateIndex>,
    ) -> Self {
        Self {
            word,
            ts,
            position,
            path,
        }
    }
}
