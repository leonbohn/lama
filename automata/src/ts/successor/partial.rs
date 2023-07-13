use crate::{
    alphabet::{Alphabet, Symbol},
    ts::Path,
    word::RawWithLength,
    Color, FiniteLength,
};

use super::Successor;

#[derive(Debug, Clone, PartialEq)]
pub struct Partial<'a, 'b, R, Ts: Successor> {
    word: &'b R,
    ts: &'a Ts,
    position: usize,
    path: Path<'a, Ts::Alphabet, Ts::Index, Ts::StateColor, Ts::EdgeColor>,
}

impl<'a, 'b, R, Ts: Successor> Partial<'a, 'b, R, Ts> {
    pub fn new(
        word: &'b R,
        ts: &'a Ts,
        position: usize,
        path: Path<'a, Ts::Alphabet, Ts::Index, Ts::StateColor, Ts::EdgeColor>,
    ) -> Self {
        Self {
            word,
            ts,
            position,
            path,
        }
    }
}
