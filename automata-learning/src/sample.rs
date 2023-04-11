use std::hash::Hash;

use automata::{FiniteKind, RightCongruence, Set, Word};

#[derive(Debug, Clone)]
pub struct Sample<W> {
    pub positive: Set<W>,
    pub negative: Set<W>,
}

impl<W: Eq + Hash> Sample<W> {
    pub fn new(positive: Set<W>, negative: Set<W>) -> Self {
        Self { positive, negative }
    }

    pub fn from_parts<I, J>(positive: I, negative: J) -> Self
    where
        I: IntoIterator<Item = W>,
        J: IntoIterator<Item = W>,
    {
        Self {
            positive: positive.into_iter().collect(),
            negative: negative.into_iter().collect(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &W> {
        self.positive.iter().chain(self.negative.iter())
    }

    pub fn positive_iter(&self) -> impl Iterator<Item = &W> {
        self.positive.iter()
    }

    pub fn negative_iter(&self) -> impl Iterator<Item = &W> {
        self.negative.iter()
    }
}

impl<W: Word<Kind = FiniteKind>> Sample<W> {
    pub fn prefix_tree(&self) -> RightCongruence<W::S> {
        todo!()
    }
}
