use std::hash::Hash;

use automata::Set;

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
}
