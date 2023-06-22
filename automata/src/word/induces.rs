use std::collections::BTreeSet;

use crate::{alphabet::Symbol, FiniteLength, InfiniteLength, Length, Word};

pub trait Induces {
    type Induced<S: Symbol>;

    fn induces<W: Word>(&self, word: &W) -> Self::Induced<W::Symbol>;
}

impl Induces for FiniteLength {
    type Induced<S: Symbol> = S;

    fn induces<W: Word>(&self, word: &W) -> Self::Induced<W::Symbol> {
        self.last_position()
            .and_then(|pos| word.get(pos))
            .expect("We assume that the word is non-empty")
    }
}

pub struct InfinitySet<S: Symbol>(BTreeSet<S>);

impl<S: Symbol> FromIterator<S> for InfinitySet<S> {
    fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl Induces for InfiniteLength {
    type Induced<S: Symbol> = InfinitySet<S>;

    fn induces<W: Word>(&self, word: &W) -> Self::Induced<W::Symbol> {
        self.loop_positions()
            .map(|pos| word.get(pos).expect("Infinite word, so this must exist!"))
            .collect()
    }
}
