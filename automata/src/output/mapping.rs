use std::{borrow::Borrow, hash::Hash};

use itertools::Itertools;

use crate::{Map, Priority, Set, Symbol};

use super::with_output::HasOutput;

/// A mapping from a type `X` to a [`Priority`].
pub trait Mapping<I> {
    type Range;

    fn apply<R: Borrow<I>>(&self, input: R) -> Self::Range;
}

/// A mutable mapping from a type `X` to a [`Priority`].
pub trait MutableMapping<I>: Mapping<I> {
    /// Sets the priority of the given element to the given priority.
    fn set_map<R: Borrow<I>>(&mut self, of: R, to: Self::Range) -> Option<Self::Range>;
}

impl<I, O: Symbol> HasOutput for Map<I, O> {
    type Gamma = O;

    type Output<'me> = std::collections::hash_map::Values<'me, I, O> where Self:'me;

    fn output_alphabet_iter(&self) -> Self::Output<'_> {
        self.values()
    }
}

impl<X, Y> Mapping<X> for Map<X, Y>
where
    X: Eq + Hash,
    Y: Clone,
{
    type Range = Y;

    fn apply<R: Borrow<X>>(&self, input: R) -> Self::Range {
        self.get(input.borrow())
            .cloned()
            .expect("Mapping must be total")
    }
}
