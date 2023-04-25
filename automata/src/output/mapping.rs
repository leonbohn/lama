use std::{borrow::Borrow, hash::Hash};

use itertools::Itertools;

use crate::{Map, Priority, Set, Symbol};

use super::with_output::HasOutput;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Mapping<X: Hash + Eq, Y> {
    map: Map<X, Y>,
}

impl<X, Y> Mapping<X, Y>
where
    X: Hash + Eq,
{
    pub fn assigments(&self) -> impl Iterator<Item = (&X, &Y)> {
        self.map.iter()
    }
}

/// A mapping from a type `X` to a [`Priority`].
pub trait Transformer<I> {
    type Range;

    fn apply<R: Borrow<I>>(&self, input: R) -> Self::Range;
}

/// A mutable mapping from a type `X` to a [`Priority`].
pub trait MutableMapping<I>: Transformer<I> {
    /// Sets the priority of the given element to the given priority.
    fn set_map<R: Borrow<I>>(&mut self, of: R, to: Self::Range) -> Option<Self::Range>;
}

pub trait Assignment {
    type Left;
    type Right;

    fn assignee(&self) -> &Self::Left;
    fn assignment(&self) -> &Self::Right;
}

impl<L, R> Assignment for (L, R) {
    type Left = L;

    type Right = R;

    fn assignee(&self) -> &Self::Left {
        &self.0
    }

    fn assignment(&self) -> &Self::Right {
        &self.1
    }
}

pub trait IntoAssigments {
    type AssigmentRef: Assignment;
    type Assignments: IntoIterator<Item = Self::AssigmentRef>;

    fn into_assignments(self) -> Self::Assignments;
}

impl<'a, X, Y> IntoAssigments for &'a Mapping<X, Y>
where
    X: Clone + Eq + Hash,
    Y: Clone,
{
    type AssigmentRef = (&'a X, &'a Y);
    type Assignments = std::collections::hash_map::Iter<'a, X, Y>;

    fn into_assignments(self) -> Self::Assignments {
        self.map.iter()
    }
}

impl<I, O: Symbol> HasOutput for Map<I, O> {
    type Gamma = O;

    type Output<'me> = std::collections::hash_map::Values<'me, I, O> where Self:'me;

    fn output_alphabet_iter(&self) -> Self::Output<'_> {
        self.values()
    }
}

impl<X, Y> Transformer<X> for Map<X, Y>
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
