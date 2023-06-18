use std::{collections::BTreeSet, fmt::Display, hash::Hash};

use impl_tools::autoimpl;
use itertools::Itertools;

use crate::{
    ts::Path,
    words::{FiniteLength, InfiniteLength, Length},
    Set, State, Symbol,
};

/// Endows a [`Path`] with a [`Length`] and a position, which allows turning it into
/// an induced object, by extracting either the reached state or the infinity set.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct InducedPath<Q, S, L> {
    path: Path<Q, S>,
    length: L,
    position: usize,
}

impl<Q: State, S: Symbol, L: Length> InducedPath<Q, S, L> {
    /// Creates a new [`InducedPath`] from the given parameters.
    pub fn new<I: Into<usize>>(path: Path<Q, S>, length: L, position: I) -> Self {
        Self {
            path,
            position: position.into(),
            length,
        }
    }

    /// Computes the induced object by going through the stored [`Length`].
    pub fn induces(self) -> L::Induces<Q, S> {
        L::Induces::from(self)
    }
}

/// This is the type of object that is induced by inputs of [`FiniteLength`].
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[autoimpl(Deref, DerefMut using self.0)]
pub struct ReachedState<Q>(pub Q);

impl<Q> ReachedState<Q> {
    /// Consumes the object and returns the inner state.
    pub fn into_inner(self) -> Q {
        self.0
    }
}

impl<Q: State> std::borrow::Borrow<Q> for ReachedState<Q> {
    fn borrow(&self) -> &Q {
        &self.0
    }
}

impl<Q, S> From<InducedPath<Q, S, FiniteLength>> for ReachedState<Q>
where
    Q: State,
    S: Symbol,
{
    fn from(value: InducedPath<Q, S, FiniteLength>) -> Self {
        ReachedState(value.path.reached().clone())
    }
}

impl<Q: Display> Display for ReachedState<Q> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "=>{}", self.0)
    }
}

/// The type of object induced by inputs of [`InfiniteLength`], the set of triggers
/// that are used infinitely often.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
#[autoimpl(Deref using self.0)]
pub struct InfinitySet<Q: State, S: Symbol>(pub BTreeSet<(Q, S)>);

impl<Q: State, S: Symbol> IntoIterator for InfinitySet<Q, S> {
    type Item = (Q, S);
    type IntoIter = std::collections::btree_set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<Q: State, S: Symbol> InfinitySet<Q, S> {
    /// Consumes the infinity set and gives the underlying set
    pub fn into_inner(self) -> BTreeSet<(Q, S)> {
        self.0
    }
}

impl<Q, S> From<InducedPath<Q, S, InfiniteLength>> for InfinitySet<Q, S>
where
    Q: State,
    S: Symbol,
{
    fn from(value: InducedPath<Q, S, InfiniteLength>) -> Self {
        debug_assert!(
            value.position < value.path.len(),
            "This would be a weird loop otherwise"
        );
        println!("{:?}", value);
        InfinitySet(value.path.into_triggers().skip(value.position).collect())
    }
}

impl<Q: State, S: Symbol> Display for InfinitySet<Q, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "=>{{{}}}",
            self.0.iter().map(|(q, a)| format!("({q}, {a})")).join(", ")
        )
    }
}
