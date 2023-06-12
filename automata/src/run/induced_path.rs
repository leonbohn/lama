use std::fmt::Display;

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
    pub fn new(path: Path<Q, S>, length: L, position: usize) -> Self {
        Self {
            path,
            position,
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
#[autoimpl(Deref using self.0)]
pub struct ReachedState<Q>(pub Q);

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
#[derive(Debug, Clone, Eq, PartialEq)]
#[autoimpl(Deref using self.0)]
pub struct InfinitySet<Q: State, S: Symbol>(pub Set<(Q, S)>);

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
