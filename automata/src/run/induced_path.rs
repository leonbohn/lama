use std::fmt::Display;

use impl_tools::autoimpl;
use itertools::Itertools;

use crate::{
    ts::Path,
    words::{FiniteLength, InfiniteLength, Length},
    Set, State, Symbol,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct InducedPath<Q, S, L> {
    path: Path<Q, S>,
    length: L,
}

impl<Q: State, S: Symbol, L> InducedPath<Q, S, L> {
    pub fn new(path: Path<Q, S>, length: L) -> Self {
        Self { path, length }
    }

    pub fn induces(self) -> L::Induces<Q, S>
    where
        L: Length,
    {
        L::Induces::from(self)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[autoimpl(Deref using self.0)]
pub struct ReachedState<Q>(Q);

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

#[derive(Debug, Clone, Eq, PartialEq)]
#[autoimpl(Deref using self.0)]
pub struct InfinitySet<Q: State, S: Symbol>(Set<(Q, S)>);

impl<Q, S> From<InducedPath<Q, S, InfiniteLength>> for InfinitySet<Q, S>
where
    Q: State,
    S: Symbol,
{
    fn from(value: InducedPath<Q, S, InfiniteLength>) -> Self {
        InfinitySet(
            value
                .path
                .into_triggers()
                .into_iter()
                .skip(value.length.loop_index())
                .collect(),
        )
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
