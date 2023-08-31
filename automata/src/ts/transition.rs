use std::{
    fmt::{Debug, Display},
    ops::Deref,
};

use crate::{alphabet::Symbol, Color};

use super::{Idx, Index, IndexType, StateIndex};

/// Wrapper type for indices of edges in a transition system.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct EdgeIndex(Idx);

impl EdgeIndex {
    /// Creates a new edge index.
    pub fn new(index: Idx) -> Self {
        Self(index)
    }
}

impl Deref for EdgeIndex {
    type Target = Idx;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Index for EdgeIndex {
    fn index(&self) -> Idx {
        self.0
    }
}

/// An edge in a [`TransitionSystem`], which stores the source and target state, an
/// associated color as well as a trigger expression (see [`Alphabet`]). Further, it
/// also maintains the indices of the next and previous edge in the list of edges
#[derive(Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct Edge<E, C, Idx> {
    source: Idx,
    target: Idx,
    color: C,
    trigger: E,
    next_edge: Option<EdgeIndex>,
    prev_edge: Option<EdgeIndex>,
}

impl<E, C, Idx> Edge<E, C, Idx> {
    /// Creates a new edge with the given source and target state, color and trigger expression.
    pub fn new(source: Idx, target: Idx, color: C, trigger: E) -> Self {
        Self {
            source,
            target,
            color,
            trigger,
            next_edge: None,
            prev_edge: None,
        }
    }

    pub fn recolor<D: Color>(self, color: D) -> Edge<E, D, Idx> {
        Edge {
            source: self.source,
            target: self.target,
            color,
            trigger: self.trigger,
            next_edge: self.next_edge,
            prev_edge: self.prev_edge,
        }
    }

    /// Sets the previous edge index.
    pub fn set_prev(&mut self, prev: EdgeIndex) {
        self.prev_edge = Some(prev);
    }

    /// Moves out of `self`, creating a new `Edge` with the given previous edge index.
    pub fn with_prev(self, prev: EdgeIndex) -> Self {
        Self {
            prev_edge: Some(prev),
            ..self
        }
    }

    /// Returns the next edge index.
    pub fn get_next(&self) -> Option<EdgeIndex> {
        self.next_edge
    }

    /// Returns the previous edge index.
    pub fn get_prev(&self) -> Option<EdgeIndex> {
        self.prev_edge
    }

    pub fn clear_next(&mut self) {
        self.next_edge = None
    }

    /// Sets the next edge index.
    pub fn set_next(&mut self, next: EdgeIndex) {
        self.next_edge = Some(next);
    }

    /// Moves out of `self`, creating a new `Edge` with the given next edge index.
    pub fn with_next(self, next: EdgeIndex) -> Self {
        Self {
            next_edge: Some(next),
            ..self
        }
    }

    /// Gets the source state index.
    pub fn source(&self) -> Idx
    where
        Idx: IndexType,
    {
        self.source
    }

    /// Gets the target state index.
    pub fn target(&self) -> Idx
    where
        Idx: IndexType,
    {
        self.target
    }

    /// Gets the color.
    pub fn color(&self) -> &C {
        &self.color
    }

    /// Gets the trigger expression.
    pub fn trigger(&self) -> &E {
        &self.trigger
    }
}

/// Struct which stores a reference to the list of edges and an index to the next edge,
/// to allow iterating over the edge list.
#[derive(Debug, Clone)]
pub struct EdgesFrom<'a, E, C, Idx> {
    edges: &'a [Edge<E, C, Idx>],
    next: Option<EdgeIndex>,
}

impl<'a, E, C, Idx> Iterator for EdgesFrom<'a, E, C, Idx> {
    type Item = &'a Edge<E, C, Idx>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_id = self.next?;
        if let Some(next_edge) = self.edges.get(next_id.index()) {
            self.next = next_edge.next_edge;
            Some(next_edge)
        } else {
            None
        }
    }
}

impl<'a, E, C, Idx> EdgesFrom<'a, E, C, Idx> {
    /// Creates a new `EdgesFrom` from the given slice of edges and the next edge index.
    pub fn new(edges: &'a [Edge<E, C, Idx>], next: Option<EdgeIndex>) -> Self {
        Self { edges, next }
    }
}

/// Struct which allows iterating over all edges originating in a given state.
#[derive(Debug, Clone)]
pub struct EdgeIndicesFrom<'a, E, C, Idx> {
    edges: &'a [Edge<E, C, Idx>],
    next: Option<EdgeIndex>,
}

impl<'a, E, C, Idx> Iterator for EdgeIndicesFrom<'a, E, C, Idx> {
    type Item = EdgeIndex;
    fn next(&mut self) -> Option<Self::Item> {
        let next_id = self.next?;
        if let Some(next_edge) = self.edges.get(next_id.index()) {
            self.next = next_edge.next_edge;
            Some(next_id)
        } else {
            None
        }
    }
}

/// A transition is a concrete instantiation of a [`Edge`] in a [`TransitionSystem`].
/// While edges can be labeled with arbitrary expressions, transitions are labeled with
/// concrete symbols from the alphabet.
///
/// As an example, we may have an edge labeled with a boolean expression over some
/// atomic propositions, e.g. `a | b`. This edge can be instantiated with the symbols
/// `a & b`, `a & !b` and `!a & b`, which are all concrete symbols from the alphabet
/// that match the expression.
#[derive(Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Transition<Idx, S, C> {
    source: Idx,
    target: Idx,
    symbol: S,
    emits: C,
}

impl<Idx, S: Clone, C: Color> Transition<Idx, S, C> {
    /// Creates a new transition with the given source and target state, symbol and color.
    pub fn new(source: Idx, symbol: S, target: Idx, color: C) -> Self {
        Self {
            source,
            target,
            symbol,
            emits: color,
        }
    }

    pub fn with_color<D: Color>(self, color: D) -> Transition<Idx, S, D> {
        Transition {
            source: self.source,
            target: self.target,
            symbol: self.symbol,
            emits: color,
        }
    }

    /// Returns the source state index.
    pub fn source(&self) -> Idx
    where
        Idx: IndexType,
    {
        self.source
    }

    /// Returns the symbol of the source state.
    pub fn symbol(&self) -> S {
        self.symbol.clone()
    }

    /// Returns the color of the transition.
    pub fn color(&self) -> &C {
        &self.emits
    }

    /// Returns the target state index.
    pub fn target(&self) -> Idx
    where
        Idx: IndexType,
    {
        self.target
    }
}

impl<Idx: Display, S: Display, C: Display> Display for Transition<Idx, S, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} --{}:{}--> {}",
            self.source, self.symbol, self.emits, self.target
        )
    }
}

impl<Idx: Debug, S: Debug, C: Debug> Debug for Transition<Idx, S, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} --{:?}:{:?}--> {:?}",
            self.source, self.symbol, self.emits, self.target
        )
    }
}
