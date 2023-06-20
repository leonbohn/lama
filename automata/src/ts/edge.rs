use std::ops::Deref;

use crate::Alphabet;

use super::{Idx, Index, Indexes, StateIndex, TransitionSystem};

/// Wrapper type for indices of edges in a transition system.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct EdgeIndex(Idx);

impl EdgeIndex {
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

impl<A: Alphabet, Q, C> Indexes<TransitionSystem<A, Q, C>> for EdgeIndex {
    type Ref<'a> = &'a Edge<A::Expression, C> where TransitionSystem<A, Q, C>: 'a, Self: 'a;

    type MutRef<'a> = &'a mut Edge<A::Expression, C> where TransitionSystem<A, Q, C>: 'a, Self: 'a;

    fn get(self, ts: &TransitionSystem<A, Q, C>) -> Option<Self::Ref<'_>> {
        ts.edges.get(self.index())
    }

    fn get_mut(self, ts: &mut TransitionSystem<A, Q, C>) -> Option<Self::MutRef<'_>> {
        ts.edges.get_mut(self.index())
    }
}

/// An edge in a [`TransitionSystem`], which stores the source and target state, an
/// associated color as well as a trigger expression (see [`Alphabet`]). Further, it
/// also maintains the indices of the next and previous edge in the list of edges
#[derive(Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct Edge<E, C> {
    source: StateIndex,
    target: StateIndex,
    color: C,
    trigger: E,
    next_edge: Option<EdgeIndex>,
    prev_edge: Option<EdgeIndex>,
}

impl<E, C> Edge<E, C> {
    /// Creates a new edge with the given source and target state, color and trigger expression.
    pub fn new(source: StateIndex, target: StateIndex, color: C, trigger: E) -> Self {
        Self {
            source,
            target,
            color,
            trigger,
            next_edge: None,
            prev_edge: None,
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
    pub fn source(&self) -> StateIndex {
        self.source
    }

    /// Gets the target state index.
    pub fn target(&self) -> StateIndex {
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
pub struct EdgesFrom<'a, E, C> {
    edges: &'a [Edge<E, C>],
    next: Option<EdgeIndex>,
}

impl<'a, E, C> Iterator for EdgesFrom<'a, E, C> {
    type Item = &'a Edge<E, C>;

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

impl<'a, E, C> EdgesFrom<'a, E, C> {
    /// Creates a new `EdgesFrom` from the given slice of edges and the next edge index.
    pub fn new(edges: &'a [Edge<E, C>], next: Option<EdgeIndex>) -> Self {
        Self { edges, next }
    }
}

pub struct EdgeIndicesFrom<'a, E, C> {
    edges: &'a [Edge<E, C>],
    next: Option<EdgeIndex>,
}

impl<'a, E, C> Iterator for EdgeIndicesFrom<'a, E, C> {
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

impl<'a, E, C> EdgeIndicesFrom<'a, E, C> {
    /// Creates a new `EdgeIndicesFrom` from the given slice of edges and the next edge index.
    pub fn new(edges: &'a [Edge<E, C>], next: Option<EdgeIndex>) -> Self {
        Self { edges, next }
    }
}
