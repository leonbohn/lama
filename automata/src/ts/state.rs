use std::ops::Deref;

use crate::Alphabet;

use super::{EdgeIndex, Idx, Index, Indexes, TransitionSystem};

/// Wrapper type for indices of states in a transition system.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct StateIndex(Idx);

impl StateIndex {
    /// Creates a new state index.
    pub fn new(index: Idx) -> Self {
        Self(index)
    }
}

impl Deref for StateIndex {
    type Target = Idx;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Index for StateIndex {
    fn index(&self) -> Idx {
        self.0
    }
}

impl<A: Alphabet, Q, C> Indexes<TransitionSystem<A, Q, C>> for StateIndex {
    type Ref<'a> = &'a State<Q> where TransitionSystem<A, Q, C>: 'a, Self: 'a;

    type MutRef<'a> = &'a mut State<Q> where TransitionSystem<A, Q, C>: 'a, Self: 'a;

    fn get(self, ts: &TransitionSystem<A, Q, C>) -> Option<Self::Ref<'_>> {
        ts.states.get(self.index())
    }

    fn get_mut(self, ts: &mut TransitionSystem<A, Q, C>) -> Option<Self::MutRef<'_>> {
        ts.states.get_mut(self.index())
    }
}

/// A state in a transition system. This stores the color of the state and the index of the
/// first edge leaving the state.
#[derive(Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct State<Q> {
    color: Q,
    first_edge: Option<EdgeIndex>,
}

impl<Q> State<Q> {
    /// Creates a new state with the given color.
    pub fn new(color: Q) -> Self {
        Self {
            color,
            first_edge: None,
        }
    }

    /// Sets the first outgoing edge of the state to the given index.
    pub fn set_first_edge(&mut self, index: EdgeIndex) {
        self.first_edge = Some(index);
    }

    /// Obtains the index of the first outgoing edge.
    pub fn first_edge(&self) -> Option<EdgeIndex> {
        self.first_edge
    }
}
