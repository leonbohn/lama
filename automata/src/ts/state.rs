use std::{fmt::Display, ops::Deref};

use crate::alphabet::Alphabet;

use super::{EdgeIndex, Idx, Index, IndexTS, Indexes};

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

impl Display for StateIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.index())
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

    /// Obtains a reference to the color of the state.
    pub fn color(&self) -> &Q {
        &self.color
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

impl<Q: Display> Display for State<Q> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.color)
    }
}
