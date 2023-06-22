use super::{EdgeIndex, StateIndex};

/// Type for indices of states and edges.
pub type Idx = usize;

/// Helper trait for index types, which also allows conversion to a state or edge index.
pub trait Index {
    /// Turns self into an index of type [`Idx`].
    fn index(&self) -> Idx;

    /// Turns self into a state index.
    fn as_state_index(&self) -> StateIndex {
        StateIndex::new(self.index())
    }

    /// Turns self into an edge index.
    fn as_edge_index(&self) -> EdgeIndex {
        EdgeIndex::new(self.index())
    }
}

impl Index for Idx {
    fn index(&self) -> Idx {
        *self
    }
}

/// Trait for types that can be used to index into a transition system. This is implemented for
/// both edge and state indices and allows for a uniform interface for accessing states and edges.
pub trait Indexes<TS> {
    /// Type of (immutable) references to the indexed type.
    type Ref<'a>
    where
        TS: 'a,
        Self: 'a;
    /// Type of mutable references to the indexed type.
    type MutRef<'a>
    where
        TS: 'a,
        Self: 'a;

    /// Returns an immutable reference to the indexed type.
    fn get(self, ts: &TS) -> Option<Self::Ref<'_>>;
    /// Returns a mutable reference to the indexed type.
    fn get_mut(self, ts: &mut TS) -> Option<Self::MutRef<'_>>;
}
