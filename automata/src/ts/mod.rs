mod state;
mod successor;
pub use state::{State, StateIndex};
mod edge;
pub use edge::{Edge, EdgeIndex};
mod transition;
pub use successor::Successor;
pub use transition::Transition;

use crate::Alphabet;

use self::edge::{EdgeIndicesFrom, EdgesFrom};

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

#[derive(Clone, Debug)]
pub struct TransitionSystem<A: Alphabet, Q, C> {
    alphabet: A,
    states: Vec<State<Q>>,
    edges: Vec<Edge<A::Expression, C>>,
}

impl<A: Alphabet, Q, C> TransitionSystem<A, Q, C> {
    /// Creates a new transition system with the given alphabet.
    pub fn new(alphabet: A) -> Self {
        Self {
            alphabet,
            states: Vec::new(),
            edges: Vec::new(),
        }
    }

    /// Returns an iterator over the [`EdgeIndex`]es of the edges leaving the given state.
    pub fn edge_indices_from(&self, source: StateIndex) -> EdgeIndicesFrom<'_, A::Expression, C> {
        EdgeIndicesFrom::new(&self.edges, self.get(source).and_then(|s| s.first_edge()))
    }

    /// Returns an iterator over references to the edges leaving the given state.
    pub fn edges_from(&self, source: StateIndex) -> EdgesFrom<'_, A::Expression, C> {
        EdgesFrom::new(&self.edges, self.get(source).and_then(|s| s.first_edge()))
    }

    /// Adds a state with given `color` to the transition system, returning the [`StateIndex`] of
    /// the new state.
    pub fn add_state(&mut self, color: Q) -> StateIndex {
        let id = self.states.len();
        let state = State::new(color);
        self.states.push(state);
        id.as_state_index()
    }

    /// Checks whether [`index`] points to a valid object in `self`.
    pub fn contains<I>(&self, index: I) -> bool
    where
        I: Indexes<Self>,
    {
        self.get(index).is_some()
    }

    fn last_edge_from(&self, source: StateIndex) -> Option<EdgeIndex> {
        self.edge_indices_from(source).last()
    }

    /// Adds an edge from `source` to `target` with the given `trigger` and `color`, returning the
    /// [`EdgeIndex`] of the new edge. This method panics if `source` or `target` do not exist in
    /// the graph.
    pub fn add_edge(
        &mut self,
        source: StateIndex,
        trigger: A::Expression,
        target: StateIndex,
        color: C,
    ) -> EdgeIndex {
        assert!(
            self.contains(source) && self.contains(target),
            "Source or target vertex does not exist in the graph."
        );
        let new_edge_id = self.edges.len().as_edge_index();

        let edge = if let Some(last_edge_id) = self.last_edge_from(source) {
            self.get_mut(last_edge_id).unwrap().set_next(new_edge_id);
            Edge::new(source, target, color, trigger).with_prev(last_edge_id)
        } else {
            self.get_mut(source).unwrap().set_first_edge(new_edge_id);
            Edge::new(source, target, color, trigger)
        };
        self.edges.push(edge);
        new_edge_id
    }

    /// Returns an immutable reference to the object indexed by `index` if it exists.
    pub fn get<I>(&self, index: I) -> Option<<I as Indexes<Self>>::Ref<'_>>
    where
        I: Indexes<Self>,
    {
        index.get(self)
    }

    /// Returns a mutable reference to the object indexed by `index` if it exists.
    pub fn get_mut<I>(&mut self, index: I) -> Option<<I as Indexes<Self>>::MutRef<'_>>
    where
        I: Indexes<Self>,
    {
        index.get_mut(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        alphabet,
        ts::{Successor, Transition},
    };

    use super::TransitionSystem;

    #[test]
    fn build_ts() {
        let mut ts = TransitionSystem::new(alphabet::Simple::from_iter(['a', 'b']));
        let s0 = ts.add_state(());
        let s1 = ts.add_state(());
        let _e0 = ts.add_edge(s0, 'a', s1, 0);
        let _e1 = ts.add_edge(s0, 'b', s0, 1);
        let _e2 = ts.add_edge(s1, 'a', s1, 0);
        let _e3 = ts.add_edge(s1, 'b', s0, 1);
        println!("{:?}", ts);
        assert!(ts.successor(s0, 'a').is_some());
        assert_eq!(
            ts.successor(s1, 'a'),
            Some(Transition::new(s1, 'a', s1, &0))
        );
        assert_eq!(ts.edges_from(s0).count(), 2);
    }
}
