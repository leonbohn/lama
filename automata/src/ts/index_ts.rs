use crate::{
    alphabet::{Alphabet, HasAlphabet},
    Color,
};

use super::{
    edge::{EdgeIndicesFrom, EdgesFrom},
    has_states::StateColored,
    Edge, EdgeIndex, HasStates, Index, Indexes, State, StateIndex, Successor, Transition,
};

#[derive(Clone, Debug)]
pub struct IndexTS<A: Alphabet, Q, C> {
    alphabet: A,
    states: Vec<State<Q>>,
    edges: Vec<Edge<A::Expression, C>>,
}

impl<A: Alphabet, Q, C> IndexTS<A, Q, C> {
    /// Creates a new transition system with the given alphabet.
    pub fn new(alphabet: A) -> Self {
        Self {
            alphabet,
            states: Vec::new(),
            edges: Vec::new(),
        }
    }

    /// Gets a mutable reference to the alphabet of the transition system.
    pub fn alphabet(&self) -> &A {
        &self.alphabet
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
        index.get_from(self)
    }

    /// Returns a mutable reference to the object indexed by `index` if it exists.
    pub fn get_mut<I>(&mut self, index: I) -> Option<<I as Indexes<Self>>::MutRef<'_>>
    where
        I: Indexes<Self>,
    {
        index.get_mut_from(self)
    }
}

impl<A: Alphabet, Q, C> Indexes<IndexTS<A, Q, C>> for StateIndex {
    type Ref<'a> = &'a State<Q> where IndexTS<A, Q, C>: 'a, Self: 'a;

    type MutRef<'a> = &'a mut State<Q> where IndexTS<A, Q, C>: 'a, Self: 'a;

    fn get_from(self, ts: &IndexTS<A, Q, C>) -> Option<Self::Ref<'_>> {
        ts.states.get(self.index())
    }

    fn get_mut_from(self, ts: &mut IndexTS<A, Q, C>) -> Option<Self::MutRef<'_>> {
        ts.states.get_mut(self.index())
    }
}

impl<A: Alphabet, Q, C> Indexes<IndexTS<A, Q, C>> for EdgeIndex {
    type Ref<'a> = &'a Edge<A::Expression, C> where IndexTS<A, Q, C>: 'a, Self: 'a;

    type MutRef<'a> = &'a mut Edge<A::Expression, C> where IndexTS<A, Q, C>: 'a, Self: 'a;

    fn get_from(self, ts: &IndexTS<A, Q, C>) -> Option<Self::Ref<'_>> {
        ts.edges.get(self.index())
    }

    fn get_mut_from(self, ts: &mut IndexTS<A, Q, C>) -> Option<Self::MutRef<'_>> {
        ts.edges.get_mut(self.index())
    }
}

impl<A: Alphabet, Q: Color, C: Color> Successor for IndexTS<A, Q, C> {
    type EdgeColor = C;
    fn successor(
        &self,
        state: StateIndex,
        symbol: A::Symbol,
    ) -> Option<Transition<'_, A::Symbol, Self::EdgeColor>> {
        self.edges_from(state)
            .find(|e| self.alphabet().matches(e.trigger(), symbol))
            .map(|e| Transition::new(state, symbol, e.target(), e.color()))
    }
}

/// An iterator over the states of an [`IndexTS`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IndexTSStates<'a, Q> {
    states: &'a [State<Q>],
    position: usize,
}

impl<'a, Q> IndexTSStates<'a, Q> {
    /// Creates a new iterator over the states of an [`IndexTS`]. Stores a reference
    /// to the slice of states
    pub fn new(states: &'a [State<Q>], position: usize) -> Self {
        Self { states, position }
    }
}

impl<'a, Q> Iterator for IndexTSStates<'a, Q> {
    type Item = (StateIndex, &'a Q);

    fn next(&mut self) -> Option<Self::Item> {
        if self.position < self.states.len() {
            let index = self.position.as_state_index();
            let state = &self.states[self.position];
            self.position += 1;
            Some((index, state.color()))
        } else {
            None
        }
    }
}

impl<A: Alphabet, Q: Color, C> StateColored for IndexTS<A, Q, C> {
    type StateColor = Q;

    fn state_color(&self, index: StateIndex) -> &Self::StateColor {
        assert!(self.contains(index), "State does not exist in the graph.");
        self.get(index)
            .map(|s| s.color())
            .expect("cannot be called if state does not exist!")
    }
}

impl<A: Alphabet, Q: Color, C> HasStates for IndexTS<A, Q, C> {
    type States<'this> = IndexTSStates<'this, Q>
    where
        Self: 'this;

    fn states_iter(&self) -> Self::States<'_> {
        IndexTSStates::new(&self.states, 0)
    }
}

impl<A: Alphabet, Q, C> HasAlphabet for IndexTS<A, Q, C> {
    type Alphabet = A;
    fn alphabet(&self) -> &Self::Alphabet {
        &self.alphabet
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        alphabet,
        ts::{Successor, Transition},
    };

    use super::IndexTS;

    #[test]
    fn build_ts() {
        let mut ts = IndexTS::new(alphabet::Simple::from_iter(['a', 'b']));
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
