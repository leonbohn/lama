use std::{collections::BTreeMap, ffi::FromBytesUntilNulError, hash::Hash};

use crate::{prelude::*, Set, Void};
use itertools::Itertools;
use owo_colors::OwoColorize;

use super::transition_system::FullTransition;

/// Type alias for the constituent parts of an [`NTS`] with the same associated types as the
/// transition sytem `D`.
pub type NTSPartsFor<D> = (
    <D as TransitionSystem>::Alphabet,
    Vec<NTState<StateColor<D>>>,
    Vec<NTEdge<ExpressionOf<D>, EdgeColor<D>>>,
);

/// Stores information characterizing a state in a non-deterministic transition system, see [`NTS`].
/// It stores a color and a pointer to the index of the first edge leaving the state.
#[derive(Clone, Eq, PartialEq)]
pub struct NTState<Q> {
    pub(super) color: Q,
    pub(super) first_edge: Option<usize>,
}

impl<Q> NTState<Q> {
    /// Create a new state with the given color.
    pub fn new(color: Q) -> Self {
        Self {
            color,
            first_edge: None,
        }
    }

    /// Applies the given recoloring function to produce a new [`NTState`] with color `C`.
    /// This method consumes `self`.
    pub fn recolor<C, F: Fn(Q) -> C>(self, f: F) -> NTState<C> {
        NTState {
            color: f(self.color),
            first_edge: self.first_edge,
        }
    }
}

/// Represents an edge in a non-deterministic transition system, see [`NTS`]. It stores a color, an
/// expression, as well as a source and target state index. Moreover, it stores the indices of the
/// next and previous edge in the list of edges leaving the source state.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct NTEdge<E, C> {
    pub(super) prev: Option<usize>,
    pub(super) source: usize,
    pub(super) target: usize,
    pub(super) color: C,
    pub(super) expression: E,
    pub(super) next: Option<usize>,
}

impl<'a, E, C: Clone> IsEdge<'a, E, usize, C> for &'a NTEdge<E, C> {
    fn target(&self) -> usize {
        self.target
    }

    fn color(&self) -> C {
        self.color.clone()
    }

    fn expression(&self) -> &'a E {
        &self.expression
    }

    fn source(&self) -> usize {
        self.source
    }
}

impl<E, C> NTEdge<E, C> {
    /// Creates a new edge with the given source, expression, color and target. The pointers
    /// to the next and previous edge are set to `None`.
    pub fn new(source: usize, expression: E, color: C, target: usize) -> Self {
        Self {
            prev: None,
            source,
            target,
            color,
            expression,
            next: None,
        }
    }

    /// Consumes `self` and applies the given function `f` to obtain a new color which is then
    /// combined with the remaining fields to form a recolored edge.
    pub fn recolor<D, F: Fn(C) -> D>(self, f: F) -> NTEdge<E, D> {
        NTEdge {
            prev: self.prev,
            source: self.source,
            target: self.target,
            color: f(self.color),
            expression: self.expression,
            next: self.next,
        }
    }
}

/// Represents a non-deterministic transition system. It stores an [`Alphabet`], a list of [`NTState`]s and a list of [`NTEdge`]s.
/// Each state
#[derive(Clone, Eq, PartialEq)]
pub struct NTS<A: Alphabet = CharAlphabet, Q = Void, C = Void> {
    alphabet: A,
    states: Vec<NTState<Q>>,
    edges: Vec<NTEdge<A::Expression, C>>,
}

impl<A: Alphabet, Q: Clone, C: Clone> Sproutable for NTS<A, Q, C> {
    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        Self {
            alphabet,
            states: vec![],
            edges: vec![],
        }
    }

    fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
        let id = self.states.len();
        let state = NTState::new(color.into());
        self.states.push(state);
        id
    }

    type ExtendStateIndexIter = std::ops::Range<usize>;

    fn extend_states<I: IntoIterator<Item = StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter {
        let i = self.states.len();
        for state in iter.into_iter() {
            self.add_state(state);
        }
        i..self.states.len()
    }

    fn set_state_color<Idx: Indexes<Self>, X: Into<StateColor<Self>>>(
        &mut self,
        index: Idx,
        color: X,
    ) {
        let Some(index) = index.to_index(self) else {
            tracing::error!("cannot set color of state that does not exist");
            return;
        };
        if index >= self.states.len() {
            panic!(
                "Index {index} is out of bounds, there are only {} states",
                self.states.len()
            );
        }
        self.states[index].color = color.into();
    }

    fn add_edge<X, Y, CI>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: CI,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Indexes<Self>,
        Y: Indexes<Self>,
        CI: Into<EdgeColor<Self>>,
    {
        let source = from.to_index(self)?;
        let target = to.to_index(self)?;

        let mut edge = NTEdge::new(source, on, color.into(), target);
        let edge_id = self.edges.len();

        if let Some(last_edge_id) = self.last_edge(source) {
            assert!(last_edge_id < self.edges.len());
            assert!(self.edges[last_edge_id].next.is_none());
            self.edges[last_edge_id].next = Some(edge_id);
            edge.prev = Some(last_edge_id);
        } else {
            assert!(self.states[source].first_edge.is_none());
            self.states[source].first_edge = Some(edge_id);
        }
        self.edges.push(edge);
        None
    }

    fn remove_edges<X: Indexes<Self>>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool {
        let Some(from) = from.to_index(self) else {
            return false;
        };
        let mut b = false;
        while let Some(pos) = self.edge_position(from, &on) {
            self.remove_edge(from, pos);
            b = true;
        }
        b
    }
}

impl<Q, C> NTS<CharAlphabet, Q, C> {
    /// Returns a transition system builder for a non-deterministic transition system. This should be the main method for the
    /// construction of non-deterministic transition systems on the fly.
    ///
    /// # Example
    ///
    /// We want to create a DFA with two states 0 and 1 over the alphabet `['a', 'b']`. We want to add the following transitions:
    /// - From state 0 to state 0 on symbol 'a'
    /// - From state 0 to state 1 on symbol 'b'
    /// - From state 1 to state 1 on symbol 'a'
    /// - From state 1 to state 0 on symbol 'b'
    /// Further, state 0 should be initial and colored `true` and state 1 should be colored `false`. This can be done as follows
    /// ```
    /// use automata::prelude::*;
    ///
    /// let ts = TSBuilder::default()
    ///     .with_colors([true, false]) // colors given in the order of the states
    ///     .with_transitions([(0, 'a', Void, 0), (0, 'b', Void, 1), (1, 'a', Void, 1), (1, 'b', Void, 0)])
    ///     .into_dfa(0); // 0 is the initial state
    /// ```
    pub fn builder() -> TSBuilder<Q, C> {
        TSBuilder::default()
    }
}

impl<A: Alphabet, Q: Clone, C: Clone> NTS<A, Q, C> {
    pub(crate) fn nts_remove_edge(
        &mut self,
        from: usize,
        on: &ExpressionOf<Self>,
    ) -> Option<(C, usize)> {
        let pos = self.edge_position(from, on)?;
        let (color, target) = (self.edges[pos].color.clone(), self.edges[pos].target);
        self.remove_edge(from, pos);
        Some((color, target))
    }

    pub(crate) fn nts_remove_transitions(
        &mut self,
        from: usize,
        on: SymbolOf<Self>,
    ) -> Set<(ExpressionOf<Self>, C, usize)>
    where
        C: Hash + Eq,
    {
        let mut set = Set::default();
        let mut current = self.first_edge(from);
        let mut to_remove = Vec::new();
        while let Some(idx) = current {
            let edge = &self.edges[idx];
            if edge.expression.symbols().contains(&on) {
                set.insert((edge.expression.clone(), edge.color.clone(), edge.target));
                to_remove.push((from, idx));
            }
            current = edge.next;
        }
        for (from, idx) in to_remove {
            self.remove_edge(from, idx);
        }
        set
    }

    pub(crate) fn nts_remove_state(&mut self, state: usize) -> Option<Q> {
        // let mut current = self.first_edge(state);
        // while let Some(idx) = current {
        //     let edge = &self.edges[idx];
        //     self.remove_edge(state, idx);
        //     current = edge.next;
        // }
        // Some(self.states.remove(state).color)
        todo!()
    }

    fn edge_position(&self, from: usize, on: &A::Expression) -> Option<usize> {
        let mut idx = self.states.get(from)?.first_edge?;
        loop {
            // FIXME: Make this be a match
            if (&self.edges[idx]).expression() == on {
                return Some(idx);
            }
            idx = self.edges[idx].next?;
        }
    }
    fn remove_edge(&mut self, from: usize, idx: usize) {
        assert!(idx < self.edges.len());

        let pred = self.edges[idx].prev;
        let succ = self.edges[idx].next;

        if self.states[from].first_edge == Some(idx) {
            assert!(pred.is_none());
            self.states[from].first_edge = succ;
            return;
        }

        assert!(
            pred.is_some(),
            "This must exist, otherwise we would be first edge"
        );
        if succ.is_some() {
            self.edges[succ.unwrap()].prev = pred;
        }
        self.edges[pred.unwrap()].next = succ;
    }
    /// Builds a new non-deterministic transition system for the given alphabet with a specified capacity.
    pub fn with_capacity(alphabet: A, cap: usize) -> Self {
        Self {
            alphabet,
            states: Vec::with_capacity(cap),
            edges: Vec::with_capacity(cap),
        }
    }
    /// Returns true if the transition system is deterministic, i.e. if there are no two edges leaving
    /// the same state with the same symbol.
    pub fn is_deterministic(&self) -> bool {
        for state in self.state_indices() {
            let mut symbols = Set::default();
            for edge in self.edges_from(state).unwrap() {
                for sym in edge.expression().symbols() {
                    if !symbols.insert(sym) {
                        return false;
                    }
                }
            }
        }
        true
    }

    /// Turns `self` into a deterministic transition system. Panics if `self` is not deterministic.
    pub fn into_deterministic(self) -> DTS<A, Q, C> {
        debug_assert!(self.is_deterministic());
        DTS(self)
    }

    fn first_edge(&self, idx: usize) -> Option<usize> {
        self.states.get(idx)?.first_edge
    }

    fn last_edge(&self, idx: usize) -> Option<usize> {
        let mut current = self.states.get(idx)?.first_edge?;
        loop {
            assert!(
                current < self.edges.len(),
                "Edge with id {current} does not exist"
            );
            if let Some(x) = self.edges[current].next {
                current = x;
            } else {
                return Some(current);
            }
        }
    }

    /// Builds a new [`NTS`] from its constituent parts.
    pub fn from_parts(
        alphabet: A,
        states: Vec<NTState<Q>>,
        edges: Vec<NTEdge<A::Expression, C>>,
    ) -> Self {
        Self {
            alphabet,
            states,
            edges,
        }
    }

    /// Decomposes `self` into a tuple of its constituents.
    pub fn into_parts(self) -> NTSPartsFor<Self> {
        (self.alphabet, self.states, self.edges)
    }
}

/// Iterator over the edges leaving a state in a non-deterministic transition system.
pub struct NTSEdgesFromIter<'a, E, C> {
    edges: &'a [NTEdge<E, C>],
    current: Option<usize>,
}

impl<'a, E, C> Iterator for NTSEdgesFromIter<'a, E, C> {
    type Item = &'a NTEdge<E, C>;
    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.current?;
        assert!(idx < self.edges.len());
        let e = &self.edges[idx];
        self.current = e.next;
        Some(e)
    }
}

impl<'a, E, C> NTSEdgesFromIter<'a, E, C> {
    /// Creates a new iterator over the edges leaving a state.
    pub fn new(edges: &'a [NTEdge<E, C>], current: Option<usize>) -> Self {
        Self { edges, current }
    }
}

impl<A: Alphabet, Q: Clone, C: Clone> TransitionSystem for NTS<A, Q, C> {
    type StateIndex = usize;

    type StateColor = Q;

    type EdgeColor = C;

    type EdgeRef<'this> = &'this NTEdge<A::Expression, C>
    where
        Self: 'this;

    type EdgesFromIter<'this> = NTSEdgesFromIter<'this, A::Expression, C>
    where
        Self: 'this;

    type StateIndices<'this> = std::ops::Range<usize>
    where
        Self: 'this;

    type Alphabet = A;

    fn alphabet(&self) -> &Self::Alphabet {
        &self.alphabet
    }

    fn state_indices(&self) -> Self::StateIndices<'_> {
        0..self.states.len()
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        Some(NTSEdgesFromIter::new(
            &self.edges,
            self.first_edge(state.to_index(self)?),
        ))
    }

    fn state_color<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::StateColor> {
        let state = state.to_index(self)?;
        if state >= self.states.len() {
            panic!(
                "index {state} is out of bounds, there are only {} states",
                self.states.len()
            );
        }
        assert!(state < self.states.len());
        self.states.get(state).map(|x| x.color.clone())
    }
}

/// Iterator over the edges in a [`NTS`] that reach a certain state.
pub struct NTSEdgesTo<'a, E, C> {
    edges: std::slice::Iter<'a, NTEdge<E, C>>,
    target: usize,
}

impl<'a, E, C> Iterator for NTSEdgesTo<'a, E, C> {
    type Item = &'a NTEdge<E, C>;
    fn next(&mut self) -> Option<Self::Item> {
        self.edges.find(|e| e.target == self.target)
    }
}

impl<'a, E, C> NTSEdgesTo<'a, E, C> {
    /// Creates a new iterator over the edges reaching a state.
    pub fn new<A: Alphabet<Expression = E>, Q: Clone>(
        nts: &'a NTS<A, Q, C>,
        target: usize,
    ) -> Self {
        Self {
            edges: nts.edges.iter(),
            target,
        }
    }
}

impl<A: Alphabet, Q: Clone, C: Clone> PredecessorIterable for NTS<A, Q, C> {
    type PreEdgeRef<'this> = &'this NTEdge<A::Expression, C>
    where
        Self: 'this;

    type EdgesToIter<'this> = NTSEdgesTo<'this, A::Expression, C>
    where
        Self: 'this;

    fn predecessors<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesToIter<'_>> {
        let state = state.to_index(self)?;
        if state < self.states.len() {
            Some(NTSEdgesTo::new(self, state))
        } else {
            None
        }
    }
}
