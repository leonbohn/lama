use std::collections::BTreeMap;

use crate::{prelude::*, Set};
use itertools::Itertools;
#[cfg(test)]
use pretty_assertions::assert_eq;

use super::transition_system::FullTransition;

#[derive(Clone, Eq, PartialEq)]
pub struct NTState<Q> {
    pub(super) color: Q,
    pub(super) first_edge: Option<usize>,
}

impl<Q> NTState<Q> {
    pub fn new(color: Q) -> Self {
        Self {
            color,
            first_edge: None,
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct NTEdge<E, C> {
    pub(super) prev: Option<usize>,
    pub(super) source: usize,
    pub(super) target: usize,
    pub(super) color: C,
    pub(super) expression: E,
    pub(super) next: Option<usize>,
}

impl<'a, E, C: Clone> IsTransition<'a, E, usize, C> for &'a NTEdge<E, C> {
    fn target(&self) -> usize {
        self.target
    }

    fn color(&self) -> C {
        self.color.clone()
    }

    fn expression(&self) -> &'a E {
        &self.expression
    }
}

impl<E, C> NTEdge<E, C> {
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
}

impl<E, C: Color> IsPreTransition<usize, E, C> for NTEdge<E, C> {
    fn source(&self) -> usize {
        self.source
    }

    fn color(&self) -> C {
        self.color.clone()
    }

    fn expression(&self) -> &E {
        &self.expression
    }
}

#[derive(Clone)]
pub struct NTS<A: Alphabet, Q, C> {
    alphabet: A,
    states: Vec<NTState<Q>>,
    edges: Vec<NTEdge<A::Expression, C>>,
}

impl<A: Alphabet, Q: Color, C: Color> Sproutable for NTS<A, Q, C> {
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

    fn set_state_color<X: Into<StateColor<Self>>>(&mut self, index: Self::StateIndex, color: X) {
        assert!(index < self.states.len());
        self.states[index].color = color.into();
    }

    fn add_edge<X, Y>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: EdgeColor<Self>,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Into<Self::StateIndex>,
        Y: Into<Self::StateIndex>,
    {
        let source = from.into();
        let target = to.into();

        let mut edge = NTEdge::new(source, on, color, target);
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

    fn remove_edge(
        &mut self,
        from: Self::StateIndex,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool {
        unimplemented!()
    }
}

impl<Q, C> NTS<Simple, Q, C> {
    pub fn builder() -> NTSBuilder<Q, C> {
        NTSBuilder::default()
    }
}

impl<A: Alphabet, Q: Color, C: Color> NTS<A, Q, C> {
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

    pub fn into_deterministic(self) -> DTS<A, Q, C> {
        assert!(self.is_deterministic());
        DTS(self)
    }

    fn first_edge(&self, idx: usize) -> Option<usize> {
        assert!(idx < self.states.len(), "State {idx} does not exist");
        self.states[idx].first_edge
    }

    fn last_edge(&self, idx: usize) -> Option<usize> {
        assert!(
            idx < self.states.len(),
            "State {idx} does not exist, have {} states",
            self.states.len()
        );

        let mut current = self.states[idx].first_edge?;
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
}

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
    pub fn new(edges: &'a [NTEdge<E, C>], current: Option<usize>) -> Self {
        Self { edges, current }
    }
}

impl<A: Alphabet, Q: Color, C: Color> TransitionSystem for NTS<A, Q, C> {
    type StateIndex = usize;

    type StateColor = Q;

    type EdgeColor = C;

    type TransitionRef<'this> = &'this NTEdge<A::Expression, C>
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

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        assert!(state < self.states.len());
        self.states.get(state).map(|x| x.color.clone())
    }
}

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
    pub fn new(edges: std::slice::Iter<'a, NTEdge<E, C>>, target: usize) -> Self {
        Self { edges, target }
    }
}

impl<A: Alphabet, Q: Color, C: Color> PredecessorIterable for NTS<A, Q, C> {
    type PreTransitionRef<'this> = &'this NTEdge<A::Expression, C>
    where
        Self: 'this;

    type EdgesToIter<'this> = NTSEdgesTo<'this, A::Expression, C>
    where
        Self: 'this;

    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        if state < self.states.len() {
            Some(NTSEdgesTo::new(self.edges.iter(), state))
        } else {
            None
        }
    }
}

pub struct NTSBuilder<Q, C> {
    edges: Vec<(usize, char, C, usize)>,
    default: Option<Q>,
    colors: Vec<(usize, Q)>,
}

impl<Q, C> Default for NTSBuilder<Q, C> {
    fn default() -> Self {
        Self {
            edges: vec![],
            default: None,
            colors: vec![],
        }
    }
}

impl NTSBuilder<(), usize> {
    pub fn into_dpa(mut self, initial: usize) -> DPA<Simple> {
        self.default_color(())
            .deterministic()
            .with_initial(initial)
            .collect_dpa()
    }
}

impl<Q, C: Color> NTSBuilder<Q, C> {
    pub fn default_color(mut self, color: Q) -> Self {
        self.default = Some(color);
        self
    }

    pub fn deterministic(mut self) -> DTS<Simple, Q, C>
    where
        Q: Color,
        C: Color,
    {
        self.collect().try_into().expect("Not deterministic!")
    }

    pub fn color(mut self, idx: usize, color: Q) -> Self
    where
        Q: Color,
    {
        assert!(self.colors.iter().all(|(q, c)| q != &idx || c == &color));
        self.colors.push((idx, color));
        self
    }
    pub fn extend<X: FullTransition<usize, char, C>, T: IntoIterator<Item = X>>(
        mut self,
        iter: T,
    ) -> Self {
        self.edges.extend(iter.into_iter().map(|t| t.clone_tuple()));
        self
    }

    pub fn collect(mut self) -> NTS<Simple, Q, C>
    where
        Q: Color,
        C: Color,
    {
        let alphabet = Simple::from_iter(self.edges.iter().map(|(_, a, _, _)| *a));
        let num_states = self
            .edges
            .iter()
            .flat_map(|(q, _, _, p)| [*p, *q])
            .unique()
            .count();
        let mut ts = NTS::new_for_alphabet(alphabet);
        let colors_it = (0..num_states).map(|x| {
            if let Some(color) =
                self.colors
                    .iter()
                    .find_map(|(q, c)| if *q == x { Some(c.clone()) } else { None })
            {
                color
            } else {
                self.default
                    .clone()
                    .expect("Default is needed as some states have no color")
            }
        });
        let created_states_number = ts.extend_states(colors_it).count();
        assert_eq!(created_states_number, num_states);

        for (p, a, c, q) in self.edges {
            ts.add_edge(p, a, q, c);
        }
        ts
    }
}
