mod successor;
pub use successor::Successor;

use petgraph::{prelude::DiGraph, Graph};

use std::ops::Deref;

use crate::{
    alphabet::{Expression, Symbol},
    Alphabet,
};

pub type Idx = usize;

pub trait Index {
    fn index(&self) -> Idx;

    fn state_index(&self) -> StateIndex {
        StateIndex(self.index())
    }

    fn edge_index(&self) -> petgraph::graph::EdgeIndex<Idx> {
        petgraph::graph::EdgeIndex::new(self.index())
    }

    fn node_index(&self) -> petgraph::graph::NodeIndex<Idx> {
        petgraph::graph::NodeIndex::new(self.index())
    }
}

pub trait Indexes<TS> {
    type Ref<'a>
    where
        TS: 'a,
        Self: 'a;
    type MutRef<'a>
    where
        TS: 'a,
        Self: 'a;

    fn get(self, ts: &TS) -> Option<Self::Ref<'_>>;
    fn get_mut(self, ts: &mut TS) -> Option<Self::MutRef<'_>>;
}

impl<A: Alphabet, Q, C> Indexes<TransitionSystem<A, Q, C>> for StateIndex {
    type Ref<'a> = &'a Q where TransitionSystem<A, Q, C>: 'a, Self: 'a;

    type MutRef<'a> = &'a mut Q where TransitionSystem<A, Q, C>: 'a, Self: 'a;

    fn get(self, ts: &TransitionSystem<A, Q, C>) -> Option<Self::Ref<'_>> {
        ts.graph.node_weight(self.node_index())
    }

    fn get_mut(self, ts: &mut TransitionSystem<A, Q, C>) -> Option<Self::MutRef<'_>> {
        ts.graph.node_weight_mut(self.node_index())
    }
}

impl<A: Alphabet, Q, C> Indexes<TransitionSystem<A, Q, C>> for EdgeIndex {
    type Ref<'a> = &'a EdgeLabel<A::Expression, C> where TransitionSystem<A, Q, C>: 'a, Self: 'a;

    type MutRef<'a> = &'a mut EdgeLabel<A::Expression, C> where TransitionSystem<A, Q, C>: 'a, Self: 'a;

    fn get(self, ts: &TransitionSystem<A, Q, C>) -> Option<Self::Ref<'_>> {
        ts.graph.edge_weight(self.edge_index())
    }

    fn get_mut(self, ts: &mut TransitionSystem<A, Q, C>) -> Option<Self::MutRef<'_>> {
        ts.graph.edge_weight_mut(self.edge_index())
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct StateIndex(Idx);

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

impl From<usize> for StateIndex {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct EdgeIndex(Idx);

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

#[derive(Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct State<Q> {
    color: Q,
    first_edge: Option<EdgeIndex>,
}

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

    pub fn with_prev(self, prev: Option<EdgeIndex>) -> Self {
        Self {
            prev_edge: prev,
            ..self
        }
    }

    pub fn with_next(self, next: Option<EdgeIndex>) -> Self {
        Self {
            next_edge: next,
            ..self
        }
    }

    pub fn source(&self) -> StateIndex {
        self.source
    }

    pub fn target(&self) -> StateIndex {
        self.target
    }

    pub fn color(&self) -> &C {
        &self.color
    }

    pub fn trigger(&self) -> &E {
        &self.trigger
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct Transition<'a, S, C> {
    source: StateIndex,
    target: StateIndex,
    symbol: S,
    color: &'a C,
}

impl<'a, S, C> Transition<'a, S, C> {
    pub fn new(source: StateIndex, symbol: S, target: StateIndex, color: &'a C) -> Self {
        Self {
            source,
            target,
            symbol,
            color,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct EdgeLabel<E, C>(E, C);

#[derive(Clone, Debug)]
pub struct TransitionSystem<A: Alphabet, Q, C> {
    alphabet: A,
    graph: DiGraph<Q, EdgeLabel<A::Expression, C>, Idx>,
}

impl<A: Alphabet, Q, C> TransitionSystem<A, Q, C> {
    pub fn new(alphabet: A) -> Self {
        Self {
            alphabet,
            graph: Graph::default(),
        }
    }

    pub fn add_state(&mut self, color: Q) -> StateIndex {
        let idx = self.graph.add_node(color);
        StateIndex(idx.index())
    }

    pub fn add_edge(
        &mut self,
        source: StateIndex,
        trigger: A::Expression,
        target: StateIndex,
        color: C,
    ) -> EdgeIndex {
        let edge = Edge::new(source, target, color, trigger);
        let idx = self.graph.add_edge(
            edge.source.node_index(),
            edge.target.node_index(),
            EdgeLabel(edge.trigger, edge.color),
        );
        EdgeIndex(idx.index())
    }

    pub fn get<I>(&self, index: I) -> Option<<I as Indexes<Self>>::Ref<'_>>
    where
        I: Indexes<Self>,
    {
        index.get(self)
    }

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
    }
}
