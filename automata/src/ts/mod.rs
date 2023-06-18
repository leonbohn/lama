use crate::{alphabet::Expression, Alphabet};

pub type Idx = usize;

pub trait Index {
    fn index(&self) -> Idx;
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct StateIndex(Idx);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct EdgeIndex(Idx);

pub struct State<Q, L = ()> {
    color: Q,
    label: Option<L>,
    first_edge: Option<EdgeIndex>,
}

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

    pub fn with_prev(self, prev: EdgeIndex) -> Self {
        Self {
            prev_edge: Some(prev),
            ..self
        }
    }

    pub fn with_next(self, next: EdgeIndex) -> Self {
        Self {
            next_edge: Some(next),
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

    pub fn all_transitions<S>(
        &self,
    ) -> std::iter::Map<E::SymbolsIter, impl FnMut(S) -> Transition<S, C> + '_>
    where
        E: Expression<S>,
        C: Clone,
    {
        self.trigger.symbols().map(|symbol| Transition {
            source: self.source,
            target: self.target,
            symbol,
            color: self.color.clone(),
        })
    }
}

pub struct Transition<S, C> {
    source: StateIndex,
    target: StateIndex,
    symbol: S,
    color: C,
}

pub struct TransitionSystem<A: Alphabet, Q, C, L = ()> {
    alphabet: A,
    states: Vec<State<Q, L>>,
    edges: Vec<Edge<A::Expression, C>>,
}
