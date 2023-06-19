use petgraph::visit::EdgeRef;

use crate::{alphabet::Symbol, Alphabet};

use super::{Index, StateIndex, Transition, TransitionSystem};

pub trait Successor<S: Symbol> {
    type Transition<'this>
    where
        Self: 'this;
    fn successor(&self, state: StateIndex, symbol: S) -> Option<Self::Transition<'_>>;
}

impl<A: Alphabet, Q, C> Successor<A::Symbol> for TransitionSystem<A, Q, C> {
    type Transition<'this> = Transition<'this, A::Symbol, C> where Self: 'this;
    fn successor(&self, state: StateIndex, symbol: A::Symbol) -> Option<Self::Transition<'_>> {
        self.graph
            .edges(state.node_index())
            .find(|edge| self.alphabet.matches(&edge.weight().0, symbol))
            .map(|edge| {
                Transition::new(
                    edge.source().index().into(),
                    symbol,
                    edge.target().index().into(),
                    &edge.weight().1,
                )
            })
    }
}
