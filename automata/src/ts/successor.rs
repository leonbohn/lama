use crate::{alphabet::Symbol, Alphabet};

use super::{StateIndex, Transition, TransitionSystem};

pub trait Successor<S: Symbol> {
    type Color;
    fn successor(&self, state: StateIndex, symbol: S) -> Option<Transition<'_, S, Self::Color>>;
    fn successor_index(&self, state: StateIndex, symbol: S) -> Option<StateIndex> {
        self.successor(state, symbol).map(|t| t.target())
    }
}

impl<A: Alphabet, Q, C> Successor<A::Symbol> for TransitionSystem<A, Q, C> {
    type Color = C;
    fn successor(
        &self,
        state: StateIndex,
        symbol: A::Symbol,
    ) -> Option<Transition<'_, A::Symbol, Self::Color>> {
        self.edges_from(state)
            .find(|e| self.alphabet.matches(e.trigger(), symbol))
            .map(|e| Transition::new(state, symbol, e.target(), e.color()))
    }
}
