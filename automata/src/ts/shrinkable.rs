use std::hash::Hash;

use crate::{automaton::Initialized, Alphabet, Color, Set, TransitionSystem};

use super::{transition_system::Indexes, ExpressionOf, HashTs, IndexType, SymbolOf, DTS, NTS};

/// Encapsulates the ability to remove states, edges, and transitions from a transition system.
pub trait Shrinkable: TransitionSystem {
    /// Removes a state from the transition system and returns the color associated with the removed state.
    /// If the state is not present, returns `None`.
    fn remove_state<Idx: Indexes<Self>>(&mut self, state: Idx) -> Option<Self::StateColor>;

    /// Removes the first edge from the transition system that originates in `source` and matches the given
    /// `crate::prelude::Expression`. The call returns a pair consisting of the color and target of the removed edge.
    /// If no suitable edge is present, returns `None`.
    ///
    /// This method expects to identify the edge that should be removed based on its [`crate::prelude::Expression`], for
    /// a method that identifies the edge based on its target, see [`Self::remove_transitions`].
    fn remove_edge<Idx: Indexes<Self>>(
        &mut self,
        source: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<(Self::EdgeColor, Self::StateIndex)>;

    /// Removes **all** transitions from the transition system that originate in `source` and match the given
    /// `symbol`. The call returns a [`Set`] of triples, each consisting of the expression, color, and target of
    /// the removed transition. If no suitable transitions are present, returns `None`.
    #[allow(clippy::type_complexity)]
    fn remove_transitions<Idx: Indexes<Self>>(
        &mut self,
        source: Idx,
        symbol: &SymbolOf<Self>,
    ) -> Option<Set<(ExpressionOf<Self>, Self::EdgeColor, Self::StateIndex)>>;
}

impl<A: Alphabet, Q: Clone + Hash + Eq, C: Clone + Hash + Eq, Index: IndexType> Shrinkable
    for HashTs<A, Q, C, Index>
{
    fn remove_state<Idx: Indexes<Self>>(&mut self, state: Idx) -> Option<Self::StateColor> {
        self.hashts_remove_state(state.to_index(self)?)
    }

    fn remove_edge<Idx: Indexes<Self>>(
        &mut self,
        source: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<(Self::EdgeColor, Self::StateIndex)> {
        self.hashts_remove_edge(source.to_index(self)?, expression)
    }

    fn remove_transitions<Idx: Indexes<Self>>(
        &mut self,
        source: Idx,
        symbol: &SymbolOf<Self>,
    ) -> Option<Set<(ExpressionOf<Self>, Self::EdgeColor, Self::StateIndex)>> {
        Some(self.hashts_remove_transitions(source.to_index(self)?, *symbol))
    }
}

impl<A: Alphabet, Q: Color, C: Color> Shrinkable for NTS<A, Q, C> {
    fn remove_state<Idx: Indexes<Self>>(&mut self, state: Idx) -> Option<Self::StateColor> {
        self.nts_remove_state(state.to_index(self)?)
    }

    fn remove_edge<Idx: Indexes<Self>>(
        &mut self,
        source: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<(Self::EdgeColor, Self::StateIndex)> {
        self.nts_remove_edge(source.to_index(self)?, expression)
    }

    fn remove_transitions<Idx: Indexes<Self>>(
        &mut self,
        source: Idx,
        symbol: &SymbolOf<Self>,
    ) -> Option<Set<(ExpressionOf<Self>, Self::EdgeColor, Self::StateIndex)>> {
        Some(self.nts_remove_transitions(source.to_index(self)?, *symbol))
    }
}

impl<A: Alphabet, Q: Color, C: Color> Shrinkable for DTS<A, Q, C> {
    fn remove_state<Idx: Indexes<Self>>(&mut self, state: Idx) -> Option<Self::StateColor> {
        self.0.remove_state(state.to_index(self)?)
    }

    fn remove_edge<Idx: Indexes<Self>>(
        &mut self,
        source: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<(Self::EdgeColor, Self::StateIndex)> {
        self.0.remove_edge(source.to_index(self)?, expression)
    }

    fn remove_transitions<Idx: Indexes<Self>>(
        &mut self,
        source: Idx,
        symbol: &SymbolOf<Self>,
    ) -> Option<Set<(ExpressionOf<Self>, Self::EdgeColor, Self::StateIndex)>> {
        self.0.remove_transitions(source.to_index(self)?, symbol)
    }
}

impl<Ts: Shrinkable> Shrinkable for Initialized<Ts> {
    fn remove_state<Idx: Indexes<Self>>(&mut self, state: Idx) -> Option<Self::StateColor> {
        let q = state.to_index(self)?;
        self.ts_mut().remove_state(q)
    }

    fn remove_edge<Idx: Indexes<Self>>(
        &mut self,
        source: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<(Self::EdgeColor, Self::StateIndex)> {
        let q = source.to_index(self)?;
        self.ts_mut().remove_edge(q, expression)
    }

    fn remove_transitions<Idx: Indexes<Self>>(
        &mut self,
        source: Idx,
        symbol: &SymbolOf<Self>,
    ) -> Option<Set<(ExpressionOf<Self>, Self::EdgeColor, Self::StateIndex)>> {
        let q = source.to_index(self)?;
        self.ts_mut().remove_transitions(q, symbol)
    }
}
