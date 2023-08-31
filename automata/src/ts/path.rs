use std::collections::BTreeMap;

use crate::{
    alphabet::{Alphabet, Symbol, SymbolOf},
    length::HasLength,
    Color, FiniteLength,
};

use super::{BTState, EdgeColor, IndexType, StateColor, StateIndex, Successor, Transition};

/// Represents a path through a transition system. Note, that the path itself is decoupled from the
/// transition system, which allows to use it for multiple transition systems. In particular, it is possible
/// to create a path through some transition system, modify the transition system and then extend the previously
/// created path in the modified transiton system.
///
/// A path consists of an `origin`, which is simply the [`StateIndex`] of the state where the path starts. It stores
/// a sequence of transitions and the colors of the states it visits.
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Path<A: Alphabet, Idx> {
    origin: Idx,
    transitions: Vec<(A::Expression, Idx)>,
}

impl<A: Alphabet, Idx> Path<A, Idx> {
    /// Returns the index of the state that is reached by the path.
    pub fn reached(&self) -> Idx
    where
        Idx: IndexType,
    {
        if self.transitions.is_empty() {
            self.origin
        } else {
            self.transitions.last().unwrap().1
        }
    }

    pub fn edge_colors<'a, TS>(&'a self, ts: &'a TS) -> impl Iterator<Item = TS::EdgeColor> + 'a
    where
        TS: Successor<Alphabet = A, StateIndex = Idx>,
        Idx: IndexType,
        TS::EdgeColor: Clone,
    {
        self.transitions
            .iter()
            .map(move |(symbol, target)| ts.edge_color(self.reached(), symbol).expect("These transitions must exist, otherwise the path cannot have been built with a ts that is consistent with the given one."))
    }

    pub fn reached_state_color<'a, TS>(&'a self, ts: &'a TS) -> TS::StateColor
    where
        TS: Successor<Alphabet = A, StateIndex = Idx>,
        Idx: IndexType,
        TS::StateColor: Clone,
    {
        ts.state_color(self.reached())
    }

    pub fn state_colors<'a, TS>(&'a self, ts: &'a TS) -> impl Iterator<Item = TS::StateColor> + 'a
    where
        TS: Successor<Alphabet = A, StateIndex = Idx>,
        Idx: IndexType,
        TS::StateColor: Clone,
    {
        std::iter::once(ts.state_color(self.origin)).chain(
            self.transitions
                .iter()
                .map(move |(_, target)| ts.state_color(*target)),
        )
    }

    /// Returns true if the path is empty/trivial, meaning it consists of only one state.
    pub fn empty(state: Idx) -> Self {
        Self {
            origin: state,
            transitions: Vec::new(),
        }
    }

    /// Attempts to extend the path in the given `ts` by the given `symbol`. If the path can be extended,
    /// the transition is returned. Otherwise, `None` is returned.
    pub fn extend_in<Ts>(
        &mut self,
        ts: &Ts,
        symbol: A::Symbol,
    ) -> Option<Transition<Idx, A::Expression, EdgeColor<Ts>>>
    where
        Idx: IndexType,
        Ts: Successor<Alphabet = A, StateIndex = Idx>,
    {
        let state = self.reached();
        let transition = ts.successor(state, symbol)?;
        self.transitions
            .push((transition.symbol(), transition.target()));
        Some(transition)
    }

    /// Returns an iterator over the [`StateIndex`]es of the states visited by the path.
    pub fn state_sequence(&self) -> impl Iterator<Item = Idx> + '_
    where
        Idx: IndexType,
    {
        std::iter::once(self.origin).chain(self.transitions.iter().map(|(_, target)| *target))
    }
}
