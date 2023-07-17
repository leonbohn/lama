use std::collections::BTreeMap;

use crate::{
    alphabet::{Alphabet, Symbol, SymbolOf},
    length::HasLength,
    word::Rawpresentation,
    Color, FiniteLength,
};

use super::{
    ColorPosition, EdgeColor, IndexType, OnEdges, OnStates, State, StateColor, StateIndex,
    Successor, Transition,
};

/// Represents a path through a transition system. Note, that the path itself is decoupled from the
/// transition system, which allows to use it for multiple transition systems. In particular, it is possible
/// to create a path through some transition system, modify the transition system and then extend the previously
/// created path in the modified transiton system.
///
/// A path consists of an `origin`, which is simply the [`StateIndex`] of the state where the path starts. It stores
/// a sequence of transitions and the colors of the states it visits.
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Path<A: Alphabet, Idx, C: Color, Pos: ColorPosition> {
    origin: Idx,
    colors: Vec<Pos::StateColor<C>>,
    transitions: Vec<Transition<Idx, A::Symbol, Pos::EdgeColor<C>>>,
}

impl<A: Alphabet, Idx, Pos: ColorPosition, C: Color> Path<A, Idx, C, Pos> {
    /// Returns the index of the state that is reached by the path.
    pub fn reached(&self) -> Idx
    where
        Idx: IndexType,
    {
        if self.transitions.is_empty() {
            self.origin
        } else {
            self.transitions.last().unwrap().target()
        }
    }

    fn transition_colors(&self) -> impl Iterator<Item = Pos::EdgeColor<C>> + '_ {
        self.transitions.iter().map(|t| t.color().clone())
    }

    pub fn colors_length(&self) -> usize {
        self.colors_vec().len()
    }

    pub fn colors_vec(&self) -> Vec<C> {
        Pos::fuse_iters(self.colors.clone(), self.transition_colors()).collect()
    }

    pub fn nth_color(&self, n: usize) -> Option<C> {
        self.colors_vec().get(n).cloned()
    }

    /// Returns true if the path is empty/trivial, meaning it consists of only one state.
    pub fn empty(state: Idx, color: Pos::StateColor<C>) -> Self {
        Self {
            origin: state,
            colors: vec![color],
            transitions: Vec::new(),
        }
    }

    /// Attempts to extend the path in the given `ts` by the given `symbol`. If the path can be extended,
    /// the transition is returned. Otherwise, `None` is returned.
    pub fn extend_in<Ts>(
        &mut self,
        ts: &Ts,
        symbol: A::Symbol,
    ) -> Option<Transition<Idx, A::Symbol, EdgeColor<Ts>>>
    where
        Idx: IndexType,
        Ts: Successor<Alphabet = A, StateIndex = Idx, Color = C, Position = Pos>,
    {
        let state = self.reached();
        let transition = ts.successor(state, symbol)?;
        self.transitions.push(transition.clone());
        self.colors.push(ts.state_color(self.reached()));
        Some(transition)
    }

    /// Returns an iterator over the [`StateIndex`]es of the states visited by the path.
    pub fn state_sequence(&self) -> impl Iterator<Item = Idx> + '_
    where
        Idx: IndexType,
    {
        std::iter::once(self.origin).chain(self.transitions.iter().map(|t| t.target()))
    }
}
