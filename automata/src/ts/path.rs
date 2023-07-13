use std::collections::BTreeMap;

use crate::{
    alphabet::{Alphabet, Symbol, SymbolOf},
    length::HasLength,
    word::Rawpresentation,
    Color, FiniteLength,
};

use super::{
    ColorPosition, Colored, EdgeColor, IndexType, OnEdges, OnStates, State, StateColor, StateIndex,
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
pub struct Path<'a, A: Alphabet, Idx, C: Color, Pos: ColorPosition> {
    origin: Idx,
    colors: Vec<&'a Pos::StateColor<C>>,
    transitions: Vec<Transition<'a, Idx, A::Symbol, Pos::EdgeColor<C>>>,
}

impl<'a, A: Alphabet, Idx, C: Color> ColorSequence<C> for Path<'a, A, Idx, C, OnStates> {
    fn nth_color(&self, n: usize) -> Option<&C> {
        self.colors.get(n).copied()
    }

    fn colors_length(&self) -> usize {
        self.colors.len()
    }
}

impl<'a, A: Alphabet, Idx, C: Color> ColorSequence<C> for Path<'a, A, Idx, C, OnEdges> {
    fn nth_color(&self, n: usize) -> Option<&C> {
        self.transitions.get(n).map(|t| t.color())
    }

    fn colors_length(&self) -> usize {
        self.transitions.len()
    }
}

pub trait ColorSequence<C> {
    fn nth_color(&self, n: usize) -> Option<&C>;
    fn colors_length(&self) -> usize;
}

impl<'a, A: Alphabet, Idx, Pos: ColorPosition, C: Color> Path<'a, A, Idx, C, Pos> {
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

    /// Returns true if the path is empty/trivial, meaning it consists of only one state.
    pub fn empty(state: Idx, color: &'a Pos::StateColor<C>) -> Self {
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
        ts: &'a Ts,
        symbol: A::Symbol,
    ) -> Option<Transition<'a, Idx, A::Symbol, EdgeColor<Ts>>>
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
    pub fn state_sequence(&'a self) -> impl Iterator<Item = Idx> + 'a
    where
        Idx: IndexType,
    {
        std::iter::once(self.origin).chain(self.transitions.iter().map(|t| t.target()))
    }

    /// Returns an iterator over the colors of the states visited by the path.
    pub fn transition_colors(&'a self) -> impl Iterator<Item = &'a Pos::EdgeColor<C>> + 'a {
        self.transitions.iter().map(|t| t.color())
    }

    /// Returns an iterator over the colors of the states visited by the path.
    pub fn state_colors(&'a self) -> impl Iterator<Item = &'a Pos::StateColor<C>> + 'a {
        self.colors.iter().copied()
    }
}
