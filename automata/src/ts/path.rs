use std::collections::BTreeMap;

use crate::{
    alphabet::{Alphabet, Symbol, SymbolOf},
    Color,
};

use super::{State, StateIndex, Successor, Transition};

/// Represents a path through a transition system. Note, that the path itself is decoupled from the
/// transition system, which allows to use it for multiple transition systems. In particular, it is possible
/// to create a path through some transition system, modify the transition system and then extend the previously
/// created path in the modified transiton system.
///
/// A path consists of an `origin`, which is simply the [`StateIndex`] of the state where the path starts. It stores
/// a sequence of transitions and the colors of the states it visits.
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Path<'a, A: Alphabet, Q, C> {
    origin: StateIndex,
    colors: Vec<&'a Q>,
    transitions: Vec<Transition<'a, A::Symbol, C>>,
}

impl<'a, A: Alphabet, Q: Color, C: Color> Path<'a, A, Q, C> {
    /// Returns the index of the state that is reached by the path.
    pub fn reached(&self) -> StateIndex {
        if self.transitions.is_empty() {
            self.origin
        } else {
            self.transitions.last().unwrap().target()
        }
    }

    /// Returns true if the path is empty/trivial, meaning it consists of only one state.
    pub fn empty(state: StateIndex, color: &'a Q) -> Self {
        Self {
            origin: state,
            colors: vec![color],
            transitions: Vec::new(),
        }
    }

    /// Attempts to extend the path in the given `ts` by the given `symbol`. If the path can be extended,
    /// the transition is returned. Otherwise, `None` is returned.
    pub fn extend_in<Ts: Successor<EdgeColor = C, StateColor = Q, Alphabet = A>>(
        &mut self,
        ts: &'a Ts,
        symbol: A::Symbol,
    ) -> Option<Transition<'a, A::Symbol, C>> {
        let state = self.reached();
        let transition = ts.successor(state, symbol)?;
        self.transitions.push(transition.clone());
        self.colors.push(ts.state_color(self.reached()));
        Some(transition)
    }

    /// Returns an iterator over the [`StateIndex`]es of the states visited by the path.
    pub fn state_sequence(&'a self) -> impl Iterator<Item = StateIndex> + 'a {
        std::iter::once(self.origin).chain(self.transitions.iter().map(|t| t.target()))
    }

    /// Returns an iterator over the colors of the states visited by the path.
    pub fn transition_colors(&'a self) -> impl Iterator<Item = &'a C> + 'a {
        self.transitions.iter().map(|t| t.color())
    }

    /// Returns an iterator over the colors of the states visited by the path.
    pub fn state_colors(&'a self) -> impl Iterator<Item = &'a Q> + 'a {
        self.colors.iter().copied()
    }
}
