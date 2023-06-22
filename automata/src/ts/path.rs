use std::collections::BTreeMap;

use crate::{
    alphabet::{Alphabet, Symbol, SymbolOf},
    Color,
};

use super::{State, StateIndex, Successor, Transition};

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Path<'a, A: Alphabet, Q, C> {
    origin: StateIndex,
    colors: Vec<&'a Q>,
    transitions: Vec<Transition<'a, A::Symbol, C>>,
}

impl<'a, A: Alphabet, Q: Color, C: Color> Path<'a, A, Q, C> {
    pub fn reached(&self) -> StateIndex {
        if self.transitions.is_empty() {
            self.origin
        } else {
            self.transitions.last().unwrap().target()
        }
    }

    pub fn empty(state: StateIndex, color: &'a Q) -> Self {
        Self {
            origin: state,
            colors: vec![color],
            transitions: Vec::new(),
        }
    }

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

    pub fn state_sequence(&'a self) -> impl Iterator<Item = StateIndex> + 'a {
        self.transitions.iter().map(|t| t.target())
    }

    pub fn transition_colors(&'a self) -> impl Iterator<Item = &'a C> + 'a {
        self.transitions.iter().map(|t| t.color())
    }

    pub fn state_colors(&'a self) -> impl Iterator<Item = &'a Q> + 'a {
        self.colors.iter().map(|c| *c)
    }
}
