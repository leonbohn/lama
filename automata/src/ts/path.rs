use crate::{
    alphabet::{Alphabet, Symbol, SymbolOf},
    Color,
};

use super::{State, StateIndex, Successor, Transition};

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Path<'a, A: Alphabet, C> {
    origin: StateIndex,
    sequence: Vec<Transition<'a, A::Symbol, C>>,
}

impl<'a, A: Alphabet, C: Color> Path<'a, A, C> {
    pub fn reached(&self) -> StateIndex {
        if self.sequence.is_empty() {
            self.origin
        } else {
            self.sequence.last().unwrap().target()
        }
    }

    pub fn empty(state: StateIndex) -> Self {
        Self {
            origin: state,
            sequence: Vec::new(),
        }
    }

    pub fn extend_in<Ts: Successor<EdgeColor = C, Alphabet = A>>(
        &mut self,
        ts: &'a Ts,
        symbol: A::Symbol,
    ) -> Option<Transition<'a, A::Symbol, C>> {
        let state = self.reached();
        let transition = ts.successor(state, symbol)?;
        self.sequence.push(transition.clone());
        Some(transition)
    }

    pub fn color_sequence(&'a self) -> impl Iterator<Item = &'a C> + 'a {
        self.sequence.iter().map(|t| t.color())
    }
}
