use crate::Alphabet;

use super::transition::{DeterministicTransition, Trigger};
use super::{FiniteState, Growable, Shrinkable, StateIndex, SymbolFor, TransitionSystem};

use itertools::Itertools;
use std::{fmt::Debug, hash::Hash};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Deterministic<S = char, Q = u32> {
    states: Vec<Q>,
    edges: Vec<DeterministicTransition<S, Q>>,
}

impl Deterministic {
    pub fn new() -> Self {
        Self {
            edges: Vec::new(),
            states: Vec::new(),
        }
    }
}

impl<S: Clone + PartialEq, Q: Clone + PartialEq> Deterministic<S, Q> {
    fn clear_targets(&mut self, from: &Q, on: &S) {
        self.edges.retain(|t| t.0 != *from || t.1 != *on);
    }
}

impl Default for Deterministic {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: IntoIterator<Item = (u32, char, u32)>> From<I> for Deterministic {
    fn from(iter: I) -> Self {
        let edges: Vec<DeterministicTransition<char, u32>> = iter
            .into_iter()
            .map(|(p, a, q)| DeterministicTransition(p, a, q))
            .collect();

        Self {
            states: edges
                .iter()
                .flat_map(|DeterministicTransition(from, _, to)| vec![*from, *to])
                .unique()
                .collect(),
            edges,
        }
    }
}

impl<S, Q> TransitionSystem for Deterministic<S, Q>
where
    S: Alphabet,
    Q: StateIndex,
{
    type Q = Q;
    type S = S;
    type Trigger = Trigger<Self::S, Self::Q>;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.edges
            .iter()
            .find(|DeterministicTransition(f, s, _)| f == from && s == on)
            .map(|DeterministicTransition(_, _, t)| t.clone())
    }
}

impl<Q, S> FiniteState for Deterministic<S, Q>
where
    Q: StateIndex,
    S: Alphabet,
{
    fn states(&self) -> Vec<Self::Q> {
        self.states.clone()
    }

    fn size(&self) -> usize {
        self.states().len()
    }
}

impl<Q, S> Growable for Deterministic<S, Q>
where
    Q: StateIndex,
    S: Alphabet,
{
    fn add_state(&mut self) -> Self::Q {
        let new_state: Q = StateIndex::create(self.states.len() as u32);
        self.states.push(new_state.clone());
        new_state
    }

    fn add_transition(
        &mut self,
        from: Self::Q,
        on: SymbolFor<Self>,
        to: Self::Q,
    ) -> std::option::Option<Q> {
        let old_target = self
            .edges
            .iter()
            .find_map(|DeterministicTransition(f, s, t)| {
                if f == &from && s == &on {
                    Some(t.clone())
                } else {
                    None
                }
            });
        self.clear_targets(&from, &on);
        self.edges.push(DeterministicTransition(from, on, to));
        old_target
    }
}

impl<S, Q> Shrinkable for Deterministic<S, Q>
where
    Q: StateIndex,
    S: Alphabet,
{
    fn remove_state(&mut self, state: Self::Q) -> Option<Self::Q> {
        let output = self.states.iter().find(|&s| s == &state).cloned();
        self.states.retain(|s| s != &state);
        output
    }

    fn remove_transition<X: AsRef<super::SymbolFor<Self>>>(
        &mut self,
        from: Self::Q,
        on: super::SymbolFor<Self>,
    ) -> Option<Self::Q> {
        let output = self
            .edges
            .iter()
            .find_map(|DeterministicTransition(f, s, t)| {
                if f == &from && s == &on {
                    Some(t.clone())
                } else {
                    None
                }
            });
        self.clear_targets(&from, &on);
        output
    }
}
