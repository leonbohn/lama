use crate::{Alphabet, Set};

use super::{
    Growable, Shrinkable, StateIndex, StateIterable, SymbolOf, TransitionIterable,
    TransitionSystem, TriggerIterable,
};
use crate::{Pair, Triple};

use std::fmt::Debug;

/// An implementation of a deterministic transition system, stored as two `Vec`s containing the states and [`DeterministicTransition`]s.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Deterministic<S: Alphabet = char, Q: StateIndex = u32> {
    states: Set<Q>,
    edges: Set<Triple<S, Q>>,
}

impl Deterministic {
    /// Create a new empty deterministic transition system.
    pub fn new() -> Self {
        Self {
            edges: Set::new(),
            states: Set::new(),
        }
    }
}

impl<S: Alphabet, Q: StateIndex> Deterministic<S, Q> {
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
        let edges: Set<Triple<char, u32>> =
            iter.into_iter().map(|(p, a, q)| Triple(p, a, q)).collect();

        Self {
            states: edges
                .iter()
                .flat_map(|Triple(from, _, to)| vec![*from, *to])
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
    type Trigger = Pair<Self::S, Self::Q>;
    type Transition = Triple<Self::S, Self::Q>;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.edges
            .iter()
            .find(|Triple(f, s, _)| f == from && s == on)
            .map(|Triple(_, _, t)| t.clone())
    }
}

#[derive(Clone, Debug)]
/// An iterator over the states of a deterministic transition system.
pub struct StateIter<'a, Q: StateIndex> {
    iter: std::collections::hash_set::Iter<'a, Q>,
}

impl<'a, Q: StateIndex> Iterator for StateIter<'a, Q> {
    type Item = Q;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().cloned()
    }
}

impl<'a, Q, S> StateIterable for &'a Deterministic<S, Q>
where
    Q: StateIndex,
    S: Alphabet,
{
    type StateRef = Q;

    type StateIter = StateIter<'a, Q>;

    fn states(&self) -> Self::StateIter {
        StateIter {
            iter: self.states.iter(),
        }
    }
}

impl<'a, Q: StateIndex, S: Alphabet> TransitionIterable for &'a Deterministic<S, Q> {
    type TransitionRef = &'a Triple<S, Q>;
    type TransitionIter = std::collections::hash_set::Iter<'a, Triple<S, Q>>;

    fn edges(&self) -> Self::TransitionIter {
        self.edges.iter()
    }
}

impl<'a, Q: StateIndex, S: Alphabet> TriggerIterable for &'a Deterministic<S, Q> {
    type TriggerRef = Pair<S, Q>;
    type TriggerIter = std::iter::Map<
        std::collections::hash_set::Iter<'a, Triple<S, Q>>,
        fn(&Triple<S, Q>) -> Pair<S, Q>,
    >;

    fn triggers(&self) -> Self::TriggerIter {
        self.edges.iter().map(|t| t.into())
    }
}

impl<Q, S> Growable for Deterministic<S, Q>
where
    Q: StateIndex,
    S: Alphabet,
{
    fn add_state(&mut self) -> Self::Q {
        let new_state: Q = StateIndex::create(self.states.len() as u32);
        self.states.insert(new_state.clone());
        new_state
    }

    fn add_transition(
        &mut self,
        from: Self::Q,
        on: SymbolOf<Self>,
        to: Self::Q,
    ) -> std::option::Option<Q> {
        let old_target = self.edges.iter().find_map(|Triple(f, s, t)| {
            if f == &from && s == &on {
                Some(t.clone())
            } else {
                None
            }
        });
        self.clear_targets(&from, &on);
        self.edges.insert(Triple(from, on, to));
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

    fn remove_transition(&mut self, from: Self::Q, on: super::SymbolOf<Self>) -> Option<Self::Q> {
        let output = self.edges.iter().find_map(|Triple(f, s, t)| {
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
