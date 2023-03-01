use crate::{Mapping, Set, Symbol};

use super::{
    Growable, Shrinkable, StateIndex, StateIterable, SymbolOf, TransitionIterable,
    TransitionSystem, TriggerIterable,
};

use std::fmt::Debug;

/// An implementation of a deterministic transition system, stored as two `Vec`s containing the states and [`DeterministicTransition`]s.
#[derive(Debug, Clone)]
pub struct Deterministic<Q: StateIndex = u32, S: Symbol = char> {
    states: Set<Q>,
    edges: Mapping<(Q, S), Q>,
}

impl Deterministic {
    /// Create a new empty deterministic transition system.
    pub fn new() -> Self {
        Self {
            edges: Mapping::new(),
            states: Set::new(),
        }
    }
}

impl Default for Deterministic {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: IntoIterator<Item = (u32, char, u32)>> From<I> for Deterministic {
    fn from(iter: I) -> Self {
        let edges: Mapping<(u32, char), u32> =
            iter.into_iter().map(|(p, a, q)| ((p, a), q)).collect();

        Self {
            states: edges
                .iter()
                .flat_map(|((from, _), to)| vec![*from, *to])
                .collect(),
            edges,
        }
    }
}

impl<S, Q> TransitionSystem for Deterministic<Q, S>
where
    S: Symbol,
    Q: StateIndex,
{
    type Q = Q;
    type S = S;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.edges
            .iter()
            .find(|((f, s), _)| f == from && s == on)
            .map(|((_, _), t)| t.clone())
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

impl<'a, Q, S> StateIterable for &'a Deterministic<Q, S>
where
    Q: StateIndex,
    S: Symbol,
{
    type StateRef = Q;

    type StateIter = StateIter<'a, Q>;

    fn states(&self) -> Self::StateIter {
        StateIter {
            iter: self.states.iter(),
        }
    }
}

impl<'a, Q: StateIndex, S: Symbol> TransitionIterable for &'a Deterministic<Q, S> {
    type TransitionRef = (Q, S, Q);
    type TransitionIter = std::iter::Map<
        std::collections::hash_map::Iter<'a, (Q, S), Q>,
        fn((&(Q, S), &Q)) -> (Q, S, Q),
    >;

    fn edges(&self) -> Self::TransitionIter {
        self.edges
            .iter()
            .map(|((p, a), q)| (p.clone(), a.clone(), q.clone()))
    }
}

impl<'a, Q: StateIndex, S: Symbol> TriggerIterable for &'a Deterministic<Q, S> {
    type TriggerRef = &'a (Q, S);
    type TriggerIter = std::collections::hash_map::Keys<'a, (Q, S), Q>;

    fn triggers(&self) -> Self::TriggerIter {
        self.edges.keys()
    }
}

impl<Q, S> Growable for Deterministic<Q, S>
where
    Q: StateIndex,
    S: Symbol,
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
        self.edges.insert((from, on), to)
    }
}

impl<S, Q> Shrinkable for Deterministic<Q, S>
where
    Q: StateIndex,
    S: Symbol,
{
    fn remove_state(&mut self, state: Self::Q) -> Option<Self::Q> {
        if self.states.remove(&state) {
            Some(state)
        } else {
            None
        }
    }

    fn remove_transition(&mut self, from: Self::Q, on: super::SymbolOf<Self>) -> Option<Self::Q> {
        self.edges.remove(&(from, on))
    }
}
