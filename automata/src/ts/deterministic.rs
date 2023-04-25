use itertools::Itertools;
use tabled::{builder::Builder, settings::Style};

use crate::{AnonymousGrowable, Map, Set, Symbol};

use super::{
    Growable, HasInput, HasStates, Shrinkable, StateIndex, StateOf, SymbolOf, TransitionOf,
    TransitionSystem, TriggerIterable, Trivial,
};

use std::{
    borrow::Borrow,
    fmt::{Debug, Display, Formatter},
};

/// An implementation of a deterministic transition system, stored as two `Vec`s containing the states and [`DeterministicTransition`]s.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Deterministic<Q: StateIndex = u32, S: Symbol = char> {
    pub(crate) states: Set<Q>,
    pub(crate) edges: Map<(Q, S), Q>,
}

impl<Q: StateIndex, S: Symbol> HasStates for Deterministic<Q, S> {
    type Q = Q;

    type States<'me> = std::collections::hash_set::Iter<'me, Q>
    where Self:'me;

    fn states_iter(&self) -> Self::States<'_> {
        self.states.iter()
    }
}

/// An iterator over the alphabet of a [`Deterministic`] transition system.
pub struct DeterministicAlphabetIter<'a, Q: StateIndex, S: Symbol> {
    iter: std::collections::hash_map::Iter<'a, (Q, S), Q>,
}

impl<'a, Q: StateIndex, S: Symbol> Iterator for DeterministicAlphabetIter<'a, Q, S> {
    type Item = &'a S;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|((_, s), _)| s)
    }
}

impl<Q: StateIndex, S: Symbol> HasInput for Deterministic<Q, S> {
    type Sigma = S;

    type Input<'me> = DeterministicAlphabetIter<'me, Q, S>
    where Self:'me;

    fn input_alphabet_iter(&self) -> Self::Input<'_> {
        DeterministicAlphabetIter {
            iter: self.edges.iter(),
        }
    }
}

impl<Q: StateIndex, S: Symbol> Deterministic<Q, S> {
    /// Create a new empty deterministic transition system.
    pub fn new() -> Self {
        Self {
            edges: Map::new(),
            states: Set::new(),
        }
    }
}

impl<Q: StateIndex, S: Symbol> FromIterator<TransitionOf<Self>> for Deterministic<Q, S> {
    fn from_iter<T: IntoIterator<Item = TransitionOf<Self>>>(iter: T) -> Self {
        let (it, er) = iter.into_iter().tee();
        Self {
            states: it.map(|(q, _, _)| q).collect(),
            edges: er.map(|(q, a, p)| ((q, a), p)).collect(),
        }
    }
}

impl Default for Deterministic {
    fn default() -> Self {
        Self::new()
    }
}

impl<Q: StateIndex> Trivial for Deterministic<Q> {
    fn trivial() -> Self {
        Self {
            states: Set::new(),
            edges: Map::new(),
        }
    }
}

impl<I: IntoIterator<Item = (u32, char, u32)>> From<I> for Deterministic {
    fn from(iter: I) -> Self {
        let edges: Map<(u32, char), u32> = iter.into_iter().map(|(p, a, q)| ((p, a), q)).collect();

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
    fn successor(&self, from: &StateOf<Self>, on: &Self::Sigma) -> Option<StateOf<Self>> {
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
    type Item = &'a Q;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<Q: StateIndex, S: Symbol> TriggerIterable for Deterministic<Q, S> {
    type TriggerIter<'me> = std::collections::hash_map::Keys<'me, (Q, S), Q> where Q: 'me, S: 'me;

    fn triggers_iter(&self) -> Self::TriggerIter<'_> {
        self.edges.keys()
    }
}

impl<Q, S> Growable for Deterministic<Q, S>
where
    Q: StateIndex,
    S: Symbol,
{
    fn add_state(&mut self, state: &StateOf<Self>) -> bool {
        self.states.insert(state.clone())
    }

    fn add_transition<X: Borrow<Q>, Y: Borrow<Q>>(
        &mut self,
        from: X,
        on: SymbolOf<Self>,
        to: Y,
    ) -> std::option::Option<Q> {
        self.edges
            .insert((from.borrow().clone(), on), to.borrow().clone())
    }
}

impl<S, Q> Shrinkable for Deterministic<Q, S>
where
    Q: StateIndex,
    S: Symbol,
{
    fn remove_state(&mut self, state: StateOf<Self>) -> Option<StateOf<Self>> {
        if self.states.remove(&state) {
            Some(state)
        } else {
            None
        }
    }

    fn remove_transition(
        &mut self,
        from: StateOf<Self>,
        on: super::SymbolOf<Self>,
    ) -> Option<StateOf<Self>> {
        self.edges.remove(&(from, on))
    }
}

impl<S, Q> AnonymousGrowable for Deterministic<Q, S>
where
    Q: StateIndex + From<u32>,
    S: Symbol,
{
    fn add_new_state(&mut self) -> StateOf<Self> {
        let new_id: Q = (self.states.len() as u32).into();
        if self.add_state(&new_id) {
            new_id
        } else {
            panic!("Failed to add new state")
        }
    }
}

impl<S: Symbol, Q: StateIndex + Display> Display for Deterministic<Q, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut builder = Builder::default();
        let alphabet = self.input_alphabet().sorted().collect_vec();
        builder.set_header(
            vec!["Deterministic".to_string()].into_iter().chain(
                alphabet
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<String>>(),
            ),
        );
        for state in self.states().sorted() {
            let mut row = vec![state.to_string()];
            for sym in &alphabet {
                row.push(if let Some(successor) = self.successor(state, sym) {
                    successor.to_string()
                } else {
                    "-".to_string()
                });
            }
            builder.push_record(row);
        }
        let mut transition_table = builder.build();
        transition_table.with(Style::psql());
        write!(f, "{}", transition_table)
    }
}
