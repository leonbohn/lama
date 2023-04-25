use itertools::Itertools;
use tabled::{builder::Builder, settings::Style};

use crate::{AnonymousGrowable, HasAlphabet, Map, Pair, Set, Symbol, Transition};

use super::{
    transition::TransitionReference, Growable, HasInput, HasStates, IntoStates, IntoTransitions,
    Shrinkable, StateIndex, StateOf, Successor, SymbolOf, TransitionOf, TriggerIterable, Trivial,
};

use std::{
    borrow::Borrow,
    fmt::{Debug, Display, Formatter},
};

/// An implementation of a deterministic transition system, stored as two `Vec`s containing the states and [`DeterministicTransition`]s.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TransitionSystem<Q: StateIndex = u32, S: Symbol = char> {
    pub(crate) states: Set<Q>,
    pub(crate) edges: Map<(Q, S), Q>,
}

impl<Q: StateIndex, S: Symbol> TransitionSystem<Q, S> {
    /// Create a new empty deterministic transition system.
    pub fn new() -> Self {
        Self {
            edges: Map::new(),
            states: Set::new(),
        }
    }

    /// Returns an iterator over all transitions of `self`.
    pub fn all_transitions(&self) -> Transitions<'_, Q, S> {
        Transitions {
            iter: self.edges.iter(),
        }
    }

    /// Returns an iterator over all transitions of `self` that start in `state`.
    pub fn states(&self) -> States<'_, Q> {
        States {
            iter: self.states.iter(),
        }
    }

    /// Returns the size of `self`, i.e. the number of states.
    pub fn size(&self) -> usize {
        self.states.len()
    }
}

impl<Q: StateIndex, S: Symbol> HasStates for TransitionSystem<Q, S> {
    type Q = Q;

    type States<'me> = std::collections::hash_set::Iter<'me, Q>
    where Self:'me;

    fn raw_states_iter(&self) -> Self::States<'_> {
        self.states.iter()
    }
}

/// An iterator over the alphabet of a [`Deterministic`] transition system.
pub struct TransitionSystemAlphabetIter<'a, Q: StateIndex, S: Symbol> {
    iter: std::collections::hash_map::Iter<'a, (Q, S), Q>,
}

impl<'a, Q: StateIndex, S: Symbol> Iterator for TransitionSystemAlphabetIter<'a, Q, S> {
    type Item = &'a S;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|((_, s), _)| s)
    }
}

impl<Q: StateIndex, S: Symbol> HasInput for TransitionSystem<Q, S> {
    type Sigma = S;

    type Input<'me> = TransitionSystemAlphabetIter<'me, Q, S>
    where Self:'me;

    fn raw_input_alphabet_iter(&self) -> Self::Input<'_> {
        TransitionSystemAlphabetIter {
            iter: self.edges.iter(),
        }
    }
}

impl<T: Transition> FromIterator<T> for TransitionSystem<T::Q, T::S> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let (it, er) = iter.into_iter().tee();
        Self {
            states: it
                .flat_map(|x| [x.source().clone(), x.target().clone()])
                .collect(),
            edges: er
                .map(|x| ((x.source().clone(), x.sym().clone()), x.target().clone()))
                .collect(),
        }
    }
}

impl Default for TransitionSystem {
    fn default() -> Self {
        Self::new()
    }
}

impl<Q: StateIndex> Trivial for TransitionSystem<Q> {
    fn trivial() -> Self {
        Self {
            states: Set::new(),
            edges: Map::new(),
        }
    }
}

impl<I: IntoIterator<Item = (u32, char, u32)>> From<I> for TransitionSystem {
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

impl<S, Q> Successor for TransitionSystem<Q, S>
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
pub struct States<'a, Q: StateIndex> {
    iter: std::collections::hash_set::Iter<'a, Q>,
}

impl<'a, Q: StateIndex> Iterator for States<'a, Q> {
    type Item = &'a Q;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<Q: StateIndex, S: Symbol> TriggerIterable for TransitionSystem<Q, S> {
    type TriggerIter<'me> = std::collections::hash_map::Keys<'me, (Q, S), Q> where Q: 'me, S: 'me;

    fn triggers_iter(&self) -> Self::TriggerIter<'_> {
        self.edges.keys()
    }
}

impl<Q, S> Growable for TransitionSystem<Q, S>
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

impl<S, Q> Shrinkable for TransitionSystem<Q, S>
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

impl<S, Q> AnonymousGrowable for TransitionSystem<Q, S>
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

impl<S: Symbol, Q: StateIndex + Display> Display for TransitionSystem<Q, S> {
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

/// Helper struct for iterating over the transitions of a transition system.
#[derive(Clone, Debug)]
pub struct Transitions<'a, Q, S> {
    iter: std::collections::hash_map::Iter<'a, (Q, S), Q>,
}

impl<'a, Q: StateIndex, S: Symbol> Iterator for Transitions<'a, Q, S> {
    type Item = TransitionReference<'a, Q, S>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|((from, on), to)| TransitionReference::new(from, on, to))
    }
}

impl<'a, S: Symbol, Q: StateIndex> IntoTransitions for &'a TransitionSystem<Q, S> {
    type TransitionRef = TransitionReference<'a, Q, S>;

    type IntoTransitions = Transitions<'a, Q, S>;

    fn into_transitions(self) -> Self::IntoTransitions {
        (*self).all_transitions()
    }
}

impl<'a, S: Symbol, Q: StateIndex> IntoStates for &'a TransitionSystem<Q, S> {
    type StateRef = &'a Q;
    type IntoStates = States<'a, Q>;

    fn into_states(self) -> Self::IntoStates {
        States {
            iter: self.states.iter(),
        }
    }
}
