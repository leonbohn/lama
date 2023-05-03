use itertools::Itertools;
use tabled::{builder::Builder, settings::Style};

use crate::{AnonymousGrowable, HasAlphabet, Map, Pair, Pointed, Set, Symbol, Transition};

use super::{
    transition::TransitionReference, Growable, HasInput, HasStates, IntoStates, IntoTransitions,
    LengthLexicographic, Shrinkable, StateIndex, StateOf, Successor, SymbolOf, TransitionOf,
    TriggerIterable, Trivial, Visitor,
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

    /// Returns an iterator over all states of `self`.
    pub fn states(&self) -> States<'_, Q> {
        States {
            iter: self.states.iter(),
        }
    }

    /// Returns the size of `self`, i.e. the number of states.
    pub fn size(&self) -> usize {
        self.states.len()
    }

    /// Computes the set of all reachable states starting in `origin`.
    pub fn reachable_states_from<X: Borrow<Q>>(&self, origin: X) -> Set<Q> {
        LengthLexicographic::new_from(self, origin.borrow().clone())
            .iter()
            .collect()
    }

    /// Computes the set of all reachable states from the initial state.
    pub fn reachable_states(&self) -> Set<Q>
    where
        Self: Pointed<Q = Q>,
    {
        self.reachable_states_from(self.initial())
    }
}

impl<Q: StateIndex, S: Symbol> HasStates for TransitionSystem<Q, S> {
    type Q = Q;
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

impl<T: Transition> Extend<T> for TransitionSystem<T::Q, T::S> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.edges.extend(
            iter.into_iter()
                .map(|x| ((x.source().clone(), x.sym().clone()), x.target().clone())),
        );
        self.states.extend(
            self.edges
                .iter()
                .flat_map(|((x, _), y)| [x.clone(), y.clone()]),
        );
    }
}

impl<Q: StateIndex, S: Symbol> Default for TransitionSystem<Q, S> {
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
    fn successor<X: Borrow<Self::Q>, Y: Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q> {
        self.edges
            .iter()
            .find(|((f, s), _)| f == from.borrow() && s == on.borrow())
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

// impl<S: Symbol, Q: StateIndex + Display> Display for TransitionSystem<Q, S> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         let mut builder = Builder::default();
//         let alphabet = self.input_alphabet().sorted().collect_vec();
//         builder.set_header(
//             vec!["Deterministic".to_string()].into_iter().chain(
//                 alphabet
//                     .iter()
//                     .map(|s| s.to_string())
//                     .collect::<Vec<String>>(),
//             ),
//         );
//         for state in self.states().sorted() {
//             let mut row = vec![state.to_string()];
//             for sym in &alphabet {
//                 row.push(if let Some(successor) = self.successor(state, *sym) {
//                     successor.to_string()
//                 } else {
//                     "-".to_string()
//                 });
//             }
//             builder.push_record(row);
//         }
//         let mut transition_table = builder.build();
//         transition_table.with(Style::psql());
//         write!(f, "{}", transition_table)
//     }
// }

/// Helper struct for iterating over the transitions of a transition system.
#[derive(Clone, Debug)]
pub struct Transitions<'a, Q, S> {
    pub(crate) iter: std::collections::hash_map::Iter<'a, (Q, S), Q>,
}

impl<'a, Q, S> Transitions<'a, Q, S> {
    /// Creates a new [`Transitions`] object from the given iterator.
    pub fn new(iter: std::collections::hash_map::Iter<'a, (Q, S), Q>) -> Self {
        Self { iter }
    }
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
