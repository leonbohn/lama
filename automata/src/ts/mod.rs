use std::{borrow::Borrow, fmt::Display};

use crate::{run::Configuration, Set, Symbol, Word};

mod implementations;
mod transition;
mod visit;

use impl_tools::autoimpl;
use itertools::Itertools;
use owo_colors::OwoColorize;
use tabled::{builder::Builder, settings::Style};
pub use transition::{Transition, TransitionReference, Trigger};

/// An implementation of a deterministic `TransitionSystem` in form of an edge list. The edge list is represented by a vector of tuples `(from, to, symbol)`. Is only available if the `det` feature is enabled.
#[cfg(feature = "det")]
pub mod transitionsystem;
#[cfg(feature = "det")]
pub use transitionsystem::TransitionSystem;

pub use visit::{LengthLexicographic, LengthLexicographicEdges, Visitor, VisitorIter};

use self::transition::StateReference;

/// A trait for the state index type. Implementors must be comparable, hashable, clonable and debuggable. The `create` method is used to create a new state index from a `u32`
pub trait StateIndex: Clone + PartialEq + Eq + std::hash::Hash + std::fmt::Debug + Ord {}

impl<X: Clone + Eq + PartialEq + std::hash::Hash + std::fmt::Debug + Ord> StateIndex for X {}

// The following two type aliases might change in the future to allow for more flexibility, i.e. for example for implementing nondeterminism.
/// Helper type for getting the symbol type of a transition system.
pub type SymbolOf<X> = <X as HasInput>::Sigma;
/// Helper type for getting the output type of a transition system.
pub type StateOf<X> = <X as HasStates>::Q;
/// Helper type for getting the trigger type of a transition system.
pub type TriggerOf<TS> = (StateOf<TS>, SymbolOf<TS>);
/// Helper type for getting the transition type of a transition system.
pub type TransitionOf<TS> = (StateOf<TS>, SymbolOf<TS>, StateOf<TS>);

/// Trait that encapsulates things which have a set of states. The states can be generic, as long as they implement the [`StateIndex`] trait.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait HasStates {
    /// The type of states of the object.
    type Q: StateIndex;
    /// An iterator over the states of the object.
    type States<'me>: Iterator<Item = &'me Self::Q>
    where
        Self: 'me;

    /// Produces an iterator over the states of the object, may contain duplicates and should rarely be used. Instead take [`Self::states()`] to get an iterator over the states without duplicates.
    fn raw_states_iter(&self) -> Self::States<'_>;

    /// Returns an iterator over the states of the object without duplicates.
    fn states(&self) -> itertools::Unique<Self::States<'_>> {
        self.raw_states_iter().unique()
    }
}

/// Trait that encapsulates things which have a set of input symbols, such as a transition system or transducer. The symbols can be generic, as long as they implement the [`Symbol`] trait.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait HasInput {
    /// The type of input symbol.
    type Sigma: Symbol;
    /// An iterator over the input symbols.
    type Input<'me>: Iterator<Item = &'me Self::Sigma>
    where
        Self: 'me;

    /// Should rarely be used as it might contain duplicates, see [`Self::input_alphabet()`] instead.
    fn raw_input_alphabet_iter(&self) -> Self::Input<'_>;

    /// Returns an iterator over the input symbols without duplicates.
    fn input_alphabet(&self) -> itertools::Unique<Self::Input<'_>> {
        self.raw_input_alphabet_iter().unique()
    }
}

/// The base trait implemented by a deterministic transition system. A transition system is a tuple `(Q, S, δ)`, where `Q` is a finite set of states, `S` is a finite set of symbols and `δ: Q × S → Q` is a transition function. Note that the transition function is not necessarily complete and some transitions may be missing.
/// States of a transition system are generic, and can be any type that implements the [`StateIndex`] trait.
/// Also the symbols of a transition system are generic, and can be any type that implements the [`Alphabet`] trait.
/// The [`TransitionTrigger`] trait is used to represent an outgoing transition. Note, that such a trigger is a pair consisting of a state and a symbol, meaning the target state is not included in a trigger.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait Successor: HasStates + HasInput {
    /// Returns the successor state of the given state on the given symbol. The transition function is deterministic, meaning that if a transition exists, it is unique. On the other hand there may not be a transition for a given state and symbol, in which case `succ` returns `None`.
    fn successor(&self, from: &Self::Q, on: &Self::Sigma) -> Option<Self::Q>;

    /// Returns the successor state for the given trigger through calling [`Self::succ`].
    fn apply_trigger(&self, trigger: &(Self::Q, Self::Sigma)) -> Option<Self::Q> {
        self.successor(trigger.source(), trigger.sym())
    }

    /// Creates a new trigger from the given state and symbol.
    fn make_trigger(from: &Self::Q, on: &Self::Sigma) -> (Self::Q, Self::Sigma) {
        (from.clone(), on.clone())
    }

    /// Starts a run from the given state. A run is given by a [`Configuration`] object, which keeps track
    /// of the current state.
    fn run_word_from<W: Word<S = Self::Sigma>>(
        &self,
        on: W,
        from: Self::Q,
    ) -> Configuration<&Self, W>
    where
        Self: Sized,
    {
        Configuration::from_state(self, from, on)
    }

    /// Creates a copy of the current TS which has its initial state set.
    fn start(&self, start: Self::Q) -> (Self, Self::Q)
    where
        Self: Sized + Clone,
    {
        (self.clone(), start)
    }

    /// Builds a string representation of the transition table of the transition system.
    /// For this, the [`tabled`] crate is used.
    fn display_transition_table(&self) -> String
    where
        Self::Q: Display,
    {
        let mut builder = Builder::default();
        builder.set_header(
            vec!["Deterministic".to_string()].into_iter().chain(
                self.input_alphabet()
                    .map(|s| s.purple().to_string())
                    .collect::<Vec<String>>(),
            ),
        );
        for state in self.states() {
            let mut row = vec![state.to_string()];
            for sym in self.input_alphabet() {
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
        transition_table.to_string()
    }

    /// Performs a breadth-first search on the transition system, starting from the given state.
    fn bfs_from(&self, start: Self::Q) -> LengthLexicographic<&Self>
    where
        Self: Sized,
    {
        LengthLexicographic::new_from(self, start)
    }

    /// Performs a breadth-first search on the transition system, starting from the initial state.
    fn bfs(&self) -> LengthLexicographic<&Self>
    where
        Self: Sized + Pointed,
    {
        LengthLexicographic::new(self)
    }

    /// Performs a breadth-first search on the transition system, starting from the given state, emitting each visited edge.
    fn bfs_edges_from(&self, start: Self::Q) -> LengthLexicographicEdges<&Self>
    where
        Self: Sized,
    {
        LengthLexicographicEdges::new_from(self, start)
    }

    /// Performs a breadth-first search on the transition system, starting from the initial state, emitting each visited edge.
    fn bfs_edges(&self) -> LengthLexicographicEdges<&Self>
    where
        Self: Sized + Pointed,
    {
        LengthLexicographicEdges::new(self)
    }
}

/// Creates a new trivial transition system, which could either be empty (for [`TransitionSystem`]) or contain a single initial state (for [`InitializedDeterministic`]).
pub trait Trivial: Successor {
    /// Creates the trivial object
    fn trivial() -> Self;
}

/// Implemented by objects which have a designated initial state.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait Pointed: Successor {
    /// Get the initial state of the automaton.
    fn initial(&self) -> Self::Q;

    /// Runs the word from the given initial state.
    fn run<'t, W>(&'t self, on: &'t W) -> Configuration<&'t Self, &'t W>
    where
        Self: Sized + HasInput,
        W: Word<S = Self::Sigma>,
    {
        Configuration::for_pointed(self, on)
    }
}

/// Trait that allows iterating over all edges in a [`TransitionSystem`].
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait TransitionIterable: Successor {
    /// Type of the iterator over all edges.
    type TransitionIter<'me>: Iterator<Item = &'me (Self::Q, Self::Sigma, Self::Q)>
    where
        Self: 'me,
        Self::Q: 'me,
        Self::Sigma: 'me;

    /// Returns an iterator over all edges in the transition system.
    fn transitions_iter(&self) -> Self::TransitionIter<'_>;

    /// Returns the set of transitions originating from a given state.
    fn transitions_from(&self, from: &Self::Q) -> Set<(Self::Q, Self::Sigma, Self::Q)> {
        self.transitions_iter()
            .filter(|e| e.source() == from)
            .cloned()
            .collect()
    }
}

/// Trait that allows iterating over all triggers in a [`TransitionSystem`].
pub trait TriggerIterable: Successor {
    /// THe iterator type
    type TriggerIter<'me>: Iterator<Item = &'me (Self::Q, Self::Sigma)>
    where
        Self: 'me;

    /// Returns an iterator over all triggers in the transition system.
    fn triggers_iter(&self) -> Self::TriggerIter<'_>;

    /// Returns the set of triggers originating from a given state.
    fn triggers_from(&self, from: &Self::Q) -> Set<&'_ (Self::Q, Self::Sigma)> {
        self.triggers_iter()
            .filter(|e| e.source() == from)
            .collect()
    }
}

/// Converts the given object in to an iterator over transitions, consumes self.
pub trait IntoTransitions: Successor + Copy {
    /// The type of transition output by the iterator.
    type TransitionRef: Transition<Q = Self::Q, S = Self::Sigma>;
    /// The type of the iterator.
    type IntoTransitions: Iterator<Item = Self::TransitionRef>;

    /// Converts the transition system into an iterator over its transitions.
    fn into_transitions(self) -> Self::IntoTransitions;
}

impl<'a, T> IntoTransitions for &'a T
where
    T: IntoTransitions,
{
    type TransitionRef = T::TransitionRef;

    type IntoTransitions = T::IntoTransitions;

    fn into_transitions(self) -> Self::IntoTransitions {
        (*self).into_transitions()
    }
}

/// Converts the given object in to an iterator over states, consumes self.
pub trait IntoStates: HasStates + Copy {
    /// The type of state output by the iterator.
    type StateRef: StateReference<Q = Self::Q>;
    /// The type of the iterator.
    type IntoStates: Iterator<Item = Self::StateRef>;

    /// Converts the transition system into an iterator over its states.
    fn into_states(self) -> Self::IntoStates;
}

impl<'a, T> IntoStates for &'a T
where
    T: IntoStates,
{
    type StateRef = T::StateRef;
    type IntoStates = T::IntoStates;
    fn into_states(self) -> Self::IntoStates {
        (*self).into_states()
    }
}

/// Converts the given transition system in to an Iterator over references to its states.
pub trait IntoStateReferences<'a>: Successor + 'a {
    /// The type of the iterator.
    type Output: Iterator<Item = &'a Self::Q>;
    /// Converts the transition system into an iterator over references to its states.
    fn into_state_references(self) -> Self::Output;
}

/// Ecapsulates the ability to add states and transitions to a transition system.
pub trait Growable: Successor {
    /// Add a new state to the transition system..
    fn add_state(&mut self, state: &Self::Q) -> bool;

    /// Add a new transition to the transition system. If the transition did not exist before, `None` is returned. Otherwise, the old target state is returned.
    fn add_transition<X: Borrow<Self::Q>, Y: Borrow<Self::Q>>(
        &mut self,
        from: X,
        on: SymbolOf<Self>,
        to: Y,
    ) -> Option<Self::Q>;
}

/// Ecapsulates the ability to add anonymous states and transitions to a transition system.
pub trait AnonymousGrowable: Growable {
    /// Add a new state to the transition system..
    fn add_new_state(&mut self) -> Self::Q;
}

/// Implmenetors of this trait can be shrunk, i.e. states and transitions can be removed from the transition system.
pub trait Shrinkable: Successor {
    /// Deletes the given state from the transition system. If the state did not exist before, `None` is returned. Otherwise, the old state is returned.
    /// This method does not remove any transitions which point to the given state.
    fn remove_state(&mut self, state: Self::Q) -> Option<Self::Q>;

    /// Deletes the given transition from the transition system. If the transition did not exist before, `None` is returned. Otherwise, the old target state is returned.
    fn remove_transition(&mut self, from: Self::Q, on: SymbolOf<Self>) -> Option<Self::Q>;
}

/// A trait implemented by a [`TransitionSystem`] which can be trimmed. This means that all unreachable states are removed from the transition system. Further, all transitions which point to or originate from unreachable states are removed. Note that this operation is only applicable to a [`TransitionSystem`] which is [`Pointed`], as the concept of reachability is only defined if a designated initial state is given.
pub trait Trimable: Successor + Pointed {
    /// Removes all unreachable states from the transition system. Additionally removes any transitions which point to or originate from unreachable states.
    fn trim(&mut self);
}
