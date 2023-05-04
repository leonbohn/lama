use std::{borrow::Borrow, fmt::Display};

use crate::{
    helpers::MooreMachine, output::IntoAssignments, run::Configuration, Combined, MealyMachine,
    Set, Symbol, Word,
};

mod restricted;
mod successor;
mod transition;
mod visit;

pub use restricted::Restricted;

pub use successor::Successor;

use impl_tools::autoimpl;
use itertools::Itertools;
use owo_colors::OwoColorize;
use tabled::{builder::Builder, settings::Style};
pub use transition::{StateReference, Transition, TransitionReference, Trigger};

/// An implementation of a deterministic `TransitionSystem` in form of an edge list. The edge list is represented by a vector of tuples `(from, to, symbol)`. Is only available if the `det` feature is enabled.
#[cfg(feature = "det")]
pub mod transitionsystem;
#[cfg(feature = "det")]
pub use transitionsystem::TransitionSystem;

pub use visit::{LengthLexicographic, LengthLexicographicEdges, Visitor, VisitorIter};

/// A trait for the state index type. Implementors must be comparable, hashable, clonable and debuggable. The `create` method is used to create a new state index from a `u32`
pub trait StateIndex: Clone + PartialEq + Eq + std::hash::Hash + std::fmt::Debug + Ord {}

impl<X: Clone + Eq + PartialEq + std::hash::Hash + std::fmt::Debug + Ord> StateIndex for X {}

// The following two type aliases might change in the future to allow for more flexibility, i.e. for example for implementing nondeterminism.
/// Helper type for getting the symbol type of a transition system.
pub type InputOf<X> = <X as HasInput>::Sigma;
/// Helper type for getting the output type of a transition system.
pub type StateOf<X> = <X as HasStates>::Q;
/// Helper type for getting the trigger type of a transition system.
pub type TriggerOf<TS> = (StateOf<TS>, InputOf<TS>);
/// Helper type for getting the transition type of a transition system.
pub type TransitionOf<TS> = (StateOf<TS>, InputOf<TS>, StateOf<TS>);

/// Trait that encapsulates things which have a set of states. The states can be generic, as long as they implement the [`StateIndex`] trait.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait HasStates {
    /// The type of states of the object.
    type Q: StateIndex;
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

    /// Collect the produced sequence of transitions into a [`TransitionSystem`].
    fn into_ts(self) -> TransitionSystem<Self::Q, Self::Sigma> {
        self.into_transitions().collect()
    }

    /// Collect into a pair of [`TransitionSystem`] and initial state.
    fn into_pointed(self) -> (TransitionSystem<Self::Q, Self::Sigma>, Self::Q)
    where
        Self: Pointed,
    {
        let ts = self.into_ts();
        (ts, self.initial())
    }

    /// Collect into a [`MooreMachine`]. This is only possible if the object is
    /// [`Pointed`] and implements [`IntoAssignments`] where the domain is the state type.
    fn into_moore(self) -> MooreMachine<Self::Range, Self::Q, Self::Sigma>
    where
        Self: IntoAssignments<Domain = Self::Q> + Pointed,
    {
        MooreMachine::from_parts(self.into_ts(), self.initial(), self.collect_mapping())
    }

    /// Collect into a [`MealyMachine`]. This is only possible if the object is
    /// [`Pointed`] and implements [`IntoAssignments`] where the domain is the trigger type.
    fn into_mealy(self) -> MealyMachine<Self::Range, Self::Q, Self::Sigma>
    where
        Self: IntoAssignments<Domain = TriggerOf<Self>> + Pointed,
    {
        MealyMachine::from_parts(self.into_ts(), self.initial(), self.collect_mapping())
    }

    /// Computes the size of the transition system
    fn size(self) -> usize {
        self.into_transitions()
            .flat_map(|t| [t.source().clone(), t.target().clone()])
            .unique()
            .count()
    }
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
        on: InputOf<Self>,
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
    fn remove_transition(&mut self, from: Self::Q, on: InputOf<Self>) -> Option<Self::Q>;
}

/// A trait implemented by a [`TransitionSystem`] which can be trimmed. This means that all unreachable states are removed from the transition system. Further, all transitions which point to or originate from unreachable states are removed. Note that this operation is only applicable to a [`TransitionSystem`] which is [`Pointed`], as the concept of reachability is only defined if a designated initial state is given.
pub trait Trimmable: Successor + Pointed {
    /// The type of the trimmed transition system.
    type Trimmed: Successor<Q = Self::Q, Sigma = Self::Sigma> + Pointed;
    /// Removes all unreachable states from the transition system. Additionally removes any transitions which point to or originate from unreachable states.
    fn trim(&self) -> Self::Trimmed;
}

impl<TS: Successor> HasInput for (TS, TS::Q) {
    type Sigma = TS::Sigma;

    type Input<'me> =  TS::Input<'me> where Self:'me;

    fn raw_input_alphabet_iter(&self) -> Self::Input<'_> {
        self.0.raw_input_alphabet_iter()
    }
}
impl<TS: Successor> HasStates for (TS, TS::Q) {
    type Q = TS::Q;
}
impl<TS: Successor> Successor for (TS, TS::Q) {
    fn successor<X: Borrow<Self::Q>, Y: Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q> {
        self.0.successor(from, on)
    }
}
impl<TS: Successor> Pointed for (TS, TS::Q) {
    fn initial(&self) -> Self::Q {
        self.1.clone()
    }
}
