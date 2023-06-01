use std::{borrow::Borrow, fmt::Display};

use crate::{
    helpers::MooreMachine, output::IntoAssignments, run::Configuration, Combined, MealyMachine,
    RightCongruence, Set, Str, Symbol, Word, DFA,
};

mod restricted;
mod successor;
mod transition;
mod visit;

use tracing::trace;
pub use visit::tarjan_scc;

pub use restricted::Restricted;

pub use successor::{Predecessor, Successor};

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

pub use visit::{Bfs, Path, Visitor, VisitorIter};

use self::transitionsystem::{States, Transitions};

/// A trait for the state index type. Implementors must be comparable, hashable, clonable and debuggable. The `create` method is used to create a new state index from a `u32`
pub trait State:
    Clone + Display + PartialEq + Eq + std::hash::Hash + std::fmt::Debug + Ord
{
}

impl<X: Clone + Eq + PartialEq + std::hash::Hash + std::fmt::Debug + Display + Ord> State for X {}

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
    type Q: State;

    /// Returns true if the object contanins the particular state.
    fn contains_state<X: Borrow<Self::Q>>(&self, state: X) -> bool;
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
    fn input_alphabet(&self) -> Self::Input<'_>;
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

type SccWitness<TS> = (Str<InputOf<TS>>, Str<InputOf<TS>>);
/// Converts the given object in to an iterator over states, consumes self.
pub trait IntoStates: Successor + Copy {
    /// The type of state output by the iterator.
    type StateRef: StateReference<Q = Self::Q>;
    /// The type of the iterator.
    type IntoStates: Iterator<Item = Self::StateRef>;

    /// Converts the transition system into an iterator over its states.
    fn into_states(self) -> Self::IntoStates;

    /// Decomposes the transition system into its strongly connected components.
    fn sccs(self) -> Vec<Vec<Self::Q>>
    where
        Self: Sized,
    {
        tarjan_scc(self)
    }

    /// Returns a vector containing for each SCC C in the transition system, a vector containing all
    /// transitions originating from a state in C and targeting a state in C.
    fn scc_transitions(self) -> Vec<Vec<TransitionOf<Self>>>
    where
        Self: Sized,
    {
        let alphabet = self.input_alphabet();
        let mut out = vec![];
        for scc in self.sccs() {
            let mut transitions_scc = vec![];
            for state in &scc {
                for transition in self.transitions_from(state) {
                    if scc.contains(transition.target()) {
                        transitions_scc.push(transition);
                    }
                }
            }
            out.push(transitions_scc);
        }
        out
    }

    /// Returns the strongly connected component containing the given state.
    fn scc_of(self, state: &Self::Q) -> Vec<Self::Q> {
        self.sccs()
            .into_iter()
            .find(|scc| scc.contains(state))
            .expect("It must occur in _some_ SCC!")
    }

    /// Returns the set of transition within the strongly connected component containing `state`.
    fn scc_transitions_of(self, state: &Self::Q) -> Vec<TransitionOf<Self>> {
        self.scc_transitions()
            .into_iter()
            .find(|scc| scc.iter().any(|t| t.source() == state))
            .expect("It must occur in _some_ SCC!")
    }

    /// Checks whether the given collection of states is a strongly connected component.
    fn is_scc<'a, I>(self, scc: I) -> bool
    where
        Self::Q: 'a,
        I: IntoIterator<Item = &'a Self::Q>,
        I::IntoIter: Clone,
        Self: Sized,
    {
        let sccs = self.sccs();
        let it = scc.into_iter();
        sccs.iter().any(|c| it.clone().all(|q| c.contains(q)))
    }

    /// Returns a path starting in `source` and reaching `target`, if it exists.
    fn path_from_to(self, source: &Self::Q, target: &Self::Q) -> Option<Str<InputOf<Self>>>
    where
        Self: Sized,
    {
        if source == target {
            return Some(Str::epsilon());
        }
        self.paths_from(source).iter().find_map(|path| {
            if path.reached() == target {
                Some(path.label().clone())
            } else {
                None
            }
        })
    }

    /// Returns a path starting in the initial state and reaching `target`, if it exists.
    fn path_to(self, target: &Self::Q) -> Option<Str<InputOf<Self>>>
    where
        Self: Sized + Pointed,
    {
        self.path_from_to(&self.initial(), target)
    }

    /// Constructs a witness for the given strongly connected component, that is a word which reaches
    /// the SCC and then returns to itself.
    fn scc_witness(self, scc: &[Self::Q]) -> Option<SccWitness<Self>>
    where
        Self: Sized + Pointed,
    {
        let mut it = scc.iter();
        let first = it.next()?;

        let reach = self.path_to(first)?;
        let mut witness = Str::epsilon();

        for state in it {
            witness += self.path_from_to(first, state)?;
            witness += self.path_from_to(state, first)?;
        }

        Some((reach, witness))
    }

    /// Constructs a witness for the given set of transitions, that is a pair (u,v) of finite words such
    /// that u reaches the source of the first transition and the word v loops back to the source of the
    /// first transition, while using all transitions in the given set.
    fn scc_transitions_witness(
        self,
        scc_transitions: &[TransitionOf<Self>],
    ) -> Option<SccWitness<Self>>
    where
        Self: Sized + Pointed,
    {
        trace!(
            "Computing witness for SCC transitions: {:?}",
            scc_transitions
        );
        let first = scc_transitions.first()?;
        trace!("First transition: {:?}", first);
        let mut current_state = first.source().clone();
        let reach = self.path_to(&current_state)?;
        trace!("Found path to first state: {:?}", reach);

        let mut witness = Str::epsilon();

        for transition in scc_transitions {
            trace!("Next transition: {:?}", transition);
            if transition.source() != &current_state {
                witness += self.path_from_to(&current_state, transition.source())?;
                trace!("Extended witness to {:?}", witness);
            }

            witness += transition.sym();
            trace!("Extended witness to {:?}", witness);
            current_state = transition.target().clone();
            trace!("Set current state to {:?}", current_state);
        }

        Some((reach, witness))
    }
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

/// Trait that allows decomposing a transition system into its parts, which are a set of states and a set of transitions.
/// This trait is implemented for all types that implement [`IntoTransitions`] and [`IntoStates`].
/// Its main purpose is to provide convenience methods for collecting into the following objects:
/// - [`TransitionSystem`] which ignores initial states and any other information,
/// - [`MooreMachine`] which assumes that the object is [`Pointed`] and has output at each state,
/// - [`MealyMachine`] which assumes that the object is [`Pointed`] and has output at each transition,
/// - and initialized [`TransitionSystem`] which assumes that the object is [`Pointed`] i.e. has a single initial state.
/// For collecting into Mealy and Moore machines the object must also implement [`IntoTransitions`].
pub trait IntoParts: IntoTransitions + IntoStates {
    fn reaching_words_dfa(self, from: Self::Q, to: Self::Q) -> DFA<Self::Q, Self::Sigma> {
        DFA::from_parts_iters(self.into_transitions(), [to], from)
    }

    /// Collect the produced sequence of transitions into a [`TransitionSystem`].
    fn into_ts(self) -> TransitionSystem<Self::Q, Self::Sigma> {
        let mut ts = TransitionSystem::from_iter(self.into_transitions());
        for state in self.into_states() {
            if !ts.contains_state(state.state()) {
                ts.add_state(&state.state());
            }
        }
        ts
    }

    /// Collect into a pair of [`TransitionSystem`] and initial state.
    fn into_pointed(self) -> (TransitionSystem<Self::Q, Self::Sigma>, Self::Q)
    where
        Self: Pointed,
    {
        let ts: TransitionSystem<<Self as HasStates>::Q, <Self as HasInput>::Sigma> =
            self.into_ts();
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

impl<TS: IntoStates + IntoTransitions> IntoParts for TS {}

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

    fn input_alphabet(&self) -> Self::Input<'_> {
        self.0.input_alphabet()
    }
}
impl<TS: Successor> HasStates for (TS, TS::Q) {
    type Q = TS::Q;

    fn contains_state<X: Borrow<Self::Q>>(&self, state: X) -> bool {
        self.0.contains_state(state)
    }
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
impl<'a, Q: State, S: Symbol> IntoStates for &'a (TransitionSystem<Q, S>, Q) {
    type StateRef = &'a Q;

    type IntoStates = States<'a, Q>;

    fn into_states(self) -> Self::IntoStates {
        self.0.into_states()
    }
}
impl<'a, Q: State, S: Symbol> IntoTransitions for &'a (TransitionSystem<Q, S>, Q) {
    type TransitionRef = TransitionReference<'a, Q, S>;

    type IntoTransitions = Transitions<'a, Q, S>;

    fn into_transitions(self) -> Self::IntoTransitions {
        self.0.into_transitions()
    }
}
