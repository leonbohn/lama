use crate::Symbol;

mod transition;
pub use transition::{Transition, Trigger};

/// An implementation of a deterministic `TransitionSystem` in form of an edge list. The edge list is represented by a vector of tuples `(from, to, symbol)`. Is only available if the `det` feature is enabled.
#[cfg(feature = "det")]
pub mod deterministic;
#[cfg(feature = "det")]
pub use deterministic::{Deterministic, InitializedDeterministic};

/// A trait for the state index type. Implementors must be comparable, hashable, clonable and debuggable. The `create` method is used to create a new state index from a `u32`.
pub trait StateIndex: Clone + Eq + std::hash::Hash + std::fmt::Debug {}

impl<X: Clone + Eq + std::hash::Hash + std::fmt::Debug> StateIndex for X {}

// The following two type aliases might change in the future to allow for more flexibility, i.e. for example for implementing nondeterminism.
/// Helper type for getting the symbol type of a transition system.
pub type SymbolOf<X> = <X as TransitionSystem>::S;
/// Helper type for getting the output type of a transition system.
pub type StateOf<X> = <X as TransitionSystem>::Q;

/// The base trait implemented by a deterministic transition system. A transition system is a tuple `(Q, S, δ)`, where `Q` is a finite set of states, `S` is a finite set of symbols and `δ: Q × S → Q` is a transition function. Note that the transition function is not necessarily complete and some transitions may be missing.
/// States of a transition system are generic, and can be any type that implements the [`StateIndex`] trait.
/// Also the symbols of a transition system are generic, and can be any type that implements the [`Alphabet`] trait.
/// The [`TransitionTrigger`] trait is used to represent an outgoing transition. Note, that such a trigger is a pair consisting of a state and a symbol, meaning the target state is not included in a trigger.
pub trait TransitionSystem {
    /// The type of the states of the transition system.
    type Q: StateIndex;
    /// The type of the symbols of the transition system.
    type S: Symbol;

    /// Returns the successor state of the given state on the given symbol. The transition function is deterministic, meaning that if a transition exists, it is unique. On the other hand there may not be a transition for a given state and symbol, in which case `succ` returns `None`.
    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q>;

    /// Returns the successor state for the given trigger through calling [`Self::succ`].
    fn apply_trigger(&self, trigger: &(Self::Q, Self::S)) -> Option<Self::Q> {
        self.succ(trigger.source(), trigger.sym())
    }

    /// Creates a new trigger from the given state and symbol.
    fn make_trigger(from: &Self::Q, on: &Self::S) -> (Self::Q, Self::S) {
        (from.clone(), on.clone())
    }
}

/// Creates a new trivial transition system, which could either be empty (for [`TransitionSystem`]) or contain a single initial state (for [`InitializedDeterministic`]).
pub trait Trivial: TransitionSystem {
    /// Creates the trivial object
    fn trivial() -> Self;
}

impl<TS: TransitionSystem> TransitionSystem for &TS {
    type Q = TS::Q;

    type S = TS::S;

    fn succ(&self, from: &Self::Q, on: &SymbolOf<Self>) -> Option<StateOf<Self>> {
        TransitionSystem::succ(*self, from, on)
    }
}

/// Implemented by objects which have a designated initial state.
pub trait Pointed: TransitionSystem {
    /// Get the initial state of the automaton.
    fn initial(&self) -> Self::Q;
}

/// Implemented by objects which have a finite number of states.
pub trait StateIterable: TransitionSystem {
    /// The state index type.
    type StateRef: StateIndex;
    /// Type of the iterator over all states.
    type StateIter: Iterator<Item = Self::StateRef>;

    /// Returns a `Vec` of all states in the transition system.
    fn states(&self) -> Self::StateIter;
}

/// Trait that allows iterating over all edges in a [`TransitionSystem`].
pub trait TransitionIterable: TransitionSystem {
    /// The edge type.
    type TransitionRef;
    /// Type of the iterator over all edges.
    type TransitionIter: Iterator<Item = Self::TransitionRef>;

    /// Returns an iterator over all edges in the transition system.
    fn edges(&self) -> Self::TransitionIter;
}

/// Trait that allows iterating over all triggers in a [`TransitionSystem`].
pub trait TriggerIterable: TransitionSystem {
    /// The trigger type.
    type TriggerRef;
    /// Type of the iterator over all triggers.
    type TriggerIter: Iterator<Item = Self::TriggerRef>;

    /// Returns an iterator over all triggers in the transition system.
    fn triggers(&self) -> Self::TriggerIter;
}

/// Converts the given transition system in to an Iterator over references to its states.
pub trait IntoStateReferences<'a>: TransitionSystem + 'a {
    /// The type of the iterator.
    type Output: Iterator<Item = &'a Self::Q>;
    /// Converts the transition system into an iterator over references to its states.
    fn into_state_references(self) -> Self::Output;
}

/// Ecapsulates the ability to add states and transitions to a transition system.
pub trait Growable: TransitionSystem {
    /// Add a new state to the transition system..
    fn add_state(&mut self, state: &Self::Q) -> bool;

    /// Add a new transition to the transition system. If the transition did not exist before, `None` is returned. Otherwise, the old target state is returned.
    fn add_transition(
        &mut self,
        from: &Self::Q,
        on: SymbolOf<Self>,
        to: &Self::Q,
    ) -> Option<Self::Q>;
}

/// Ecapsulates the ability to add anonymous states and transitions to a transition system.
pub trait AnonymousGrowable: Growable {
    /// Add a new state to the transition system..
    fn add_new_state(&mut self) -> Self::Q;
}

/// Implmenetors of this trait can be shrunk, i.e. states and transitions can be removed from the transition system.
pub trait Shrinkable: TransitionSystem {
    /// Deletes the given state from the transition system. If the state did not exist before, `None` is returned. Otherwise, the old state is returned.
    /// This method does not remove any transitions which point to the given state.
    fn remove_state(&mut self, state: Self::Q) -> Option<Self::Q>;

    /// Deletes the given transition from the transition system. If the transition did not exist before, `None` is returned. Otherwise, the old target state is returned.
    fn remove_transition(&mut self, from: Self::Q, on: SymbolOf<Self>) -> Option<Self::Q>;
}

/// A trait implemented by a [`TransitionSystem`] which can be trimmed. This means that all unreachable states are removed from the transition system. Further, all transitions which point to or originate from unreachable states are removed. Note that this operation is only applicable to a [`TransitionSystem`] which is [`Pointed`], as the concept of reachability is only defined if a designated initial state is given.
pub trait Trimable: TransitionSystem + Pointed {
    /// Removes all unreachable states from the transition system. Additionally removes any transitions which point to or originate from unreachable states.
    fn trim(&mut self);
}
