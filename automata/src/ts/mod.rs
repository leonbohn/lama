use crate::{acceptance::AcceptanceCondition, Alphabet, WithAcceptance};

mod initial;
pub use initial::{Pointed, WithInitial};

mod transition;
pub use transition::{Transition, TransitionTrigger};

mod state_index;
pub use state_index::StateIndex;

/// An implementation of a deterministic `TransitionSystem` in form of an edge list. The edge list is represented by a vector of tuples `(from, to, symbol)`. Is only available if the `det` feature is enabled.
#[cfg(feature = "det")]
pub mod deterministic;

// The following two type aliases might change in the future to allow for more flexibility, i.e. for example for implementing nondeterminism.
/// Helper type for getting the symbol type of a transition system.
pub type SymbolFor<X> = <X as TransitionSystem>::S;
/// Helper type for getting the output type of a transition system.
pub type OutputOf<X> = <X as TransitionSystem>::Q;

/// The base trait implemented by a deterministic transition system. A transition system is a tuple `(Q, S, δ)`, where `Q` is a finite set of states, `S` is a finite set of symbols and `δ: Q × S → Q` is a transition function. Note that the transition function is not necessarily complete and some transitions may be missing.
/// States of a transition system are generic, and can be any type that implements the [`StateIndex`] trait.
/// Also the symbols of a transition system are generic, and can be any type that implements the [`Alphabet`] trait.
/// The [`TransitionTrigger`] trait is used to represent an outgoing transition. Note, that such a trigger is a pair consisting of a state and a symbol, meaning the target state is not included in a trigger.
pub trait TransitionSystem {
    /// The type of the states of the transition system.
    type Q: StateIndex;
    /// The type of the symbols of the transition system.
    type S: Alphabet;
    /// The type of the triggers (i.e. outgoing edges without target) of the transition system.
    type Trigger: TransitionTrigger<S = SymbolFor<Self>, Q = Self::Q>
        + From<(Self::Q, SymbolFor<Self>)>;

    /// Returns the successor state of the given state on the given symbol. The transition function is deterministic, meaning that if a transition exists, it is unique. On the other hand there may not be a transition for a given state and symbol, in which case `succ` returns `None`.
    fn succ(&self, from: &Self::Q, on: &SymbolFor<Self>) -> Option<OutputOf<Self>>;

    /// Constructs an instance of [`WithInitial`] from the current transition system, which is a wrapper around the transition system that stores the given initial state `from`.
    fn with_initial(&self, from: Self::Q) -> WithInitial<Self>
    where
        Self: Sized,
    {
        WithInitial(self, from)
    }

    /// Constructs an instance of [`WithAcceptance`] from the current transition system, which is a wrapper around the transition system that stores the given acceptance condition `acc`.
    fn with_acceptance<Acc: AcceptanceCondition>(&self, acc: Acc) -> WithAcceptance<Acc, Self>
    where
        Self: Sized,
    {
        WithAcceptance::new(acc, self)
    }
}

/// Implemented by objects which have a finite number of states.
pub trait FiniteState: TransitionSystem {
    /// Returns a `Vec` of all states in the transition system.
    fn states(&self) -> Vec<Self::Q>;

    /// Gives the size of the transition system, i.e. the number of states.
    fn size(&self) -> usize;
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
    fn add_state(&mut self) -> Self::Q;

    /// Add a new transition to the transition system. If the transition did not exist before, `None` is returned. Otherwise, the old target state is returned.
    fn add_transition(
        &mut self,
        from: Self::Q,
        on: SymbolFor<Self>,
        to: Self::Q,
    ) -> Option<Self::Q>;
}

/// Implmenetors of this trait can be shrunk, i.e. states and transitions can be removed from the transition system.
pub trait Shrinkable: TransitionSystem {
    /// Deletes the given state from the transition system. If the state did not exist before, `None` is returned. Otherwise, the old state is returned.
    /// This method does not remove any transitions which point to the given state.
    fn remove_state(&mut self, state: Self::Q) -> Option<Self::Q>;

    /// Deletes the given transition from the transition system. If the transition did not exist before, `None` is returned. Otherwise, the old target state is returned.
    fn remove_transition<X: AsRef<SymbolFor<Self>>>(
        &mut self,
        from: Self::Q,
        on: SymbolFor<Self>,
    ) -> Option<Self::Q>;
}

/// A trait implemented by a [`TransitionSystem`] which can be trimmed. This means that all unreachable states are removed from the transition system. Further, all transitions which point to or originate from unreachable states are removed. Note that this operation is only applicable to a [`TransitionSystem`] which is [`Pointed`], as the concept of reachability is only defined if a designated initial state is given.
pub trait Trimable: TransitionSystem + Pointed {
    /// Removes all unreachable states from the transition system. Additionally removes any transitions which point to or originate from unreachable states.
    fn trim(&mut self);
}

/// Helper trait for implementing [`TransitionSystem`] for a type which contains a reference to a [`TransitionSystem`]. This is useful for example when implementing a wrapper around a transition system.
pub trait HasTransitionSystem {
    /// The type of the transition system.
    type TransitionSystem: TransitionSystem;
    /// Returns a reference to the transition system.
    fn transition_system(&self) -> &Self::TransitionSystem;
}

impl<T: HasTransitionSystem> TransitionSystem for T {
    type Q = <T::TransitionSystem as TransitionSystem>::Q;

    type S = SymbolFor<T::TransitionSystem>;

    type Trigger = <T::TransitionSystem as TransitionSystem>::Trigger;

    fn succ(&self, from: &Self::Q, on: &SymbolFor<Self>) -> Option<OutputOf<Self>> {
        self.transition_system().succ(from, on)
    }
}
