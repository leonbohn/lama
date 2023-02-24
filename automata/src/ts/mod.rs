use crate::Alphabet;

pub mod labels;

pub mod transition;
pub use transition::{Transition, TransitionTrigger};

mod state_index;
pub use state_index::StateIndex;

/// An implementation of a deterministic `TransitionSystem` in form of an edge list.
#[cfg(feature = "det")]
pub mod deterministic;

/// Implemented by objects which have a designated initial state.
pub trait Pointed<Q> {
    /// Get the initial state of the automaton.
    fn initial(&self) -> Q;
}

pub type SymbolFor<X> = <X as TransitionSystem>::S;
pub type OutputOf<X> = <X as TransitionSystem>::Q;

pub trait TransitionSystem {
    type Q: StateIndex;
    type S: Alphabet;
    type Trigger: TransitionTrigger<S = SymbolFor<Self>, Q = Self::Q>;

    fn succ(&self, from: &Self::Q, on: &SymbolFor<Self>) -> Option<OutputOf<Self>>;
}

pub trait FiniteState: TransitionSystem {
    fn states(&self) -> Vec<Self::Q>;
    fn size(&self) -> usize;
}

pub trait IntoStateReferences<'a>: TransitionSystem + 'a {
    type Output: Iterator<Item = &'a Self::Q>;
    fn into_state_references(self) -> Self::Output;
}

pub trait Growable: TransitionSystem {
    /// Add a new state to the transition system..
    fn add_state(&mut self) -> Self::Q;

    /// Add a new transition to the transition system. If the transition did not exist before, `None` is returned. Otherwise, the old target state is returned.
    fn add_transition<X: AsRef<SymbolFor<Self>>>(
        &mut self,
        from: Self::Q,
        on: SymbolFor<Self>,
        to: Self::Q,
    ) -> Option<Self::Q>;
}

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

pub trait Trimable: TransitionSystem {
    /// Removes all unreachable states from the transition system. Additionally removes any transitions which point to or originate from unreachable states.
    fn trim(&mut self);
}
