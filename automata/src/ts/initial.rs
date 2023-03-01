use super::{HasTransitionSystem, TransitionSystem};

/// Implemented by objects which have a designated initial state.
pub trait Pointed: TransitionSystem {
    /// Get the initial state of the automaton.
    fn initial(&self) -> Self::Q;
}

/// Allows us to add an initial state to an existing transition system, is used in [`TransitionSystem::with_initial`].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WithInitial<'ts, TS: TransitionSystem>(pub &'ts TS, pub TS::Q);

impl<'ts, TS: TransitionSystem> TransitionSystem for WithInitial<'ts, TS> {
    type Q = TS::Q;
    type S = TS::S;
    type Trigger = TS::Trigger;
    fn succ(&self, from: &Self::Q, on: &super::SymbolOf<Self>) -> Option<super::StateOf<Self>> {
        self.0.succ(from, on)
    }
}

impl<'ts, TS: TransitionSystem> Pointed for WithInitial<'ts, TS> {
    fn initial(&self) -> Self::Q {
        self.1.clone()
    }
}

/// Helper trait that is implemented by wrappers around transition systems that add an initial state.
pub trait HasInitialState: HasTransitionSystem {
    /// Get the initial state.
    fn initial(&self) -> <<Self as HasTransitionSystem>::TransitionSystem as TransitionSystem>::Q;
}

impl<HIS: HasInitialState> Pointed for HIS {
    fn initial(&self) -> Self::Q {
        self.initial()
    }
}
