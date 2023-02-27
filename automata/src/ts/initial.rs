use crate::{
    run::{Walk, Walker},
    words::Word,
};

use super::TransitionSystem;

/// Implemented by objects which have a designated initial state.
pub trait Pointed: TransitionSystem {
    /// Get the initial state of the automaton.
    fn initial(&self) -> Self::Q;

    /// Start a new [`Walker`] from the initial state.
    fn walk<'ts, 'w, W: Word<S = Self::S>>(&'ts self, on: &'w W) -> Walker<'ts, 'w, W, Self>
    where
        Self: Sized,
    {
        self.walk_from_on(self.initial(), on)
    }
}

/// Allows us to add an initial state to an existing transition system, is used in [`TransitionSystem::with_initial`].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WithInitial<'ts, TS: TransitionSystem>(pub &'ts TS, pub TS::Q);

impl<'ts, TS: TransitionSystem> TransitionSystem for WithInitial<'ts, TS> {
    type Q = TS::Q;
    type S = TS::S;
    type Trigger = TS::Trigger;
    fn succ(&self, from: &Self::Q, on: &super::SymbolFor<Self>) -> Option<super::OutputOf<Self>> {
        self.0.succ(from, on)
    }
}

impl<'ts, TS: TransitionSystem> Pointed for WithInitial<'ts, TS> {
    fn initial(&self) -> Self::Q {
        self.1.clone()
    }
}
