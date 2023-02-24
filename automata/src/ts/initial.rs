use super::TransitionSystem;

/// Implemented by objects which have a designated initial state.
pub trait Pointed: TransitionSystem {
    /// Get the initial state of the automaton.
    fn initial(&self) -> Self::Q;
}

#[derive(Debug, Clone, Eq, PartialEq)]
/// Implemented by objects which have a designated initial state.
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
