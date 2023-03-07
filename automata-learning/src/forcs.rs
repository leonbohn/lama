use std::fmt::Display;

use automata::{
    ts::{SymbolOf, Trivial},
    Deterministic, Growable, InitializedDeterministic, Mapping, Pointed, TransitionSystem,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CongruenceClass(pub String);

impl<D: Display> From<D> for CongruenceClass {
    fn from(d: D) -> Self {
        Self(d.to_string())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RightCongruence(InitializedDeterministic<CongruenceClass, char>);

impl TransitionSystem for RightCongruence {
    type Q = CongruenceClass;

    type S = char;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.0.succ(from, on)
    }
}

impl Pointed for RightCongruence {
    fn initial(&self) -> Self::Q {
        "".into()
    }
}

impl Trivial for RightCongruence {
    fn trivial() -> Self {
        Self((Deterministic::new(), "".into()).into())
    }
}

impl Growable for RightCongruence {
    fn add_state(&mut self, state: &Self::Q) -> bool {
        self.0.add_state(state)
    }

    fn add_transition(
        &mut self,
        from: &Self::Q,
        on: SymbolOf<Self>,
        to: &Self::Q,
    ) -> Option<Self::Q> {
        self.0.add_transition(from, on, to)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProgressRightCongruence(CongruenceClass, RightCongruence);

impl TransitionSystem for ProgressRightCongruence {
    type Q = CongruenceClass;

    type S = char;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.1.succ(from, on)
    }
}

impl Growable for ProgressRightCongruence {
    fn add_state(&mut self, state: &Self::Q) -> bool {
        self.1.add_state(state)
    }

    fn add_transition(
        &mut self,
        from: &Self::Q,
        on: SymbolOf<Self>,
        to: &Self::Q,
    ) -> Option<Self::Q> {
        self.1.add_transition(from, on, to)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FORC(RightCongruence, Mapping<CongruenceClass, RightCongruence>);
