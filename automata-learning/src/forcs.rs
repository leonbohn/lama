use std::fmt::Display;

use automata::{
    ts::{SymbolOf, Trivial},
    Deterministic, FiniteWord, Growable, InitializedDeterministic, Mapping, Pointed, Symbol,
    TransitionSystem,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CongruenceClass<S>(pub FiniteWord<S>);

impl<S: Symbol> CongruenceClass<S> {
    pub fn epsilon() -> Self {
        Self(FiniteWord::empty())
    }
}

impl<D: Display> From<D> for CongruenceClass<char> {
    fn from(d: D) -> Self {
        Self(FiniteWord::from_display(d))
    }
}

pub type CongruenceTransition<S> = (CongruenceClass<S>, S, CongruenceClass<S>);
pub type CongruenceTrigger<S> = (CongruenceClass<S>, S);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RightCongruence<S: Symbol>(InitializedDeterministic<CongruenceClass<S>, S>);

impl<S: Symbol> TransitionSystem for RightCongruence<S> {
    type Q = CongruenceClass<S>;

    type S = S;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.0.succ(from, on)
    }
}

impl<S: Symbol> Pointed for RightCongruence<S> {
    fn initial(&self) -> Self::Q {
        CongruenceClass::epsilon()
    }
}

impl<S: Symbol> Trivial for RightCongruence<S> {
    fn trivial() -> Self {
        Self((Deterministic::new(), CongruenceClass::epsilon()).into())
    }
}

impl<S: Symbol> Growable for RightCongruence<S> {
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
pub struct ProgressRightCongruence<S: Symbol>(CongruenceClass<S>, RightCongruence<S>);

impl<S: Symbol> TransitionSystem for ProgressRightCongruence<S> {
    type Q = CongruenceClass<S>;

    type S = S;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.1.succ(from, on)
    }
}

impl<S: Symbol> Growable for ProgressRightCongruence<S> {
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
pub struct FORC<S: Symbol>(
    RightCongruence<S>,
    Mapping<CongruenceClass<S>, RightCongruence<S>>,
);
