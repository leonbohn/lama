use std::fmt::Display;

use automata::{
    ts::{SymbolOf, Trivial},
    Deterministic, Growable, InitializedDeterministic, Mapping, Pointed, Str, Symbol,
    TransitionSystem,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Class<S>(pub Str<S>);

impl<S: Symbol> Class<S> {
    pub fn epsilon() -> Self {
        Self(Str::empty())
    }
}

impl<D: Display> From<D> for Class<char> {
    fn from(d: D) -> Self {
        Self(Str::from_display(d))
    }
}

pub type CongruenceTransition<S> = (Class<S>, S, Class<S>);
pub type CongruenceTrigger<S> = (Class<S>, S);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RightCongruence<S: Symbol>(InitializedDeterministic<Class<S>, S>);

impl<S: Symbol> TransitionSystem for RightCongruence<S> {
    type Q = Class<S>;

    type S = S;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.0.succ(from, on)
    }
}

impl<S: Symbol> Pointed for RightCongruence<S> {
    fn initial(&self) -> Self::Q {
        Class::epsilon()
    }
}

impl<S: Symbol> Trivial for RightCongruence<S> {
    fn trivial() -> Self {
        Self((Deterministic::new(), Class::epsilon()).into())
    }
}

impl<S: Symbol> Growable for RightCongruence<S> {
    fn add_state(&mut self, state: &Self::Q) -> bool {
        self.0.add_state(state)
    }

    fn add_transition<X: std::borrow::Borrow<Self::Q>, Y: std::borrow::Borrow<Self::Q>>(
        &mut self,
        from: X,
        on: SymbolOf<Self>,
        to: Y,
    ) -> Option<Self::Q> {
        self.0.add_transition(from, on, to)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProgressRightCongruence<S: Symbol>(Class<S>, RightCongruence<S>);

impl<S: Symbol> TransitionSystem for ProgressRightCongruence<S> {
    type Q = Class<S>;

    type S = S;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.1.succ(from, on)
    }
}

impl<S: Symbol> Growable for ProgressRightCongruence<S> {
    fn add_state(&mut self, state: &Self::Q) -> bool {
        self.1.add_state(state)
    }

    fn add_transition<X: std::borrow::Borrow<Self::Q>, Y: std::borrow::Borrow<Self::Q>>(
        &mut self,
        from: X,
        on: SymbolOf<Self>,
        to: Y,
    ) -> Option<Self::Q> {
        self.1.add_transition(from, on, to)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FORC<S: Symbol>(RightCongruence<S>, Mapping<Class<S>, RightCongruence<S>>);
