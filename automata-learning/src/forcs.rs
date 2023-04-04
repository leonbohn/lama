use std::fmt::Display;

use automata::{
    ts::{SymbolOf, Trivial},
    Deterministic, Growable, InitializedDeterministic, Mapping, Pointed, Str, Symbol,
    TransitionSystem,
};
use itertools::Itertools;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Class<S: Symbol>(pub Str<S>);

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
pub struct RightCongruence<S: Symbol = char>(InitializedDeterministic<Class<S>, S>);

impl<S: Symbol> RightCongruence<S> {
    /// Builds a new empty right congruence relation consisting of a single initial
    /// congruence class, the epsilon class.
    pub fn empty_trivial() -> Self {
        let mut ts = Deterministic::new();
        let eps = Class::epsilon();
        ts.add_state(&eps);
        Self((ts, eps).into())
    }

    pub fn states_canonical(&self) -> impl Iterator<Item = &Class<S>> + '_ {
        self.0.states().sorted()
    }
}

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
        let mut det = Deterministic::new();
        det.add_state(&Class::epsilon());
        Self((det, Class::epsilon()).into())
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

#[cfg(test)]
mod tests {
    use automata::{ts::Trivial, Growable, Pointed};

    use crate::forcs::{Class, RightCongruence};

    pub fn easy_cong() -> RightCongruence {
        let mut ts = RightCongruence::trivial();
        let q0 = ts.initial();
        let q1 = "b".into();
        assert!(ts.add_state(&q1));
        ts.add_transition(&q0, 'a', &q0);
        ts.add_transition(&q1, 'a', &q1);
        ts.add_transition(&q0, 'b', &q1);
        ts.add_transition(&q1, 'b', &q0);
        ts
    }

    #[test]
    fn state_ordering_test() {
        let cong = easy_cong();
        let states: Vec<_> = cong.states_canonical().collect();
        assert_eq!(states, vec![&Class::epsilon(), &"b".into()])
    }
}
