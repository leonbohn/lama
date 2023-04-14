use std::{fmt::Display, ops::Add};

use crate::{
    ts::{SymbolOf, Trivial},
    Deterministic, Growable, InitializedDeterministic, Mapping, Pointed, Shrinkable, StateIterable,
    Str, Symbol, TransitionSystem,
};
use itertools::Itertools;

/// Represents an equivalence class of a right congruence relation.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Class<S: Symbol>(pub Str<S>);

impl<S: Symbol> Class<S> {
    /// Returns the class associated with the empty word.
    pub fn epsilon() -> Self {
        Self(Str::empty())
    }
}

impl Class<char> {
    /// Turns a given displayable thing into a class.
    pub fn from<D: Display>(d: D) -> Self {
        Self(Str::from_display(d))
    }
}

impl<S: Symbol> Add<S> for Class<S> {
    type Output = Self;

    fn add(self, rhs: S) -> Self::Output {
        let mut extend = self.0;
        extend.symbols.push(rhs);
        Self(extend)
    }
}

impl<S: Symbol> Add<&S> for &Class<S> {
    type Output = Class<S>;

    fn add(self, rhs: &S) -> Self::Output {
        let mut extend = self.0.clone();
        extend.symbols.push(rhs.clone());
        Class(extend)
    }
}

impl<S: Symbol> Add<S> for &Class<S> {
    type Output = Class<S>;

    fn add(self, rhs: S) -> Self::Output {
        let mut extend = self.0.clone();
        extend.symbols.push(rhs);
        Class(extend)
    }
}

impl<S: Symbol> Add<&S> for Class<S> {
    type Output = Self;

    fn add(self, rhs: &S) -> Self::Output {
        let mut extend = self.0;
        extend.symbols.push(rhs.clone());
        Self(extend)
    }
}

/// Alias for a transition in a right congruence relation.
pub type CongruenceTransition<S> = (Class<S>, S, Class<S>);
/// Alias for a trigger in a right congruence relation.
pub type CongruenceTrigger<S> = (Class<S>, S);

/// Represents a right congruence relation, which is in essence just a deterministic transition system. The only notable difference is that a right congruence per default encodes an initial state, namely that belonging to the epsilon class (see [`Class::epsilon`]]).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RightCongruence<S: Symbol = char>(InitializedDeterministic<Class<S>, S>);

impl<S: Symbol> StateIterable for RightCongruence<S> {
    type StateIter<'me> = std::collections::hash_set::Iter<'me, Class<S>> where S: 'me;

    fn states_iter(&self) -> Self::StateIter<'_> {
        self.0.det.states.iter()
    }
}

impl<S: Symbol> RightCongruence<S> {
    /// Builds a new empty right congruence relation consisting of a single initial
    /// congruence class, the epsilon class.
    pub fn empty_trivial() -> Self {
        let mut ts = Deterministic::new();
        let eps = Class::epsilon();
        ts.add_state(&eps);
        Self((ts, eps).into())
    }

    /// Iterates over the classes/states of a right congruence in canonical order (i.e. in the order that they were created/inserted).
    pub fn states_canonical(&self) -> impl Iterator<Item = &Class<S>> + '_ {
        self.states_iter().sorted()
    }
}

impl<S: Symbol> TransitionSystem for RightCongruence<S> {
    type Q = Class<S>;

    type S = S;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.0.succ(from, on)
    }

    fn vec_alphabet(&self) -> Vec<Self::S> {
        self.0.vec_alphabet()
    }

    fn vec_states(&self) -> Vec<Self::Q> {
        self.0.vec_states()
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

impl<S: Symbol> Shrinkable for RightCongruence<S> {
    fn remove_state(&mut self, state: Self::Q) -> Option<Self::Q> {
        self.0.remove_state(state)
    }

    fn remove_transition(&mut self, from: Self::Q, on: SymbolOf<Self>) -> Option<Self::Q> {
        self.0.remove_transition(from, on)
    }
}

/// Encapsulates a special type of right congruence relation which is can be used to build
/// family of right congruences (FORC), which is a special kind of acceptor for omega languages.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProgressRightCongruence<S: Symbol>(Class<S>, RightCongruence<S>);

impl<S: Symbol> TransitionSystem for ProgressRightCongruence<S> {
    type Q = Class<S>;

    type S = S;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.1.succ(from, on)
    }

    fn vec_alphabet(&self) -> Vec<Self::S> {
        self.1.vec_alphabet()
    }

    fn vec_states(&self) -> Vec<Self::Q> {
        self.1.vec_states()
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

/// Encapsulates a family of right congruences (FORC), which is a special kind of acceptor for omega languages.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FORC<S: Symbol>(RightCongruence<S>, Mapping<Class<S>, RightCongruence<S>>);

impl<S: Symbol> Display for RightCongruence<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Congruence: [[ {} ]]", self.0)
    }
}

impl<S: Symbol> Display for Class<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0)
    }
}

#[cfg(test)]
mod tests {
    use crate::{ts::Trivial, Growable, Pointed};

    use super::{Class, RightCongruence};

    pub fn easy_cong() -> RightCongruence {
        let mut ts = RightCongruence::trivial();
        let q0 = ts.initial();
        let q1 = Class::from("b");
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
        assert_eq!(states, vec![&Class::epsilon(), &Class::from("b")])
    }
}
