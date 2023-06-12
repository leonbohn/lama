use std::{borrow::Borrow, fmt::Display, ops::Add};

use crate::{
    ts::{
        transitionsystem::{States, TransitionSystemAlphabetIter, Transitions},
        HasInput, HasStates, InputOf, IntoParts, IntoStates, IntoTransitions, StateOf,
        StateReference, TransitionReference, Trivial,
    },
    words::{FiniteLength, HasLength, Length},
    Growable, Map, Pointed, Set, Shrinkable, State, Str, Subword, Successor, Symbol,
    TransitionSystem, TriggerIterable, Word, DFA,
};
use itertools::Itertools;

/// Represents an equivalence class of a right congruence relation.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Class<S: Symbol>(pub Str<S>);

impl<S: Symbol> Class<S> {
    /// Returns the class associated with the empty word.
    pub fn epsilon() -> Self {
        Self(Str::epsilon())
    }

    /// Create a class for the given single letter.
    pub fn letter<L: Borrow<S>>(l: L) -> Self {
        Self(Str {
            symbols: vec![l.borrow().clone()],
        })
    }

    /// Returns an iterator over the elements/symbols of the class
    pub fn iter(&self) -> impl Iterator<Item = &S> + '_ {
        self.0.symbols.iter()
    }

    /// Appends the given symbol to the end of the class.
    pub fn push_back<X: Borrow<S>>(&mut self, with: X) {
        self.0.push_back(with)
    }

    /// Returns the raw string underlying the class.
    pub fn raw(&self) -> &Str<S> {
        &self.0
    }
}

impl<S: Symbol> From<Str<S>> for Class<S> {
    fn from(value: Str<S>) -> Self {
        Self(value)
    }
}

impl<S: Symbol> From<&Str<S>> for Class<S> {
    fn from(value: &Str<S>) -> Self {
        Self(value.clone())
    }
}

impl<S: Symbol> IntoIterator for Class<S> {
    type Item = S;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.symbols.into_iter()
    }
}

impl<S: Symbol> Ord for Class<S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.length()
            .cmp(&other.length())
            .then(self.iter().cmp(other.iter()))
    }
}

impl<S: Symbol> PartialOrd for Class<S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<S: Symbol> HasLength for Class<S> {
    type Len = FiniteLength;
    fn length(&self) -> Self::Len {
        self.0.length()
    }
}

impl<S: Symbol> Word for Class<S> {
    type S = S;

    fn nth<I: Into<usize>>(&self, index: I) -> Option<Self::S> {
        self.0.symbols.get(index.into()).cloned()
    }

    fn alphabet(&self) -> crate::Set<Self::S> {
        self.0.alphabet()
    }
}

impl<S: Symbol> Subword for Class<S> {
    type SuffixType = Class<S>;

    type PrefixType = Class<S>;

    fn prefix(&self, length: usize) -> Self::PrefixType {
        Class(self.0.prefix(length))
    }

    fn skip(&self, number: usize) -> Self::SuffixType {
        Class(self.0.skip(number))
    }
}

impl Class<char> {
    /// Turns a given displayable thing into a class.
    pub fn from_display<D: Display>(d: D) -> Self {
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

impl From<&str> for Class<char> {
    fn from(value: &str) -> Self {
        Self(Str::from_display(value))
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

impl<S: Symbol> Add<Class<S>> for Class<S> {
    type Output = Self;

    fn add(self, rhs: Class<S>) -> Self::Output {
        Class(
            self.0
                .symbols
                .iter()
                .chain(rhs.0.symbols.iter())
                .cloned()
                .collect(),
        )
    }
}

impl<S: Symbol> Add<&Class<S>> for Class<S> {
    type Output = Self;

    fn add(self, rhs: &Class<S>) -> Self::Output {
        Class(
            self.0
                .symbols
                .iter()
                .chain(rhs.0.symbols.iter())
                .cloned()
                .collect(),
        )
    }
}

impl<S: Symbol> Add<Class<S>> for &Class<S> {
    type Output = Class<S>;

    fn add(self, rhs: Class<S>) -> Self::Output {
        Class(
            self.0
                .symbols
                .iter()
                .chain(rhs.0.symbols.iter())
                .cloned()
                .collect(),
        )
    }
}

impl<S: Symbol> Add<&Class<S>> for &Class<S> {
    type Output = Class<S>;

    fn add(self, rhs: &Class<S>) -> Self::Output {
        Class(
            self.0
                .symbols
                .iter()
                .chain(rhs.0.symbols.iter())
                .cloned()
                .collect(),
        )
    }
}

/// Alias for a transition in a right congruence relation.
pub type CongruenceTransition<S> = (Class<S>, S, Class<S>);
/// Alias for a trigger in a right congruence relation.
pub type CongruenceTrigger<S> = (Class<S>, S);

/// Represents a right congruence relation, which is in essence just a deterministic transition system. The only notable difference is that a right congruence per default encodes an initial state, namely that belonging to the epsilon class (see [`Class::epsilon`]]).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RightCongruence<S: Symbol = char>(pub TransitionSystem<Class<S>, S>, pub Class<S>);

impl<S: Symbol> HasStates for RightCongruence<S> {
    type Q = Class<S>;

    fn contains_state<X: Borrow<Self::Q>>(&self, state: X) -> bool {
        self.0.contains_state(state)
    }
}

impl<S: Symbol> RightCongruence<S> {
    /// Builds a new empty right congruence relation consisting of a single initial
    /// congruence class, the epsilon class.
    pub fn empty_trivial() -> Self {
        let mut ts = TransitionSystem::new();
        let eps = Class::epsilon();
        ts.add_state(&eps);
        RightCongruence(ts, eps)
    }

    /// Iterates over the classes/states of a right congruence in canonical order (i.e. in the order that they were created/inserted).
    pub fn states_canonical(&self) -> impl Iterator<Item = &Class<S>> + '_ {
        self.0.into_states().sorted()
    }

    /// Obtains the transition system underlying this right congruence.
    pub fn extract_ts(self) -> TransitionSystem<Class<S>, S> {
        self.0
    }

    /// Constructs a right congruence relation from its constituent parts, i.e. from a
    /// [`TransitionSystem`] which uses [`Class`]es as states and an initial state.
    pub fn from_parts(ts: TransitionSystem<Class<S>, S>, initial: Class<S>) -> Self {
        Self(ts, initial)
    }

    /// Returns the set of all potential triggers in `self` over the given `alphabet`.
    /// This operation essentially boils down to combining each state with all alphabet
    /// symbols.
    pub fn all_potential_triggers<I: IntoIterator<Item = S>>(
        &self,
        alphabet: I,
    ) -> Set<(Class<S>, S)> {
        let alphabet: Set<_> = alphabet.into_iter().collect();
        self.states_canonical()
            .flat_map(|class| {
                alphabet
                    .iter()
                    .map(move |symbol| (class.clone(), symbol.clone()))
            })
            .collect()
    }
}

impl<S: Symbol> HasInput for RightCongruence<S> {
    type Sigma = S;

    type Input<'me> = itertools::Unique<TransitionSystemAlphabetIter<'me, StateOf<Self>, S>> where Self:'me;

    fn input_alphabet(&self) -> Self::Input<'_> {
        self.0.input_alphabet()
    }
}

impl<S: Symbol> Successor for RightCongruence<S> {
    fn successor<X: Borrow<Self::Q>, Y: Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q> {
        self.0.successor(from, on)
    }
}

impl<S: Symbol> Pointed for RightCongruence<S> {
    fn initial(&self) -> Self::Q {
        Class::epsilon()
    }
}

impl<S: Symbol> Trivial for RightCongruence<S> {
    fn trivial() -> Self {
        let mut det = TransitionSystem::new();
        det.add_state(&Class::epsilon());
        Self(det, Class::epsilon())
    }
}

impl<'a, S: Symbol> IntoStates for &'a RightCongruence<S> {
    type StateRef = &'a Class<S>;

    type IntoStates = States<'a, Class<S>>;

    fn into_states(self) -> Self::IntoStates {
        self.0.into_states()
    }
}

impl<'a, S: Symbol> IntoTransitions for &'a RightCongruence<S> {
    type TransitionRef = TransitionReference<'a, Class<S>, S>;

    type IntoTransitions = Transitions<'a, Class<S>, S>;

    fn into_transitions(self) -> Self::IntoTransitions {
        self.0.into_transitions()
    }
}

impl<S: Symbol> Growable for RightCongruence<S> {
    fn add_state(&mut self, state: &Self::Q) -> bool {
        self.0.add_state(state)
    }

    fn add_transition<X: std::borrow::Borrow<Self::Q>, Y: std::borrow::Borrow<Self::Q>>(
        &mut self,
        from: X,
        on: InputOf<Self>,
        to: Y,
    ) -> Option<Self::Q> {
        self.0.add_transition(from, on, to)
    }
}

impl<S: Symbol> Shrinkable for RightCongruence<S> {
    fn remove_state(&mut self, state: Self::Q) -> Option<Self::Q> {
        self.0.remove_state(state)
    }

    fn remove_transition(&mut self, from: Self::Q, on: InputOf<Self>) -> Option<Self::Q> {
        self.0.remove_transition(from, on)
    }
}

impl<S: Symbol> TriggerIterable for RightCongruence<S> {
    type TriggerIter<'me> = std::collections::hash_map::Keys<'me, CongruenceTrigger<S>, Class<S>> where  S: 'me;

    fn triggers_iter(&self) -> Self::TriggerIter<'_> {
        self.0.triggers_iter()
    }
}

impl<S: Symbol> FromIterator<(Class<S>, S, Class<S>)> for RightCongruence<S> {
    fn from_iter<T: IntoIterator<Item = (Class<S>, S, Class<S>)>>(iter: T) -> Self {
        Self(TransitionSystem::from_iter(iter), Class::epsilon())
    }
}

impl<S: Symbol> FromIterator<(Str<S>, S, Str<S>)> for RightCongruence<S> {
    fn from_iter<T: IntoIterator<Item = (Str<S>, S, Str<S>)>>(iter: T) -> RightCongruence<S> {
        iter.into_iter()
            .map(|(from, on, to)| (Class::from(from), on, Class::from(to)))
            .collect()
    }
}

/// Encapsulates a special type of right congruence relation which is can be used to build
/// family of right congruences (FORC), which is a special kind of acceptor for omega languages.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProgressRightCongruence<S: Symbol>(Class<S>, RightCongruence<S>);

impl<S: Symbol> HasStates for ProgressRightCongruence<S> {
    type Q = Class<S>;

    fn contains_state<X: Borrow<Self::Q>>(&self, state: X) -> bool {
        self.1.contains_state(state)
    }
}

impl<S: Symbol> HasInput for ProgressRightCongruence<S> {
    type Sigma = S;

    type Input<'me> = itertools::Unique<TransitionSystemAlphabetIter<'me, StateOf<Self>, S>>
    where Self:'me;

    fn input_alphabet(&self) -> Self::Input<'_> {
        self.1.input_alphabet()
    }
}

impl<S: Symbol> Successor for ProgressRightCongruence<S> {
    fn successor<X: Borrow<Self::Q>, Y: Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q> {
        self.1.successor(from, on)
    }
}

impl<S: Symbol> Growable for ProgressRightCongruence<S> {
    fn add_state(&mut self, state: &Self::Q) -> bool {
        self.1.add_state(state)
    }

    fn add_transition<X: std::borrow::Borrow<Self::Q>, Y: std::borrow::Borrow<Self::Q>>(
        &mut self,
        from: X,
        on: InputOf<Self>,
        to: Y,
    ) -> Option<Self::Q> {
        self.1.add_transition(from, on, to)
    }
}

/// Encapsulates a family of right congruences (FORC), which is a special kind of acceptor for omega languages.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FORC<S: Symbol>(RightCongruence<S>, Map<Class<S>, RightCongruence<S>>);

impl<S: Symbol> Display for RightCongruence<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Congruence: [[\n{}\n]]", self.0)
    }
}

impl<S: Symbol> Display for Class<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0)
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{
        ts::{HasStates, Trivial},
        Growable, Pointed,
    };

    use super::{Class, RightCongruence};

    pub fn easy_cong() -> RightCongruence {
        let mut ts = RightCongruence::trivial();
        let q0 = ts.initial();
        let q1 = Class::from_display("b");
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
        let states: Vec<_> = cong.0.states().sorted().collect();
        assert_eq!(states, vec![&Class::epsilon(), &Class::from_display("b")])
    }
}
