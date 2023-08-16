use std::fmt::{Debug, Display};

use itertools::Itertools;

use crate::{
    alphabet::{HasAlphabet, Symbol},
    automaton::WithInitial,
    ts::{
        FiniteState, HasMutableStates, HasStates, IndexTS, OnStates, Sproutable, TransitionSystem,
    },
    Alphabet, FiniteLength, HasLength, Pointed, Successor, Word, DFA,
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Class<S>(pub Vec<S>);

impl<S> Class<S> {
    /// Creates an instance of the empty class
    pub fn epsilon() -> Self {
        Self(vec![])
    }
}

impl<S> FromIterator<S> for Class<S> {
    fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<S: Symbol> Display for Class<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0.iter().map(|sym| sym.show()).join(""))
    }
}
impl<S> HasLength for Class<S> {
    type Length = FiniteLength;

    fn length(&self) -> Self::Length {
        FiniteLength(self.0.len())
    }
}
impl<S: Symbol> Word for Class<S> {
    type Symbol = S;

    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        self.get(position).cloned()
    }
}

impl<S> std::ops::Deref for Class<S> {
    type Target = Vec<S>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<S> std::ops::DerefMut for Class<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl<S> Default for Class<S> {
    fn default() -> Self {
        Self(vec![])
    }
}
impl<S> From<Vec<S>> for Class<S> {
    fn from(value: Vec<S>) -> Self {
        Self(value)
    }
}
impl From<&str> for Class<char> {
    fn from(value: &str) -> Self {
        Self(value.chars().collect())
    }
}
impl<S: std::fmt::Debug> std::fmt::Debug for Class<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0.iter().map(|sym| format!("{:?}", sym)).join("")
        )
    }
}

impl<S: Ord> Ord for Class<S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .len()
            .cmp(&other.0.len())
            .then_with(|| self.0.cmp(&other.0))
    }
}
impl<S: Ord> PartialOrd for Class<S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct RightCongruence<A: Alphabet> {
    ts: IndexTS<A, Class<A::Symbol>, OnStates>,
}

impl<A: Alphabet> Debug for RightCongruence<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RightCongruence\n{:?}", self.ts)
    }
}

impl<A: Alphabet> RightCongruence<A> {
    fn recompute_labels(&mut self) {
        for (mr, id) in self
            .ts
            .minimal_representatives_from(self.initial())
            .collect_vec()
        {
            self.ts.set_state_color(id, mr.into());
        }
    }

    pub fn class_to_index(&self, class: &Class<A::Symbol>) -> Option<usize> {
        self.ts
            .indices_with_color()
            .find_map(|(id, c)| if c == class { Some(id) } else { None })
    }

    pub fn looping_words(&self, class: &Class<A::Symbol>) -> DFA<A> {
        self.map_colors(|c| &c == class)
            .collect_ts()
            .with_initial(self.class_to_index(class).unwrap())
    }
}

impl<A: Alphabet> FiniteState for RightCongruence<A> {
    fn state_indices(&self) -> Vec<Self::StateIndex> {
        self.ts.state_indices()
    }
}

impl<A: Alphabet> Pointed for RightCongruence<A> {
    fn initial(&self) -> Self::StateIndex {
        0
    }
}

impl<A: Alphabet> Sproutable for RightCongruence<A> {
    fn add_state(&mut self, color: crate::ts::StateColor<Self>) -> Self::StateIndex {
        self.ts.add_state(color)
    }

    fn set_state_color(&mut self, index: Self::StateIndex, color: crate::ts::StateColor<Self>) {
        self.ts.set_state_color(index, color)
    }

    fn add_edge<X, Y>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: crate::ts::EdgeColor<Self>,
    ) -> crate::ts::EdgeIndex
    where
        X: Into<Self::StateIndex>,
        Y: Into<Self::StateIndex>,
    {
        self.ts.add_edge(from, on, to, color)
    }

    fn undo_add_edge(&mut self) {
        self.ts.undo_add_edge()
    }

    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        let mut ts = IndexTS::new_for_alphabet(alphabet);
        let initial = ts.add_state(Class::epsilon());
        Self { ts }
    }
}

impl<A: Alphabet> HasAlphabet for RightCongruence<A> {
    type Alphabet = A;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}

impl<A: Alphabet> Successor for RightCongruence<A> {
    type StateIndex = usize;

    type Position = OnStates;

    type Color = Class<A::Symbol>;

    fn successor(
        &self,
        state: Self::StateIndex,
        symbol: crate::alphabet::SymbolOf<Self>,
    ) -> Option<
        crate::ts::Transition<
            Self::StateIndex,
            crate::alphabet::SymbolOf<Self>,
            crate::ts::EdgeColor<Self>,
        >,
    > {
        self.ts.successor(state, symbol)
    }

    fn state_color(&self, state: Self::StateIndex) -> crate::ts::StateColor<Self> {
        self.ts.state_color(state)
    }
}

impl<A: Alphabet> RightCongruence<A> {
    pub fn new(alphabet: A) -> Self {
        Self::new_for_alphabet(alphabet)
    }
}
