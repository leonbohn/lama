use std::fmt::{Debug, Display};

use itertools::Itertools;

use crate::{
    alphabet::{HasAlphabet, Symbol},
    ts::{FiniteState, FiniteStatesIterType, HasFiniteStates, Sproutable, BTS},
    Alphabet, Color, FiniteLength, HasLength, Map, Pointed, TransitionSystem, Word, DFA,
};

/// Represents a congruence class, which is in essence simply a non-empty sequence of symbols
/// for the underlying alphabet.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Class<S>(pub Vec<S>);

impl<S> Class<S> {
    /// Creates an instance of the empty class
    pub fn epsilon() -> Self {
        Self(vec![])
    }

    /// Takes in a single symbol and returns a class containing only that symbol.
    pub fn singleton(sym: S) -> Self {
        Self(vec![sym])
    }

    /// Turns this class into a string, using the given alphabet to convert symbols to strings.
    pub fn mr_to_string(&self) -> String
    where
        S: Symbol,
    {
        if self.is_empty() {
            "ε".to_string()
        } else {
            self.0.iter().map(|sym| sym.show()).join("")
        }
    }
}

impl<S> FromIterator<S> for Class<S> {
    fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<S: Symbol> Display for Class<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            if self.0.is_empty() {
                "ε".to_string()
            } else {
                self.0.iter().map(|sym| sym.show()).join("")
            }
        )
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

/// A colored class is a [`Class`] which additionally has an associated color.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ColoredClass<S: Symbol, Q = ()> {
    class: Class<S>,
    color: Q,
}

impl<S: Symbol, Q: Debug> Display for ColoredClass<S, Q> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} | {:?}]", self.class, self.color)
    }
}

impl<S: Symbol, J: Into<Class<S>>> From<J> for ColoredClass<S, ()> {
    fn from(value: J) -> Self {
        Self {
            class: value.into(),
            color: (),
        }
    }
}

impl<S: Symbol, Q: Color> From<(Class<S>, Q)> for ColoredClass<S, Q> {
    fn from(value: (Class<S>, Q)) -> Self {
        Self {
            class: value.0,
            color: value.1,
        }
    }
}

impl<S: Symbol, Q: Color> ColoredClass<S, Q> {
    /// Creates a new colored class from the given class and color.
    pub fn new<X: Into<Class<S>>>(class: X, color: Q) -> Self {
        Self {
            class: class.into(),
            color,
        }
    }

    /// Returns a reference to the underlying class.
    pub fn class(&self) -> &Class<S> {
        &self.class
    }

    /// Consumes `self` and returns a [`ColoredClass`] with the same color but the given `class`.
    pub fn reclass<X: Into<Class<S>>>(self, class: X) -> ColoredClass<S, Q> {
        ColoredClass {
            class: class.into(),
            color: self.color,
        }
    }

    /// Consumes `self` and returns a [`ColoredClass`] with the same class but the given `color`.
    pub fn recolor<D: Color>(self, color: D) -> ColoredClass<S, D> {
        ColoredClass {
            class: self.class,
            color,
        }
    }

    /// Returns a reference to the stored color.
    pub fn color(&self) -> &Q {
        &self.color
    }
}

/// A right congruence is an equivalence relation that is compatible with the right concatenation. We
/// represent these as a transition system, where the states are the equivalence classes and the colors
/// on edges are `()`.
#[derive(Clone, Eq, PartialEq)]
pub struct RightCongruence<A: Alphabet, Q = (), C: Color = ()> {
    ts: BTS<A, ColoredClass<A::Symbol, Q>, C>,
}

impl<A: Alphabet, Q: Color + Debug, C: Color + Debug> Debug for RightCongruence<A, Q, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RightCongruence\n{:?}", self.ts)
    }
}

/// Implementors of this trait can be used as indices for the right congruence.
pub trait IndexesRightCongruence<A: Alphabet> {
    /// Turns `self` into an index for the given right congruence.
    fn to_index(&self, congruence: &RightCongruence<A>) -> Option<usize>;
}

impl<A: Alphabet> IndexesRightCongruence<A> for usize {
    fn to_index(&self, _congruence: &RightCongruence<A>) -> Option<usize> {
        Some(*self)
    }
}

impl<A: Alphabet> IndexesRightCongruence<A> for &Class<A::Symbol> {
    fn to_index(&self, congruence: &RightCongruence<A>) -> Option<usize> {
        congruence.class_to_index(self)
    }
}

impl<A: Alphabet, Q: Color, C: Color> RightCongruence<A, Q, C> {
    /// Turns the given transition system into a right congruence.
    pub fn from_ts<X: Into<ColoredClass<A::Symbol, Q>> + Color>(ts: BTS<A, X, C>) -> Self {
        Self {
            ts: ts.map_state_colors(|c| c.into()).collect_ts(),
        }
    }

    /// Returns a reference to the underlying [`TransitionSystem`].
    pub fn ts(&self) -> &BTS<A, ColoredClass<A::Symbol, Q>, C> {
        &self.ts
    }

    /// Returns a mutable reference to the underlying [`TransitionSystem`].
    pub fn ts_mut(&mut self) -> &mut BTS<A, ColoredClass<A::Symbol, Q>, C> {
        &mut self.ts
    }

    /// Gives a reference to the underlying alphabet.
    pub fn alphabet(&self) -> &A {
        self.ts.alphabet()
    }

    /// Recomputes the labels or [`Class`]es of the states in the right congruence.
    pub(crate) fn recompute_labels(&mut self) {
        for (mr, id) in self
            .ts
            .minimal_representatives_from(self.initial())
            .collect_vec()
        {
            self.ts
                .set_state_color(id, self.ts().state_color(id).unwrap().reclass(mr));
        }
    }

    /// Returns the index of the class containing the given word.
    pub fn class_to_index(&self, class: &Class<A::Symbol>) -> Option<usize> {
        self.ts
            .indices_with_color()
            .find_map(|(id, c)| if c.class() == class { Some(id) } else { None })
    }

    /// Computes a DFA that accepts precisely those finite words which loop on the given `class`. Formally,
    /// if `u` represents the given class, then the DFA accepts precisely those words `w` such that `uw`
    /// is congruent to `u`.
    pub fn looping_words(&self, class: &Class<A::Symbol>) -> DFA<A> {
        self.map_state_colors(|c| c.class() == class)
            .erase_edge_colors()
            .collect_ts()
            .with_initial(self.class_to_index(class).unwrap())
    }
}

impl<'a, A: Alphabet, Q: Color, C: Color> HasFiniteStates<'a> for RightCongruence<A, Q, C> {
    type StateIndicesIter = FiniteStatesIterType<'a, BTS<A, ColoredClass<A::Symbol, Q>, C, usize>>;
}

impl<A: Alphabet, Q: Color, C: Color> FiniteState for RightCongruence<A, Q, C> {
    fn state_indices(&self) -> FiniteStatesIterType<'_, Self> {
        self.ts.state_indices()
    }
}

impl<A: Alphabet, Q: Color, C: Color> Pointed for RightCongruence<A, Q, C> {
    fn initial(&self) -> Self::StateIndex {
        0
    }
}

impl<A: Alphabet, Q: Color + Default, C: Color> Sproutable for RightCongruence<A, Q, C> {
    fn add_state<X: Into<crate::ts::StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
        self.ts.add_state(color.into())
    }

    fn set_state_color<X: Into<crate::ts::StateColor<Self>>>(
        &mut self,
        index: Self::StateIndex,
        color: X,
    ) {
        self.ts.set_state_color(index, color.into())
    }

    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        let mut ts = BTS::new_for_alphabet(alphabet);
        let _initial = ts.add_state(ColoredClass::new(Class::epsilon(), Q::default()));
        Self { ts }
    }

    fn add_edge<X, Y>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: crate::ts::EdgeColor<Self>,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Into<Self::StateIndex>,
        Y: Into<Self::StateIndex>,
    {
        self.ts.add_edge(from, on, to, color)
    }

    fn remove_edge(
        &mut self,
        from: Self::StateIndex,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool {
        self.ts.remove_edge(from, on)
    }
}

impl<A: Alphabet, Q: Color, C: Color> HasAlphabet for RightCongruence<A, Q, C> {
    type Alphabet = A;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}

impl<A: Alphabet, Q: Color + Default, C: Color> RightCongruence<A, Q, C> {
    /// Creates a new [`RightCongruence`] for the given alphabet.
    pub fn new(alphabet: A) -> Self {
        Self::new_for_alphabet(alphabet)
    }
}

/// A family of right congruences (FORC) consists of a *leading* right congruence and for each
/// class of this congruence a *progress* right congruence.
#[derive(Clone, PartialEq, Eq)]
pub struct FORC<A: Alphabet, Q: Color = (), C: Color = ()> {
    pub(crate) leading: RightCongruence<A>,
    pub(crate) progress: Map<Class<A::Symbol>, RightCongruence<A, Q, C>>,
}

impl<A: Alphabet, Q: Color, C: Color> FORC<A, Q, C> {
    /// Creates a new FORC with the given leading congruence and progress congruences.
    pub fn new(
        leading: RightCongruence<A>,
        progress: Map<Class<A::Symbol>, RightCongruence<A, Q, C>>,
    ) -> Self {
        Self { leading, progress }
    }

    /// Insert a new progress congruence for the given class.
    pub fn insert(&mut self, class: Class<A::Symbol>, congruence: RightCongruence<A, Q, C>) {
        self.progress.insert(class, congruence);
    }

    /// Tries to obtain a reference to the progress right congruence for the given `class`.
    pub fn prc<D>(&self, class: D) -> Option<&RightCongruence<A, Q, C>>
    where
        D: std::borrow::Borrow<Class<A::Symbol>>,
    {
        self.progress.get(class.borrow())
    }

    /// Creates a new FORC from the given leading congruence and progress congruences.
    pub fn from_iter<I: IntoIterator<Item = (Class<A::Symbol>, RightCongruence<A, Q, C>)>>(
        leading: RightCongruence<A>,
        progress: I,
    ) -> Self {
        Self {
            leading,
            progress: progress.into_iter().collect(),
        }
    }
}
