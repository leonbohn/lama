use std::fmt::{Debug, Display};

use itertools::{Itertools, MapInto};

use crate::{
    alphabet::{Simple, Symbol},
    prelude::DFALike,
    ts::{transition_system::Indexes, Deterministic, Sproutable, BTS},
    word::FiniteWord,
    Alphabet, Color, FiniteLength, HasLength, Map, Pointed, Show, TransitionSystem, DFA,
};

mod class;
pub use class::{Class, ColoredClass};

mod forc;
pub use forc::FORC;

mod transitionprofile;
pub use transitionprofile::{Accumulates, RunProfile, RunSignature, TransitionMonoid};

mod cayley;

/// A right congruence is an equivalence relation that is compatible with the right concatenation. We
/// represent these as a transition system, where the states are the equivalence classes and the colors
/// on edges are `()`.
#[derive(Clone, Eq, PartialEq)]
pub struct RightCongruence<A: Alphabet, Q = (), C: Color = ()> {
    ts: BTS<A, ColoredClass<A::Symbol, Q>, C>,
}

impl<S: Symbol + Show, Q: Show> Show for ColoredClass<S, Q> {
    fn show(&self) -> String {
        format!("{} | {}", self.class.show(), self.color.show())
    }

    fn show_collection<'a, I>(iter: I) -> String
    where
        Self: 'a,
        I: IntoIterator<Item = &'a Self>,
        I::IntoIter: DoubleEndedIterator,
    {
        todo!()
    }
}

impl<A: Alphabet, Q: Color + Show, C: Color + Show> Debug for RightCongruence<A, Q, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RightCongruence\n{:?}", self.ts)
    }
}

impl<A: Alphabet, Q: Color, C: Color> RightCongruence<A, Q, C> {
    /// Assumes that `self` is det. and complete.
    pub fn congruent<W, V>(&self, word: W, other: V) -> bool
    where
        W: FiniteWord<A::Symbol>,
        V: FiniteWord<A::Symbol>,
    {
        self.reached(word).unwrap() == self.reached(other).unwrap()
    }

    /// Turns the given transition system into a right congruence.
    pub fn from_ts<Ts: Pointed + Deterministic<Alphabet = A, StateColor = Q, EdgeColor = C>>(
        ts: Ts,
    ) -> Self {
        let mut cong = Self {
            ts: ts
                .map_state_colors(|c| ColoredClass::new(Class::default(), c))
                .collect_ts(),
        };
        cong.recompute_labels();
        cong
    }

    /// Verifies whether an element of `self` is  idempotent, i.e. if the mr of the indexed
    /// class is u, then it should be that uu ~ u.
    pub fn is_idempotent<I: Indexes<Self>>(&self, elem: I) -> bool {
        if let Some(q) = self.get(elem) {
            self.reached_state_index_from(self.state_color(q).unwrap().class(), q) == Some(q)
        } else {
            false
        }
    }

    /// Returns an iterator which yields pairs `(c, idx)` consisting of a reference `c` to the class name together
    /// with the corresponding index of the class.
    pub fn classes(&self) -> impl Iterator<Item = (&Class<A::Symbol>, usize)> + '_ {
        self.ts.indices_with_color().map(|(id, c)| (c.class(), id))
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

    #[inline(always)]
    /// Returns the index of the class containing the given word.
    pub fn class_to_index(&self, class: &Class<A::Symbol>) -> Option<usize> {
        self.ts
            .indices_with_color()
            .find_map(|(id, c)| if c.class() == class { Some(id) } else { None })
    }

    pub fn class_name<Idx: Indexes<Self>>(&self, index: Idx) -> Option<ColoredClass<A::Symbol, Q>> {
        self.ts().state_color(index.to_index(self)?)
    }

    /// Computes a DFA that accepts precisely those finite words which loop on the given `class`. Formally,
    /// if `u` represents the given class, then the DFA accepts precisely those words `w` such that `uw`
    /// is congruent to `u`.
    pub fn looping_words(&self, class: &Class<A::Symbol>) -> DFA<A> {
        self.map_state_colors(|c: ColoredClass<A::Symbol, Q>| c.class() == class)
            .erase_edge_colors()
            .collect_ts()
            .with_initial(self.class_to_index(class).unwrap())
            .into_dfa()
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

    type ExtendStateIndexIter = std::ops::Range<usize>;

    fn extend_states<I: IntoIterator<Item = crate::ts::StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter {
        self.ts_mut().extend_states(iter)
    }
}

impl<A: Alphabet, Q: Color + Default, C: Color> RightCongruence<A, Q, C> {
    /// Creates a new [`RightCongruence`] for the given alphabet.
    pub fn new(alphabet: A) -> Self {
        Self::new_for_alphabet(alphabet)
    }
}
