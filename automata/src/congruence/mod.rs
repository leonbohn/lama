use std::fmt::{Debug, Display};

use itertools::Itertools;

use crate::{
    alphabet::{HasAlphabet, Simple, Symbol},
    ts::{
        transition_system::Indexes, FiniteState, FiniteStatesIterType, HasFiniteStates, Sproutable,
        BTS,
    },
    Alphabet, Color, FiniteLength, HasLength, Map, Pointed, TransitionSystem, Word, DFA,
};

mod class;
pub use class::{Class, ColoredClass};

mod forc;
pub use forc::FORC;

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

impl<A: Alphabet, Q: Color, C: Color> RightCongruence<A, Q, C> {
    /// Turns the given transition system into a right congruence.
    pub fn from_ts<X: Into<ColoredClass<A::Symbol, Q>> + Color>(ts: BTS<A, X, C>) -> Self {
        Self {
            ts: ts.map_state_colors(|c| c.into()).collect_ts(),
        }
    }

    /// Verifies whether an element of `self` is  idempotent, i.e. if the mr of the indexed
    /// class is u, then it should be that uu ~ u.
    pub fn is_idempotent<I: Indexes<Self>>(&self, elem: I) -> bool {
        if let Some(q) = self.get(elem) {
            self.reached_state_index_from(q, self.state_color(q).unwrap().class()) == Some(q)
        } else {
            false
        }
    }

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

    /// Computes a DFA that accepts precisely those finite words which loop on the given `class`. Formally,
    /// if `u` represents the given class, then the DFA accepts precisely those words `w` such that `uw`
    /// is congruent to `u`.
    pub fn looping_words(&self, class: &Class<A::Symbol>) -> DFA<A> {
        self.map_state_colors(|c: ColoredClass<A::Symbol, Q>| c.class() == class)
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

    type ExtendStateIndexIter = std::ops::Range<usize>;

    fn extend_states<I: IntoIterator<Item = crate::ts::StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter {
        self.ts_mut().extend_states(iter)
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
