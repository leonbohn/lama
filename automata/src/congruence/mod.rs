use std::fmt::{Debug, Display};

use itertools::{Itertools, MapInto};

use crate::{
    alphabet::{CharAlphabet, Symbol},
    automaton::IntoDFA,
    prelude::{DFALike, IsEdge},
    ts::{transition_system::Indexes, Deterministic, EdgeColor, Sproutable, StateColor, DTS},
    word::FiniteWord,
    Alphabet, Color, FiniteLength, HasLength, Map, Pointed, Show, TransitionSystem, Void, DFA,
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
pub struct RightCongruence<A: Alphabet = CharAlphabet, Q = Void, C = Void> {
    ts: DTS<A, ColoredClass<A::Symbol, Q>, C>,
}

impl<S: Symbol + Show, Q: Show> Show for ColoredClass<S, Q> {
    fn show(&self) -> String {
        format!("{} | {}", self.class.show(), self.color.show())
    }
}

impl<S: Symbol + Show> Show for ColoredClass<S, Void> {
    fn show(&self) -> String {
        self.class.show()
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

impl<A: Alphabet, Q: Clone + Debug, C: Clone + Debug> Debug for RightCongruence<A, Q, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.ts.build_transition_table(
                |q, c| format!("{}|{:?}", q.show(), c),
                |edge| edge.target().show()
            )
        )
    }
}

impl<A: Alphabet, Q: Clone, C: Clone> RightCongruence<A, Q, C> {
    /// Assumes that `self` is det. and complete.
    pub fn congruent<W, V>(&self, word: W, other: V) -> bool
    where
        W: FiniteWord<A::Symbol>,
        V: FiniteWord<A::Symbol>,
    {
        self.reached_state_index(word).unwrap() == self.reached_state_index(other).unwrap()
    }

    /// Turns the given transition system into a right congruence.
    pub fn from_ts<Ts: Pointed + Deterministic<Alphabet = A, StateColor = Q, EdgeColor = C>>(
        ts: Ts,
    ) -> Self {
        let mut cong = Self {
            ts: ts
                .map_state_colors(|c| ColoredClass::new(Class::default(), c))
                .collect_dts(),
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
    pub fn classes(&self) -> impl Iterator<Item = (Class<A::Symbol>, usize)> + '_ {
        self.ts
            .state_indices_with_color()
            .map(|(id, c)| (c.class().clone(), id))
    }

    /// Returns a reference to the underlying [`TransitionSystem`].
    pub fn ts(&self) -> &DTS<A, ColoredClass<A::Symbol, Q>, C> {
        &self.ts
    }

    /// Returns a mutable reference to the underlying [`TransitionSystem`].
    pub fn ts_mut(&mut self) -> &mut DTS<A, ColoredClass<A::Symbol, Q>, C> {
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
        self.ts.state_indices_with_color().find_map(|(id, c)| {
            if c.class() == class {
                Some(id)
            } else {
                None
            }
        })
    }

    /// Returns the [`ColoredClass`] that is referenced by `index`.
    pub fn class_name<Idx: Indexes<Self>>(&self, index: Idx) -> Option<ColoredClass<A::Symbol, Q>> {
        self.ts().state_color(index.to_index(self)?)
    }

    /// Computes a DFA that accepts precisely those finite words which loop on the given `class`. Formally,
    /// if `u` represents the given class, then the DFA accepts precisely those words `w` such that `uw`
    /// is congruent to `u`.
    pub fn looping_words(&self, class: &Class<A::Symbol>) -> DFA<A> {
        self.map_state_colors(|c: ColoredClass<A::Symbol, Q>| c.class() == class)
            .erase_edge_colors()
            .collect_dts()
            .with_initial(self.class_to_index(class).unwrap())
            .into_dfa()
    }
}

impl<A: Alphabet, Q: Clone, C: Clone> Pointed for RightCongruence<A, Q, C> {
    fn initial(&self) -> Self::StateIndex {
        assert!(!self.is_empty());
        0
    }
}

impl<A: Alphabet, Q: Clone, C: Clone> Sproutable for RightCongruence<A, Q, C> {
    fn add_state<X: Into<crate::ts::StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
        self.ts.add_state(color.into())
    }

    fn set_state_color<Idx: Indexes<Self>, X: Into<StateColor<Self>>>(
        &mut self,
        index: Idx,
        color: X,
    ) {
        let Some(index) = index.to_index(self) else {
            tracing::error!("cannot set color of state that does not exist");
            return;
        };
        self.ts.set_state_color(index, color.into())
    }

    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        let mut ts = DTS::new_for_alphabet(alphabet);
        Self { ts }
    }
    fn add_edge<X, Y, CI>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: CI,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Indexes<Self>,
        Y: Indexes<Self>,
        CI: Into<EdgeColor<Self>>,
    {
        let from = from.to_index(self)?;
        let to = to.to_index(self)?;
        self.ts_mut().add_edge(from, on, to, color.into())
    }
    fn remove_edges<X>(&mut self, from: X, on: <Self::Alphabet as Alphabet>::Expression) -> bool
    where
        X: Indexes<Self>,
    {
        from.to_index(self)
            .map(|idx| self.ts_mut().remove_edges(idx, on))
            .unwrap_or(false)
    }

    type ExtendStateIndexIter = std::ops::Range<usize>;

    fn extend_states<I: IntoIterator<Item = crate::ts::StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter {
        self.ts_mut().extend_states(iter)
    }
}

impl<A: Alphabet, Q: Clone + Default, C: Clone> RightCongruence<A, Q, C> {
    /// Creates a new [`RightCongruence`] for the given alphabet.
    pub fn new(alphabet: A) -> RightCongruence<A, Q, C> {
        Self::new_for_alphabet(alphabet)
    }
}
