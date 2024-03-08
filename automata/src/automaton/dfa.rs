use std::collections::BTreeSet;

use crate::{
    algorithms::moore_partition_refinement,
    prelude::*,
    ts::{
        finite::ReachedColor,
        operations::{MapStateColor, MatchingProduct},
        Quotient,
    },
};

use super::{acceptor::FiniteSemantics, Automaton, StatesWithColor};

#[derive(Clone, Copy, Default, Hash, Eq, PartialEq)]
pub struct DFASemantics;

impl std::fmt::Debug for DFASemantics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DFA (reach true)")
    }
}

impl<C> FiniteSemantics<bool, C> for DFASemantics {
    type Output = bool;
    fn finite_semantic<R>(&self, run: R) -> Self::Output
    where
        R: FiniteRun<StateColor = bool, EdgeColor = C>,
    {
        run.state_colors()
            .and_then(|colors| colors.last())
            .unwrap_or(false)
    }
}

/// A deterministic finite automaton (DFA) is a deterministic automaton with a simple acceptance condition. It accepts a finite word if it reaches an accepting state.
pub type DFA<A = CharAlphabet> = Automaton<Initialized<DTS<A, bool, Void>>, DFASemantics, false>;
/// Helper trait for creating a [`DFA`] from a given transition system.
pub type IntoDFA<T> = Automaton<T, DFASemantics, false>;

impl<T> DFALike for T where T: Congruence<StateColor = bool> {}
/// Implemented by a [`Congruence`] if it can be viewed as a [`DFA`], meaning
/// it has a boolean state color.
pub trait DFALike: Congruence<StateColor = bool> {
    /// Consumes self and returns a [`DFA`].
    fn into_dfa(self) -> IntoDFA<Self> {
        Automaton::from_parts(self, DFASemantics)
    }
    /// Uses a reference to `self` for creating a [`DFA`].
    fn borrow_dfa(&self) -> IntoDFA<&Self> {
        (self).into_dfa()
    }
    /// Collects the transition structure of `self` and returns a [`DFA`].  
    fn collect_dfa(&self) -> DFA<Self::Alphabet> {
        DFA::from_parts(self.erase_edge_colors().collect().0, DFASemantics)
    }
    /// Collects the reachable part of the transition structure of `self` and returns a [`DFA`].
    fn collect_trim_dfa(&self) -> DFA<Self::Alphabet> {
        self.erase_edge_colors().trim_collect().into_dfa()
    }

    /// Returns the indices of all states that are accepting.
    fn accepting_states(&self) -> StatesWithColor<'_, Self> {
        StatesWithColor::new(self, true)
    }

    /// Returns the indices of all states that are rejecting.
    fn rejecting_states(&self) -> StatesWithColor<'_, Self> {
        StatesWithColor::new(self, false)
    }

    /// Minimizes `self` using Hopcroft's partition refinement algorithm.
    fn minimized(self) -> IntoDFA<impl DFALike<Alphabet = Self::Alphabet>> {
        let min = moore_partition_refinement(self);
        min.into_dfa()
    }

    /// Checks whether `self` is equivalent to `other`, i.e. whether the two DFAs accept
    /// the same language. This is done by negating `self` and then verifying that the intersection
    /// of the negated automaton with `other` is empty.
    fn equivalent<E: DFALike<Alphabet = Self::Alphabet>>(&self, other: E) -> bool {
        self.negation().intersection(other).is_empty_language()
    }

    /// Tries to construct a (finite) word witnessing that the accepted language is empty. If such a word exists,
    /// the function returns it, otherwise `None`.
    fn give_word(&self) -> Option<Vec<SymbolOf<Self>>> {
        self.minimal_representatives().find_map(|(mr, index)| {
            if self
                .state_color(index)
                .expect("Every state must be colored")
            {
                Some(mr)
            } else {
                None
            }
        })
    }

    /// Returns true if and only if the accepted language is empty.
    fn is_empty_language(&self) -> bool {
        self.give_word().is_none()
    }

    /// Computes the union of `self` with the given `other` object (that can be viewed as a DFA) through
    /// a simple product construction.
    fn union<E: DFALike<Alphabet = Self::Alphabet>>(
        self,
        other: E,
    ) -> IntoDFA<impl DFALike<Alphabet = Self::Alphabet>> {
        self.ts_product(other)
            .map_state_colors(|(a, b)| a || b)
            .into_dfa()
    }

    /// Computes the intersection of `self` with the given `other` object (that can be viewed as a DFA) through
    /// a simple product construction.
    fn intersection<E: DFALike<Alphabet = Self::Alphabet>>(
        self,
        other: E,
    ) -> IntoDFA<impl DFALike<Alphabet = Self::Alphabet>> {
        self.ts_product(other)
            .map_state_colors(|(a, b)| a && b)
            .into_dfa()
    }

    /// Computes the negation of `self` by swapping accepting and non-accepting states.
    fn negation(self) -> IntoDFA<impl DFALike<Alphabet = Self::Alphabet>> {
        self.map_state_colors(|x| !x).into_dfa()
    }

    /// Attempts to separate the state `left` from the state `right` by finding a word that leads to different colors.
    /// For a [`DFA`], this means that the returned word is in the symmetric difference of
    /// the languages accepted by the two states.
    fn separate<X, Y>(&self, left: X, right: Y) -> Option<Vec<SymbolOf<Self>>>
    where
        X: Indexes<Self>,
        Y: Indexes<Self>,
    {
        let q = left.to_index(self)?;
        let p = right.to_index(self)?;
        if p == q {
            return None;
        }

        self.with_initial(q)
            .ts_product(self.with_initial(p))
            .minimal_representatives()
            .find_map(|(rep, ProductIndex(l, r))| {
                if self.state_color(l).unwrap() != self.state_color(r).unwrap() {
                    Some(rep)
                } else {
                    None
                }
            })
    }
}

/// Helper trait to convert from boolean to usize. Normally, a `true` value corresponds to `1`, while
/// a `false` value corresponds to `0`. This does not really work well with min-even parity conditions
/// so this helper trait is introduced.
// TODO: remove this if possible.
pub trait ReducesTo<T = bool> {
    /// Reduce `self` to a value of type `T`.
    fn reduce(self) -> T;
}

impl ReducesTo<bool> for bool {
    fn reduce(self) -> bool {
        self
    }
}

impl ReducesTo<bool> for usize {
    fn reduce(self) -> bool {
        (self % 2) == 0
    }
}

impl ReducesTo<bool> for BTreeSet<bool> {
    fn reduce(self) -> bool {
        self.into_iter().any(|x| x)
    }
}

impl ReducesTo<bool> for BTreeSet<usize> {
    fn reduce(self) -> bool {
        self.into_iter().min().unwrap() % 2 == 0
    }
}

type DfaProductReduced<L, R> = MapStateColor<MatchingProduct<L, R>, fn((bool, bool)) -> bool>;
