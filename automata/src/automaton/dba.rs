use crate::prelude::*;

use super::{acceptor::OmegaSemantics, Automaton};

#[derive(Debug, Default, Clone, Eq, PartialEq, Hash, Copy)]
pub struct DBASemantics;

impl<Q> OmegaSemantics<Q, bool> for DBASemantics {
    type Output = bool;
    fn omega_semantic<R>(&self, run: R) -> Self::Output
    where
        R: OmegaRun<StateColor = Q, EdgeColor = bool>,
    {
        run.infinity_edge_colors()
            .map(|mut colors| colors.any(|b| b))
            .unwrap_or(false)
    }
}

/// A deterministic Büchi automaton (DBA) is a deterministic automaton with Büchi acceptance condition. It accepts a word if it visits an accepting state infinitely often.
/// It is a special case of a deterministic parity automaton [`crate::DPA`] with
/// min even and priorities 0 and 1.
pub type DBA<A = CharAlphabet> = Automaton<Initialized<DTS<A, Void, bool>>, DBASemantics, true>;
/// Helper trait for creating a [`DBA`] from a given transition system.
pub type IntoDBA<T> = Automaton<T, DBASemantics, true>;

/// Similar to [`DFALike`], this trait is supposed to be (automatically) implemented by everything that can be viewed
/// as a [`crate::DBA`].
pub trait DBALike: Congruence<EdgeColor = bool> {
    /// Uses a reference to `self` for creating a [`DBA`].
    fn borrow_dba(&self) -> IntoDBA<&Self> {
        self.into_dba()
    }

    /// Consumes `self` and returns a [`DBA`].
    fn into_dba(self) -> IntoDBA<Self> {
        Automaton::from_parts(self, DBASemantics)
    }

    /// Tries to identify a word which is accepted by `self`. If such a word exists, it returns it and otherwise
    /// the function gives back `None`.
    fn dba_give_word(&self) -> Option<ReducedOmegaWord<SymbolOf<Self>>> {
        for good_scc in self
            .sccs()
            .iter()
            .filter(|scc| !scc.is_empty() && self.is_reachable(*scc.first().unwrap()))
        {
            if let Some(full_word) = good_scc.maximal_word() {
                // let InfinityColors(colors) = self
                //     .induced(&full_word, self.initial())
                //     .expect("word is valid");
                if let Some(infset) =
                    self.visited_edge_colors_from(&full_word, *good_scc.first().unwrap())
                {
                    if infset.iter().any(|b| *b) {
                        let spoke = self
                            .word_from_to(self.initial(), *good_scc.first().unwrap())
                            .expect("We know this is reachable!");
                        return Some(ReducedOmegaWord::ultimately_periodic(spoke, full_word));
                    }
                }
                // if colors.contains(&true) {
                //     let base = self
                //         .word_from_to(self.initial(), *good_scc.first().unwrap())
                //         .expect("we know this is reachable");
                //     return Some(OmegaWord::from_parts(base, full_word));
            }
        }
        None
    }

    /// Returns `true` if and only if `self` accepts a non-empty language.
    fn dba_is_empty(&self) -> bool {
        self.dba_give_word().is_none()
    }
}

impl<Ts> DBALike for Ts where Ts: Deterministic<EdgeColor = bool> + Pointed {}
