use crate::prelude::*;

use super::acceptor::OmegaWordAcceptor;

impl_mealy_automaton!(DBA, bool);

/// Similar to [`IsDfa`], this trait is supposed to be (automatically) implemented by everything that can be viewed
/// as a [`crate::DBA`].
pub trait DBALike: Deterministic<EdgeColor = bool> + Pointed {
    /// Uses a reference to `self` for creating a [`DBA`].
    fn as_dba(&self) -> IntoDBA<&Self> {
        DBA::from(self)
    }

    /// Consumes `self` and returns a [`DBA`].
    fn into_dba(self) -> IntoDBA<Self> {
        DBA::from(self)
    }

    /// Tries to identify a word which is accepted by `self`. If such a word exists, it returns it and otherwise
    /// the function gives back `None`.
    fn dba_give_word(&self) -> Option<Reduced<SymbolOf<Self>>> {
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
                        return Some(Reduced::ultimately_periodic(spoke, full_word));
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

impl<A: Alphabet> OmegaWordAcceptor<A::Symbol> for DBA<A> {
    fn accepts_omega<W: OmegaWord<A::Symbol>>(&self, word: W) -> bool {
        self.recurrent_edge_colors(word)
            .map(|mut set| set.any(|x| x))
            .unwrap_or(false)
    }
}
