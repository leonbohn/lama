use crate::prelude::*;

impl_mealy_automaton!(DBA, bool);

/// Similar to [`IsDfa`], this trait is supposed to be (automatically) implemented by everything that can be viewed
/// as a [`crate::DBA`].
pub trait DBALike: TransitionSystem<EdgeColor = bool> + Pointed {
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
            .filter(|scc| self.is_reachable(*scc.first().unwrap()))
        {
            if let Some(full_word) = good_scc.maximal_word() {
                todo!()
                // let InfinityColors(colors) = self
                //     .induced(&full_word, self.initial())
                //     .expect("word is valid");
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

impl<Ts> DBALike for Ts where Ts: TransitionSystem<EdgeColor = bool> + Pointed {}
