use std::borrow::Borrow;

use crate::{
    ts::{IntoStates, Visitor},
    Acceptor, Pointed, State, Str, Successor, Symbol, Transformer, Trigger, UltimatelyPeriodicWord,
    DBA, DFA, DPA,
};

/// Implemented by [`Acceptor`]s for which we can efficiently decide emptiness. Can be used to obtain
/// a witness, i.e. a word that is accepted by the automaton.
pub trait IsEmpty: Acceptor {
    /// Produces a word that witnesses the non-emptiness starting in the state `from` (meaning it is accepted starting in `from`).
    /// If no such word exists, the function returns `None`. Usually this is used through helper functions like
    /// `is_empty` and `is_nonempty`.
    fn give_word_from(&self, from: &Self::Q) -> Option<Self::Word>;

    /// Produces a word that witnesses the non-emptiness starting in the initial state. If no such word exists, the function returns `None`.
    fn give_word(&self) -> Option<Self::Word>
    where
        Self: Pointed,
    {
        self.give_word_from(&self.initial())
    }

    /// Returns true iff the language of the automaton is empty.
    fn is_empty(&self) -> bool
    where
        Self: Pointed,
    {
        self.give_word_from(&self.initial()).is_none()
    }

    /// Checks if the language of the automaton is empty, starting from the given state.
    fn is_empty_from<X: Borrow<Self::Q>>(&self, from: X) -> bool {
        self.give_word_from(from.borrow()).is_none()
    }

    /// Returns the opposite of `is_empty`.
    fn is_nonempty_from<X: Borrow<Self::Q>>(&self, from: X) -> bool {
        !self.is_empty_from(from)
    }

    /// Returns true iff the language of the automaton is empty, starting from the given state.
    fn is_nonempty(&self) -> bool
    where
        Self: Pointed,
    {
        !self.is_empty()
    }
}

impl<Q: State, S: Symbol> IsEmpty for DFA<Q, S> {
    fn give_word_from(&self, from: &Self::Q) -> Option<Self::Word> {
        self.paths().iter().find_map(|path| {
            if self.apply(path.reached()) {
                Some(path.label().clone())
            } else {
                None
            }
        })
    }
}

impl<Q: State, S: Symbol> IsEmpty for DBA<Q, S> {
    fn give_word_from(&self, from: &Self::Q) -> Option<Self::Word> {
        let sccs = self.scc_transitions();

        sccs.iter()
            .find(|scc| {
                scc.iter().any(|transition| {
                    self.apply((transition.source().clone(), transition.sym().clone()))
                })
            })
            .and_then(|good_scc| {
                /// Construct a witness from a word, that takes all transitions in the good SCC infinitely often
                self.scc_transitions_witness(good_scc)
                    .map(UltimatelyPeriodicWord::from)
            })
    }
}

impl<Q: State, S: Symbol> IsEmpty for DPA<Q, S> {
    fn give_word_from(&self, from: &Self::Q) -> Option<Self::Word> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        operations::IsEmpty,
        tests::{inf_aa_dba, one_mod_three_times_a_dfa},
        upw, Acceptor,
    };

    #[test]
    fn non_emptiness() {
        let dfa = one_mod_three_times_a_dfa();
        assert!(dfa.is_nonempty());
        let witness = dfa.give_word();
        assert_eq!(witness, Some("a".into()));

        let dba = inf_aa_dba();
        assert!(dba.accepts(&dba.give_word().unwrap()))
    }
}
