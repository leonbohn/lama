use std::borrow::Borrow;

use tracing::trace;

use crate::{
    acceptance::AcceptanceCondition,
    run::{Configuration, Evaluate},
    ts::{InputOf, Pointed, Successor, Visitor},
    StateIndex, Str, Symbol, Transformer, Transition, Trigger, UltimatelyPeriodicWord, Word, DBA,
    DFA,
};

/// Implemented by objects which can accept a word. We use `W` as an ipnut type parameter to allow for different implementations based on the type of word.
pub trait Acceptor: Successor {
    /// The type of object accepted.
    type Word: Word<S = Self::Sigma>;

    /// Returns true iff the given `word` is accepted, i.e. it satisfies the acceptance condition.
    fn accepts(&self, input: &Self::Word) -> bool;

    /// Returns the opposite of `accepts`.
    fn rejects(&self, input: &Self::Word) -> bool {
        !self.accepts(input)
    }
}

impl<Q: StateIndex, S: Symbol> Acceptor for DFA<Q, S> {
    type Word = Str<S>;

    fn accepts(&self, input: &Self::Word) -> bool {
        trace!("Seeing if the word {} is accepted", input);
        self.run(input).evaluate().map_or(false, |q| {
            trace!("Evaluating acceptance for state {:?}", q);
            self.acceptance().apply(&q)
        })
    }
}

impl<Q: StateIndex, S: Symbol> Acceptor for DBA<Q, S> {
    type Word = UltimatelyPeriodicWord<S>;

    fn accepts(&self, input: &Self::Word) -> bool {
        self.run(input).evaluate().map_or(false, |q| {
            q.iter().any(|q| {
                self.acceptance()
                    .apply((q.source().clone(), q.sym().clone()))
            })
        })
    }
}

/// Implemented by [`Acceptor`]s for which we can efficiently decide emptiness. Can be used to obtain
/// a witness, i.e. a word that is accepted by the automaton.
pub trait IsEmpty: Acceptor {
    /// Produces a word that witnesses the non-emptiness starting in the state `from` (meaning it is accepted starting in `from`).
    /// If no such word exists, the function returns `None`. Usually this is used through helper functions like
    /// `is_empty` and `is_nonempty`.
    fn give_word_from(&self, from: &Self::Q) -> Option<Self::Word>;

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

impl<Q: StateIndex, S: Symbol> IsEmpty for DFA<Q, S> {
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

impl<Q: StateIndex, S: Symbol> IsEmpty for DBA<Q, S> {
    fn give_word_from(&self, from: &Self::Q) -> Option<Self::Word> {
        let sccs = self.scc_transitions();

        if let Some(good_scc) = sccs.iter().find(|scc| {
            scc.iter().any(|transition| {
                self.apply((transition.source().clone(), transition.sym().clone()))
            })
        }) {
            todo!()
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::acceptance::{acceptor::IsEmpty, tests::one_mod_three_times_a_dfa};

    #[test]
    fn non_emptiness() {
        let dfa = one_mod_three_times_a_dfa();
        assert!(dfa.is_nonempty());
        let witness = dfa.give_word();
        assert_eq!(witness, Some("a".into()));
    }
}
