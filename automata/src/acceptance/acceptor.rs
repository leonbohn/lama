use std::borrow::Borrow;

use tracing::trace;

use crate::{
    acceptance::AcceptanceCondition,
    run::{Configuration, Evaluate},
    ts::{InputOf, IntoStates, Pointed, Successor, Visitor},
    State, Str, Symbol, Transformer, Transition, Trigger, UltimatelyPeriodicWord, Word, DBA, DFA,
    DPA,
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

impl<Q: State, S: Symbol> Acceptor for DFA<Q, S> {
    type Word = Str<S>;

    fn accepts(&self, input: &Self::Word) -> bool {
        trace!("Seeing if the word {} is accepted", input);
        self.run(input).evaluate().map_or(false, |q| {
            trace!("Evaluating acceptance for state {:?}", q);
            self.acceptance().apply(&q)
        })
    }
}

impl<Q: State, S: Symbol> Acceptor for DBA<Q, S> {
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

impl<Q: State, S: Symbol> Acceptor for DPA<Q, S> {
    type Word = UltimatelyPeriodicWord<S>;

    fn accepts(&self, input: &Self::Word) -> bool {
        self.run(input).evaluate().map_or(false, |q| {
            q.iter()
                .map(|transition| {
                    self.apply((transition.source().clone(), transition.sym().clone()))
                })
                .min()
                .map(|i| i % 2 == 0)
                .unwrap_or(false)
        })
    }
}
