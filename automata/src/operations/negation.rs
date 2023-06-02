use crate::{Acceptor, State, Symbol, DFA};

/// Implementors of this trait can be complemented efficiently.
pub trait Negation: Acceptor {
    /// The type of [`Acceptor`] constructed by the [`complement`] operation.
    type NegationAcceptor: Acceptor;

    /// Complements `self`, yielding an [`Acceptor`] that accepts precisely
    /// those words that `self` rejects.
    fn complement(&self) -> Self::NegationAcceptor;
}

impl<Q: State, S: Symbol> Negation for DFA<Q, S> {
    type NegationAcceptor = Self;

    fn complement(&self) -> Self {
        self.clone().map_acceptance(|x| !x)
    }
}
