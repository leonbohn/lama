use crate::{ts::IntoParts, Acceptor, Pair, State, Successor, Symbol, DFA};

/// For implementors, the union with another [`Acceptor`] of type `Rhs` can be computed.
pub trait Union<Rhs: Acceptor = Self>: Acceptor {
    /// The type of acceptor that the [`union`] operation returns.
    type UnionAcceptor: Acceptor;

    /// Computes an [`Acceptor`] which accepts precisely those words which are
    /// either accepted by `self` or by `rhs`.
    fn union(&self, rhs: &Rhs) -> Self::UnionAcceptor;
}

impl<P: State, Q: State, S: Symbol> Union<DFA<P, S>> for DFA<Q, S> {
    type UnionAcceptor = DFA<Pair<Q, P>, S>;

    fn union(&self, rhs: &DFA<P, S>) -> Self::UnionAcceptor {
        self.product(rhs)
            .into_moore()
            .map_acceptance(|Pair { left, right }| *left || *right)
    }
}
