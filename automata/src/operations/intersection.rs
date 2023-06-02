use crate::{ts::IntoParts, Acceptor, Pair, State, Successor, Symbol, DFA};

/// Implementors of this trait are [`Acceptor`]s which can be intersected with
/// [`Acceptor`]s of type `Rhs`. The result of the operation is an [`Acceptor`]
/// which should accept precisely those inputs/words which are accepted by both
/// `self` and `rhs`.
pub trait Intersection<Rhs: Acceptor = Self>: Acceptor {
    /// Type type of [`Acceptor`] constructed by the [`intersection`] operation.
    type IntersectionAcceptor: Acceptor;

    /// Intersect `self` with `rhs` giving a [`Self::IntersectionAcceptor`] that
    /// accepts if and only if both `self` and `rhs` accept.
    fn intersection(&self, rhs: &Rhs) -> Self::IntersectionAcceptor;
}

impl<P: State, Q: State, S: Symbol> Intersection<DFA<P, S>> for DFA<Q, S> {
    type IntersectionAcceptor = DFA<Pair<Q, P>, S>;

    fn intersection(&self, rhs: &DFA<P, S>) -> Self::IntersectionAcceptor {
        self.product(rhs)
            .into_moore()
            .map_acceptance(|Pair { left, right }| *left && *right)
    }
}
