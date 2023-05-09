use crate::{ts::IntoParts, Acceptor, Pair, State, Successor, Symbol, DFA};

pub trait Intersection<Rhs: Acceptor = Self>: Acceptor {
    type IntersectionAcceptor: Acceptor;

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
