use crate::{ts::IntoParts, Acceptor, Pair, State, Successor, Symbol, DFA};

pub trait Union<Rhs: Acceptor = Self>: Acceptor {
    type UnionAcceptor: Acceptor;
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
