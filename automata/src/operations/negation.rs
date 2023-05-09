use crate::{Acceptor, State, Symbol, DFA};

pub trait Negation: Acceptor {
    fn complement(&self) -> Self;
}

impl<Q: State, S: Symbol> Negation for DFA<Q, S> {
    fn complement(&self) -> Self {
        self.clone().map_acceptance(|x| !x)
    }
}
