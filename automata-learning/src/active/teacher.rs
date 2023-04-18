use automata::{Acceptor, Class, Dfa, StateIndex, Symbol, Transducer};

pub trait Oracle {
    type Input: Symbol;
    type Output: Symbol;

    fn output(&mut self, word: &Class<Self::Input>) -> Self::Output;

    fn equivalence<M: Transducer>(&mut self, hypothesis: &M) -> Result<(), Class<Self::Input>>;
}

impl<Q: StateIndex, S: Symbol> Oracle for Dfa<Q, S> {
    type Input = S;
    type Output = bool;

    fn output(&mut self, word: &Class<S>) -> bool {
        self.accepts(word)
    }

    fn equivalence<M: Transducer>(&mut self, hypothesis: &M) -> Result<(), Class<S>> {
        todo!()
    }
}
