use automata::Transducer;

pub trait Oracle {
    type Word;
    type Output;

    fn output(&mut self, word: &Self::Word) -> Self::Output;

    fn equivalence<M: Transducer>(&mut self, hypothesis: &M) -> Result<(), Self::Word>;
}
