use automata::{
    alphabet::{Alphabet, HasAlphabet, Symbol, SymbolOf},
    ts::{ColorPosition, OnStates, Pointed, Successor},
    Acceptor, Color, FiniteLength, Length, Map, MooreMachine, Transformer, Word,
};

/// A trait that encapsulates a minimally adequate teacher (MAT) for active learning. This is mainly used by
/// L*-esque algorithms and can be implemented by wildly different types, for example an automaton, a function
/// or even a collection of words.
///
/// This trait is designed in a generic way, allowing us to use it for learning a priority mapping, which assigns
/// non-empty finite words a value of type `Output`. This means we can learn a Mealy machine by using [`Priority`] as
/// the `Output` type, but it also enables us to learn a regular language/deterministic finite automaton by using
/// `bool` as the `Output` type.
pub trait Oracle<Pos: ColorPosition = OnStates>: HasAlphabet {
    type Length: Length;
    /// The output type, for a DFA that would be a boolean, but a Mealy Machine might output a [`Priority`] instead.
    type Output: Color;

    /// Query the desired output for the given word.
    fn output<W: Word<Symbol = SymbolOf<Self>, Length = Self::Length>>(
        &self,
        word: W,
    ) -> Self::Output;

    /// Test the given hypothesis for equivalence, returning `Ok(())` if it is equivalent and `Err((word, color))` otherwise.
    /// In the latter case, `word` is a counterexample from the symmetric difference of the target and the hypothesis,
    /// meaning it produces a different output in the hypothesis compared to the target.
    fn equivalence<H>(&self, hypothesis: H) -> Result<(), (Vec<SymbolOf<Self>>, Self::Output)>
    where
        H: Pointed
            + Successor<Alphabet = Self::Alphabet, Position = Pos, Color = Self::Output>
            + Transformer<SymbolOf<Self>, Self::Length, Output = Self::Output>;
}
