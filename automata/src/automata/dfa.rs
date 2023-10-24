use crate::{prelude::*, ts::finite::ReachedColor};

impl_moore_automaton! {
    /// A deterministic finite automaton consists of a finite set of states, a finite set of input
    /// symbols, a transition function, a start state, and a set of accepting states.
    /// Internally, it is represented as a [`MooreMachine`], i.e. a transition system for which we consider
    /// the `bool` values that it has on the states. So a DFA accepts an input (which is the same as
    /// the Moore machine outputs `true` on the input), if the value of the state that it reaches upon
    /// reading the input is `true`.
    DFA, bool
}

impl<D: DFALike> Acceptor<SymbolOf<D>, FiniteLength> for DFA<D::Alphabet, D::EdgeColor, D> {
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = FiniteLength, Symbol = SymbolOf<D>>,
    {
        self.reached_color(&word) == Some(ReachedColor(true))
    }
}
