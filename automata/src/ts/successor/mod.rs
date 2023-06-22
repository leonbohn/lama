use std::collections::BTreeSet;

use crate::{
    alphabet::{HasAlphabet, Symbol, SymbolOf},
    word::{Induces, RawWithLength},
    Color, Word,
};

use self::walker::RunResult;

use super::{has_states::StateColor, IndexTS, Path, StateIndex, Transition};

mod partial;
pub use partial::Partial;

mod successful;
pub use successful::Successful;

mod walker;
pub use walker::Walker;

pub trait Successor: StateColor + HasAlphabet {
    type EdgeColor: Color;

    fn successor(
        &self,
        state: StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Transition<'_, SymbolOf<Self>, Self::EdgeColor>>;

    fn successor_index(&self, state: StateIndex, symbol: SymbolOf<Self>) -> Option<StateIndex> {
        self.successor(state, symbol).map(|t| t.target())
    }

    fn run<'a, 'b, R: Word<Symbol = SymbolOf<Self>>>(
        &'a self,
        word: &'b R,
        state: StateIndex,
    ) -> RunResult<'a, 'b, Self, R>
    where
        Self: Sized,
    {
        self.walk(word, state).result()
    }

    fn induced<'a, 'b, R: Word<Symbol = SymbolOf<Self>>>(
        &'a self,
        word: &'b R,
        state: StateIndex,
    ) -> Option<<R::Length as Induces>::Induced<Self::EdgeColor>>
    where
        Self: Sized,
        Self::EdgeColor: Symbol,
    {
        self.run(word, state)
            .ok()
            .map(|result| result.colors().reached())
    }

    fn walk<'a, 'b, R: Word<Symbol = SymbolOf<Self>>>(
        &'a self,
        word: &'b R,
        state: StateIndex,
    ) -> Walker<'a, 'b, Self, R>
    where
        Self: Sized,
    {
        Walker::new(word, self, state)
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use super::Successor;
    use crate::{alphabet, ts::IndexTS, word::RawWithLength, FiniteLength, Word};

    #[test]
    #[traced_test]
    fn run() {
        let mut ts = IndexTS::new(alphabet::Simple::from_iter(['a', 'b']));
        let s0 = ts.add_state(());
        let s1 = ts.add_state(());
        let _e0 = ts.add_edge(s0, 'a', s1, 0);
        let _e1 = ts.add_edge(s0, 'b', s0, 1);
        let _e2 = ts.add_edge(s1, 'a', s1, 0);
        let _e3 = ts.add_edge(s1, 'b', s0, 1);

        let input = RawWithLength::new(vec!['a', 'b', 'b', 'a'], FiniteLength::new(4));
        let res = ts.run(&input, s0);
        assert!(matches!(res, Ok(_)));

        let Ok(result) = res else {panic!("Run should be successful!")};
        println!("{}", result.colors());
        println!("{}", result.colors().reached());
    }
}
