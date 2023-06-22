use std::collections::BTreeSet;

use crate::{
    alphabet::{HasAlphabet, Symbol, SymbolOf},
    word::RawWithLength,
    Color, Word,
};

use self::walker::RunResult;

use super::{CanInduce, IndexTS, Path, StateColored, StateIndex, Transition};

mod partial;
pub use partial::Partial;

mod successful;
pub use successful::Successful;

mod walker;
pub use walker::Walker;

/// Encapsulates the transition function δ of a (finite) transition system. This is the main trait that
/// is used to query a transition system. Transitions are labeled with a [`Alphabet::Expression`], which
/// determines on which [`Alphabet::Symbol`]s the transition can be taken. Additionally, every transition
/// is labeled with a [`Color`], which can be used to store additional information about it, like an
/// associated priority.
///
/// # The difference between [`Transition`]s and [`crate::ts::Edge`]s
/// Internally, a transition system is represented as a graph, where the states are the nodes and the
/// transitions are the edges. However, the [`Transition`]s are not the same as the [`crate::ts::Edge`]s.
/// Both store the source and target vertex as well as the color, however an [`crate::ts::Edge`] is labelled
/// with an expression, while a [`Transition`] is labelled with an actual symbol (that [`Alphabet::matches`]
/// the expression). So a transition is a concrete edge that is taken (usually by the run on a word), while
/// an edge may represent any different number of transitions.
pub trait Successor: StateColored + HasAlphabet {
    /// The type of the color associated with the edges.
    type EdgeColor: Color;

    /// For a given `state` and `symbol`, returns the transition that is taken, if it exists.
    fn successor(
        &self,
        state: StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Transition<'_, SymbolOf<Self>, Self::EdgeColor>>;

    /// Returns just the [`StateIndex`] of the successor that is reached on the given `symbol`
    /// from `state`. If no suitable transition exists, `None` is returned.
    fn successor_index(&self, state: StateIndex, symbol: SymbolOf<Self>) -> Option<StateIndex> {
        self.successor(state, symbol).map(|t| t.target())
    }

    /// Starts a new [`Walker`] that can be used to successively take transitions from `state` on
    /// the letters of `word`.
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

    /// Runs the given `word` on the transition system, starting from `state`, which means starting
    /// a new [`Walker`] and immediately taking all transitions on the letters of `word`. If the
    /// run is successful (i.e. for all symbols of `word` a suitable transition can be taken), this
    /// returns a [`Successful`] run, which can then be used to obtain the colors of the transitions
    /// or the sequence of states that are visited. If the run is unsuccessful, meaning a symbol is
    /// encountered for which no transition exists, this returns a [`Partial`] run, which can be used
    /// to obtain the colors of the transitions that were taken before, as well as the state that
    /// the transition system was left from and the remaining suffix.
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

    /// Runs the given `word` on the transition system, starting from `state` by calling [`Self::run`].
    /// If the run is successful (i.e. for all symbols of `word` a suitable transition can be taken),
    /// this returns whatever is *induced* by the run. For a [`Word`] of finite length, this is
    /// simply
    fn induced<'a, 'b, R: Word<Symbol = SymbolOf<Self>>, I>(
        &'a self,
        word: &'b R,
        state: StateIndex,
    ) -> Option<I>
    where
        Successful<'a, 'b, R, Self>: CanInduce<I>,
        Self: Sized,
    {
        self.run(word, state).ok().map(|r| r.induce())
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use super::Successor;
    use crate::{
        alphabet,
        ts::{
            finite::{self, ReachedColor, ReachedState},
            IndexTS,
        },
        word::RawWithLength,
        FiniteLength, Word,
    };

    #[test]
    #[traced_test]
    fn run() {
        let mut ts = IndexTS::new(alphabet::Simple::from_iter(['a', 'b']));
        let s0 = ts.add_state(1337);
        let s1 = ts.add_state(42);
        let _e0 = ts.add_edge(s0, 'a', s1, 0);
        let _e1 = ts.add_edge(s0, 'b', s0, 1);
        let _e2 = ts.add_edge(s1, 'a', s1, 0);
        let _e3 = ts.add_edge(s1, 'b', s0, 1);

        let input = RawWithLength::new(vec!['a', 'b', 'b', 'a'], FiniteLength::new(4));
        let res = ts.run(&input, s0);
        assert!(matches!(res, Ok(_)));

        let ReachedState(q) = ts.induced(&"ab", s0).unwrap();
        assert_eq!(q, s0);
        let ReachedColor(c) = ts.induced(&input, s0).unwrap();
        assert_eq!(c, 42);

        let finite::StateColorSequence(seq) = ts.induced(&"abba", s0).unwrap();
        assert_eq!(seq, vec![1337, 42, 1337, 1337, 42]);
    }
}
