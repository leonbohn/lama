use std::collections::BTreeSet;

use crate::{
    alphabet::{HasAlphabet, HasUniverse, Symbol, SymbolOf},
    automaton::WithInitial,
    word::RawWithLength,
    Color, Pointed, Word,
};

use self::{
    reachable::{ReachableStateIndices, ReachableStates},
    sccs::{tarjan_scc, Scc},
    walker::RunResult,
};

use super::{
    operations::{MapColors, MatchingProduct},
    CanInduce, ColorPosition, EdgeColor, HasStateIndices, IndexTS, IndexType, Induced, Path,
    StateColor, StateIndex, Transition,
};

mod partial;
pub use partial::Partial;

mod successful;
pub use successful::Successful;

mod walker;
pub use walker::Walker;

mod reachable;
pub use reachable::MinimalRepresentatives;

mod restricted;
pub use restricted::RestrictByStateIndex;

mod sccs;
pub use sccs::Tarjan;

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
pub trait Successor: HasAlphabet {
    type StateIndex: IndexType;
    type Position: ColorPosition;
    type Color: Color;

    /// For a given `state` and `symbol`, returns the transition that is taken, if it exists.
    fn successor(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Transition<Self::StateIndex, SymbolOf<Self>, EdgeColor<Self>>>;

    fn state_color(&self, state: Self::StateIndex) -> StateColor<Self>;

    fn with_initial(&self, initial: Self::StateIndex) -> WithInitial<&Self>
    where
        Self: Sized,
    {
        (self, initial).into()
    }

    fn restrict_state_indices<F: Fn(Self::StateIndex) -> bool>(
        self,
        filter: F,
    ) -> RestrictByStateIndex<Self, F>
    where
        Self: Sized,
    {
        RestrictByStateIndex::new(self, filter)
    }

    fn map_colors<D: Color, F: Fn(Self::Color) -> D>(self, f: F) -> MapColors<Self, F>
    where
        Self: Sized,
    {
        MapColors::new(self, f)
    }

    fn all_accepting_dfa(self) -> MapColors<Self, fn(Self::Color) -> bool>
    where
        Self: Sized,
    {
        self.map_colors(|_| true)
    }

    fn sccs(&self) -> Vec<Scc<Self>>
    where
        Self: Sized + HasStateIndices,
        Self::Alphabet: HasUniverse,
    {
        tarjan_scc(self)
    }

    /// Returns just the [Self::Index] of the successor that is reached on the given `symbol`
    /// from `state`. If no suitable transition exists, `None` is returned.
    fn successor_index(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::StateIndex> {
        self.successor(state, symbol).map(|t| t.target())
    }

    /// Starts a new [`Walker`] that can be used to successively take transitions from `state` on
    /// the letters of `word`.
    fn walk<'a, 'b, R: Word<Symbol = SymbolOf<Self>>>(
        &'a self,
        word: &'b R,
        state: Self::StateIndex,
    ) -> Walker<'a, 'b, Self, R>
    where
        Self: Sized,
    {
        Walker::new(word, self, state)
    }

    fn word_from_to(
        &self,
        from: Self::StateIndex,
        to: Self::StateIndex,
    ) -> Option<Vec<SymbolOf<Self>>>
    where
        Self: Sized,
        Self::Alphabet: HasUniverse,
    {
        self.minimal_representatives_from(from)
            .find_map(|(word, state)| if state == to { Some(word) } else { None })
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
        state: Self::StateIndex,
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
    fn induced<'a, 'b, R, I>(&'a self, word: &'b R, state: Self::StateIndex) -> Option<I>
    where
        I: Induced,
        Successful<'a, 'b, R, Self>: CanInduce<I>,
        Self: Sized,
        R: Word<Symbol = SymbolOf<Self>>,
    {
        self.run(word, state).ok().map(|r| r.induce())
    }

    fn make_edge_color(&self, color: Self::Color) -> EdgeColor<Self> {
        <Self::Position as ColorPosition>::edge_color(color)
    }

    fn make_state_color(&self, color: Self::Color) -> StateColor<Self> {
        <Self::Position as ColorPosition>::state_color(color)
    }

    fn minimal_representatives(&self) -> MinimalRepresentatives<&Self>
    where
        Self: Sized + Pointed,
        Self::Alphabet: HasUniverse,
    {
        MinimalRepresentatives::new(self, self.initial())
    }

    fn reachable_state_indices(&self) -> ReachableStateIndices<&Self>
    where
        Self: Sized + Pointed,
        Self::Alphabet: HasUniverse,
    {
        ReachableStateIndices::new(self, self.initial())
    }

    fn reachable_states(&self) -> ReachableStates<&Self>
    where
        Self: Sized + Pointed,
        Self::Alphabet: HasUniverse,
    {
        ReachableStates::new(self, self.initial())
    }

    fn minimal_representatives_from<I: Into<Self::StateIndex>>(
        &self,
        state: I,
    ) -> MinimalRepresentatives<&Self>
    where
        Self: Sized,
        Self::Alphabet: HasUniverse,
    {
        MinimalRepresentatives::new(self, state.into())
    }

    fn reachable_state_indices_from<I: Into<Self::StateIndex>>(
        &self,
        state: I,
    ) -> ReachableStateIndices<&Self>
    where
        Self: Sized + Pointed,
        Self::Alphabet: HasUniverse,
    {
        ReachableStateIndices::new(self, self.initial())
    }

    fn reachable_states_from<I: Into<Self::StateIndex>>(&self, state: I) -> ReachableStates<&Self>
    where
        Self: Sized + Pointed,
        Self::Alphabet: HasUniverse,
    {
        ReachableStates::new(self, self.initial())
    }
}

impl<Ts: Successor> Successor for &Ts {
    type StateIndex = Ts::StateIndex;

    type Position = Ts::Position;

    type Color = Ts::Color;

    fn successor(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Transition<Self::StateIndex, SymbolOf<Self>, EdgeColor<Self>>> {
        Ts::successor(self, state, symbol)
    }

    fn state_color(&self, state: Self::StateIndex) -> StateColor<Self> {
        Ts::state_color(self, state)
    }
}
impl<Ts: Successor> Successor for &mut Ts {
    type StateIndex = Ts::StateIndex;

    type Position = Ts::Position;

    type Color = Ts::Color;

    fn successor(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Transition<Self::StateIndex, SymbolOf<Self>, EdgeColor<Self>>> {
        Ts::successor(self, state, symbol)
    }

    fn state_color(&self, state: Self::StateIndex) -> StateColor<Self> {
        Ts::state_color(self, state)
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
            index_ts::MealyTS,
            IndexTS, Sproutable,
        },
        word::RawWithLength,
        FiniteLength, Word,
    };

    #[test]
    #[traced_test]
    fn run() {
        let mut ts = MealyTS::new(alphabet::Simple::from_iter(['a', 'b']));
        let s0 = ts.add_state(());
        let s1 = ts.add_state(());
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
    }
}
