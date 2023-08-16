use std::{
    collections::BTreeSet,
    fmt::{Debug, Display},
    marker::PhantomData,
};

use ahash::HashSet;
use impl_tools::autoimpl;
use itertools::Itertools;
use owo_colors::OwoColorize;
use tracing::trace;

use crate::{
    alphabet::{Alphabet, HasAlphabet, Symbol, SymbolOf},
    ts::{
        finite::{ReachedColor, ReachedState, SeenColors, TransitionColorSequence},
        infinite::InfinitySet,
        operations::{MapColors, MatchingProduct},
        ColorPosition, Congruence, EdgeColor, FiniteState, HasMutableStates, HasStates, IndexTS,
        IndexType, OnEdges, OnStates, Path, Pointed, Product, Sproutable, State, StateColor,
        StateIndex, Successor, Transition, TransitionSystem,
    },
    word::OmegaWord,
    Color, FiniteLength, HasLength, InfiniteLength, Length, Set, Word,
};

#[derive(Clone, PartialEq)]
pub struct WithInitial<Ts: Successor>(Ts, Ts::StateIndex);

impl<Ts: TransitionSystem> std::fmt::Debug for WithInitial<Ts> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Initial state {} with table\n{}",
            self.initial(),
            self.build_transition_table(|index, color| {
                if index == self.initial() {
                    format!("{} : {:?}", index.to_string().bold(), color)
                } else {
                    format!("{} : {:?}", index, color)
                }
            })
        )
    }
}

mod boilerplate_impls {
    use crate::ts::{ColorPosition, FiniteState};

    use super::*;

    impl<Ts: Successor> From<(Ts, Ts::StateIndex)> for WithInitial<Ts> {
        fn from(value: (Ts, Ts::StateIndex)) -> Self {
            Self(value.0, value.1)
        }
    }

    impl<Ts: FiniteState> FiniteState for WithInitial<Ts> {
        fn state_indices(&self) -> Vec<Self::StateIndex> {
            self.0.state_indices()
        }
    }

    impl<A, C, Pos> WithInitial<IndexTS<A, C, Pos, usize>>
    where
        A: Alphabet,
        C: Color,
        Pos: ColorPosition,
    {
        pub fn new(alphabet: A) -> Self
        where
            StateColor<Self>: Default,
        {
            let mut ts = IndexTS::new(alphabet);
            let initial = ts.add_state(<StateColor<Self> as Default>::default());
            Self(ts, initial)
        }

        pub fn with_capacity(alphabet: A, size: usize) -> Self
        where
            StateColor<Self>: Default,
        {
            let mut ts = IndexTS::with_capacity(alphabet, size);
            let initial = ts.add_state(<StateColor<Self> as Default>::default());
            Self(ts, initial)
        }
    }

    impl<Ts: Successor> WithInitial<Ts> {
        pub fn ts(&self) -> &Ts {
            &self.0
        }
        pub fn ts_mut(&mut self) -> &mut Ts {
            &mut self.0
        }
    }
    impl<Ts: Successor + Sproutable> Sproutable for WithInitial<Ts>
    where
        StateColor<Ts>: Default,
    {
        fn add_state(&mut self, color: StateColor<Self>) -> Self::StateIndex {
            self.ts_mut().add_state(color)
        }

        fn add_edge<X, Y>(
            &mut self,
            from: X,
            on: <Self::Alphabet as Alphabet>::Expression,
            to: Y,
            color: EdgeColor<Self>,
        ) -> crate::ts::EdgeIndex
        where
            X: Into<Self::StateIndex>,
            Y: Into<Self::StateIndex>,
        {
            self.ts_mut().add_edge(from, on, to, color)
        }

        fn undo_add_edge(&mut self) {
            self.ts_mut().undo_add_edge()
        }

        fn set_state_color(&mut self, index: Self::StateIndex, color: StateColor<Self>) {
            self.ts_mut().set_state_color(index, color)
        }

        fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
            let mut ts = Ts::new_for_alphabet(alphabet);
            let initial = ts.add_state(<StateColor<Ts> as Default>::default());
            Self(ts, initial)
        }
    }
    impl<Ts: Successor + HasStates> HasStates for WithInitial<Ts> {
        type State<'this> = Ts::State<'this>
        where
            Self: 'this;

        type StatesIter<'this> = Ts::StatesIter<'this>
        where
            Self: 'this;

        fn state(&self, index: Self::StateIndex) -> Option<Self::State<'_>> {
            self.ts().state(index)
        }

        fn states_iter(&self) -> Self::StatesIter<'_> {
            self.ts().states_iter()
        }
    }
    impl<Ts: Successor + HasMutableStates> HasMutableStates for WithInitial<Ts> {
        type StateMut<'this>  = Ts::StateMut<'this> where Self:'this;

        fn state_mut(&mut self, index: Self::StateIndex) -> Option<Self::StateMut<'_>> {
            self.ts_mut().state_mut(index)
        }
    }
    impl<Ts: Successor + HasAlphabet> HasAlphabet for WithInitial<Ts> {
        type Alphabet = Ts::Alphabet;

        fn alphabet(&self) -> &Self::Alphabet {
            self.0.alphabet()
        }
    }

    impl<Ts: Successor> Pointed for WithInitial<Ts> {
        fn initial(&self) -> Self::StateIndex {
            self.1
        }
    }
    impl<Ts: Successor> Successor for WithInitial<Ts> {
        type StateIndex = Ts::StateIndex;
        type Color = Ts::Color;
        type Position = Ts::Position;

        fn successor(
            &self,
            state: Self::StateIndex,
            symbol: SymbolOf<Self>,
        ) -> Option<Transition<Self::StateIndex, SymbolOf<Self>, EdgeColor<Self>>> {
            self.ts().successor(state, symbol)
        }

        fn state_color(&self, state: Self::StateIndex) -> StateColor<Self> {
            self.ts().state_color(state)
        }
    }
}

pub type MooreMachine<A, C, Idx = usize> = WithInitial<IndexTS<A, C, OnStates, Idx>>;
pub type MealyMachine<A, C, Idx = usize> = WithInitial<IndexTS<A, C, OnEdges, Idx>>;

pub type DFA<A, Idx = usize> = MooreMachine<A, bool, Idx>;
pub type DBA<A, Idx = usize> = MealyMachine<A, bool, Idx>;
pub type DPA<A, Idx = usize> = MealyMachine<A, usize, Idx>;
pub type SBDBA<A, Idx = usize> = MooreMachine<A, bool, Idx>;
pub type SBDPA<A, Idx = usize> = MooreMachine<A, usize, Idx>;

#[autoimpl(for<T: trait> &T, &mut T)]
pub trait ToPriority {
    fn priority(&self) -> usize;
}

impl ToPriority for usize {
    fn priority(&self) -> usize {
        *self
    }
}

impl ToPriority for bool {
    fn priority(&self) -> usize {
        if *self {
            0
        } else {
            1
        }
    }
}

pub trait Transformer<S, Len: Length> {
    type Output: Debug;
    fn transform<W: Word<Symbol = S, Length = Len>>(&self, input: W) -> Self::Output;
}

impl<Ts> Transformer<SymbolOf<Ts>, FiniteLength> for Ts
where
    Ts: Successor + Pointed,
    Ts::Color: Clone + Default,
{
    type Output = Ts::Color;

    fn transform<W: Word<Symbol = SymbolOf<Ts>, Length = FiniteLength>>(
        &self,
        input: W,
    ) -> Self::Output {
        if let Some(ReachedColor(c)) = self.induced(&input, self.initial()) {
            c
        } else {
            Default::default()
        }
    }
}

impl<Ts> Transformer<SymbolOf<Ts>, InfiniteLength> for Ts
where
    Ts: Successor + Pointed,
    Ts::Color: Clone,
{
    type Output = BTreeSet<Ts::Color>;

    fn transform<W: Word<Symbol = SymbolOf<Ts>, Length = InfiniteLength>>(
        &self,
        input: W,
    ) -> Self::Output {
        if let Some(InfinitySet(c)) = self.induced(&input, self.initial()) {
            c
        } else {
            unreachable!()
        }
    }
}

pub trait Acceptor<S, K> {
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = K, Symbol = S>;
}

impl<S: Symbol, T: Transformer<S, FiniteLength, Output = bool>> Acceptor<S, FiniteLength> for T {
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = FiniteLength, Symbol = S>,
    {
        self.transform(word)
    }
}

pub trait ReducesTo<T = bool> {
    fn reduce(self) -> T;
}

impl ReducesTo<bool> for bool {
    fn reduce(self) -> bool {
        self
    }
}

impl ReducesTo<bool> for usize {
    fn reduce(self) -> bool {
        (self % 2) == 0
    }
}

impl ReducesTo<bool> for BTreeSet<bool> {
    fn reduce(self) -> bool {
        self.into_iter().any(|x| x)
    }
}

impl ReducesTo<bool> for BTreeSet<usize> {
    fn reduce(self) -> bool {
        self.into_iter().min().unwrap() % 2 == 0
    }
}

impl<S, T> Acceptor<S, InfiniteLength> for T
where
    S: Symbol,
    T: Transformer<S, InfiniteLength>,
    T::Output: ReducesTo<bool>,
{
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = InfiniteLength, Symbol = S>,
    {
        let transformed = self.transform(word);
        transformed.reduce()
    }
}

type DfaProductReduced<L, R> = MapColors<MatchingProduct<L, R>, fn((bool, bool)) -> bool>;

pub trait IsDfa:
    Successor<Color = bool, Position = OnStates>
    + Pointed
    + Sized
    + Acceptor<SymbolOf<Self>, FiniteLength>
    + Transformer<SymbolOf<Self>, FiniteLength, Output = bool>
{
    fn dfa_give_word(&self) -> Option<Vec<SymbolOf<Self>>> {
        self.minimal_representatives().find_map(|(mr, index)| {
            if self.state_color(index) {
                Some(mr)
            } else {
                None
            }
        })
    }

    fn dfa_is_empty(&self) -> bool {
        self.dfa_give_word().is_none()
    }

    fn union<Ts: IsDfa<Alphabet = Self::Alphabet>>(self, other: Ts) -> DfaProductReduced<Self, Ts> {
        self.product(other).map_colors(|(a, b)| a || b)
    }

    fn intersection<Ts: IsDfa<Alphabet = Self::Alphabet>>(
        self,
        other: Ts,
    ) -> DfaProductReduced<Self, Ts> {
        self.product(other).map_colors(|(a, b)| a && b)
    }

    fn negation(self) -> MapColors<Self, fn(bool) -> bool> {
        self.map_colors(|x| !x)
    }
}

impl<Ts> IsDfa for Ts where
    Ts: Successor<Color = bool, Position = OnStates>
        + Pointed
        + Sized
        + Acceptor<SymbolOf<Self>, FiniteLength>
        + Transformer<SymbolOf<Self>, FiniteLength, Output = bool>
{
}

pub trait IsDba:
    Successor<Color = bool>
    + Pointed
    + Sized
    + Acceptor<SymbolOf<Self>, InfiniteLength>
    + Transformer<SymbolOf<Self>, InfiniteLength, Output = BTreeSet<bool>>
{
    fn dba_give_word(&self) -> Option<OmegaWord<SymbolOf<Self>, InfiniteLength>>
    where
        Self: FiniteState,
    {
        for good_scc in self.sccs().iter().filter(|scc| self.is_reachable(scc[0])) {
            if let Some(full_word) = good_scc.maximal_word() {
                let SeenColors(colors) = self
                    .induced(&full_word, self.initial())
                    .expect("word is valid");
                if colors.contains(&true) {
                    let base = self
                        .word_from_to(self.initial(), good_scc[0])
                        .expect("we know this is reachable");
                    return Some(OmegaWord::from_parts(base, full_word));
                }
            }
        }
        None
    }

    fn dba_is_empty(&self) -> bool
    where
        Self: FiniteState,
    {
        self.dba_give_word().is_none()
    }
}

impl<Ts> IsDba for Ts where
    Ts: Successor<Color = bool>
        + Pointed
        + Sized
        + Acceptor<SymbolOf<Self>, InfiniteLength>
        + Transformer<SymbolOf<Self>, InfiniteLength, Output = BTreeSet<bool>>
{
}

pub trait IsDpa:
    Successor<Color = usize>
    + Pointed
    + Sized
    + Acceptor<SymbolOf<Self>, InfiniteLength>
    + Transformer<SymbolOf<Self>, InfiniteLength, Output = BTreeSet<usize>>
{
}

impl<Ts> IsDpa for Ts where
    Ts: Successor<Color = usize>
        + Pointed
        + Sized
        + Acceptor<SymbolOf<Self>, InfiniteLength>
        + Transformer<SymbolOf<Self>, InfiniteLength, Output = BTreeSet<usize>>
{
}

#[cfg(test)]
mod tests {
    use super::IsDfa;
    use crate::{
        alphabet::{self, Simple},
        automaton::{Acceptor, IsDba, Transformer},
        ts::{HasColorMut, HasMutableStates, IndexTS, Pointed, Product, Sproutable, Successor},
        upw,
        word::OmegaWord,
        InfiniteLength,
    };

    #[test]
    fn dbas() {
        let mut dba = super::DBA::new(Simple::from_iter(['a', 'b']));
        let q1 = dba.add_state(());
        let q0 = dba.initial();

        let _e0 = dba.add_edge(q0, 'a', q1, true);
        let _e1 = dba.add_edge(q0, 'b', q0, false);
        let _e2 = dba.add_edge(q1, 'a', q1, true);
        let _e3 = dba.add_edge(q1, 'b', q0, false);
        assert!(dba.accepts(OmegaWord::new("abb", InfiniteLength(3, 0))));
        assert!(!dba.accepts(OmegaWord::new("b", InfiniteLength(1, 0))));
        assert!(dba.accepts(upw!("a")));
        assert!(!dba.accepts(upw!("b")));

        assert!(!dba.dba_is_empty());
        println!("{:?}", dba.dba_give_word());

        println!("{:?}", dba);
    }

    #[test]
    fn dfas_and_boolean_operations() {
        let mut dfa = super::DFA::new(Simple::new(['a', 'b']));
        let s0 = dfa.initial();
        dfa.state_mut(s0).unwrap().set_color(true);
        let s1 = dfa.add_state(false);
        let _e0 = dfa.add_edge(s0, 'a', s1, ());
        let _e1 = dfa.add_edge(s0, 'b', s0, ());
        let _e2 = dfa.add_edge(s1, 'a', s1, ());
        let _e3 = dfa.add_edge(s1, 'b', s0, ());

        assert!(!dfa.dfa_is_empty());
        assert_eq!(dfa.dfa_give_word(), Some(vec![]));

        let dfb = dfa.clone();

        assert!(dfa.accepts("ababab"));
        assert!(!dfa.accepts("a"));

        let notdfa = (&dfa).negation();
        assert!(!notdfa.accepts("ababab"));
        assert!(notdfa.accepts("a"));

        let intersection = (&dfa).intersection(&notdfa);
        assert!(!intersection.accepts("ababab"));
        assert!(!intersection.accepts("a"));

        let union = (&dfa).union(&notdfa);
        assert!(union.accepts("ababab"));
        assert!(union.accepts("a"));
    }
}
