use std::marker::PhantomData;

use tracing::trace;

use crate::{
    alphabet::{Alphabet, HasAlphabet, SymbolOf},
    ts::{
        finite::{ReachedColor, ReachedState},
        infinite::InfinitySet,
        Congruence, EdgeColor, HasMutableStates, HasStates, IndexTS, IndexType, OnEdges, OnStates,
        Pointed, Sproutable, State, StateColor, StateIndex, Successor, Transition,
        TransitionSystem,
    },
    Color, FiniteLength, InfiniteLength, Length, Word,
};

#[derive(Debug, Clone, PartialEq)]
pub struct WithInitial<Ts: Successor>(Ts, Ts::StateIndex);

mod boilerplate_impls {
    use crate::ts::ColorPosition;

    use super::*;

    // impl<Ts> std::ops::Deref for WithInitial<Ts>
    // where
    //     Ts: Successor,
    // {
    //     type Target = Ts;

    //     fn deref(&self) -> &Self::Target {
    //         &self.0
    //     }
    // }

    // impl<Ts> std::ops::DerefMut for WithInitial<Ts>
    // where
    //     Ts: Successor,
    // {
    //     fn deref_mut(&mut self) -> &mut Self::Target {
    //         &mut self.0
    //     }
    // }

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
    }

    impl<Ts: Successor> WithInitial<Ts> {
        pub fn ts(&self) -> &Ts {
            &self.0
        }
        pub fn ts_mut(&mut self) -> &mut Ts {
            &mut self.0
        }
    }
    impl<Ts: Successor + Sproutable> Sproutable for WithInitial<Ts> {
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

pub trait Acceptor<S, K> {
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = K, Symbol = S>;
}

impl<A: Alphabet, Idx: IndexType> Acceptor<A::Symbol, FiniteLength> for DFA<A, Idx> {
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = FiniteLength, Symbol = A::Symbol>,
    {
        if let Some(ReachedColor(c)) = self.induced(&word, self.initial()) {
            c
        } else {
            false
        }
    }
}

impl<A: Alphabet, Idx: IndexType> Acceptor<A::Symbol, InfiniteLength> for DBA<A, Idx> {
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = InfiniteLength, Symbol = A::Symbol>,
    {
        if let Some(InfinitySet(qs)) = self.induced(&word, self.initial()) {
            qs.into_iter().any(|q| q)
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        alphabet::{self, Simple},
        automaton::Acceptor,
        ts::{HasColorMut, HasMutableStates, IndexTS, Pointed, Sproutable},
        upw,
        word::RawWithLength,
        InfiniteLength,
    };

    #[test]
    #[tracing_test::traced_test]
    fn dbas() {
        let mut dba = super::DBA::new(Simple::from_iter(['a', 'b']));
        let q1 = dba.add_state(());
        let q0 = dba.initial();

        let _e0 = dba.add_edge(q0, 'a', q1, true);
        let _e1 = dba.add_edge(q0, 'b', q0, false);
        let _e2 = dba.add_edge(q1, 'a', q1, true);
        let _e3 = dba.add_edge(q1, 'b', q0, false);
        assert!(dba.accepts(RawWithLength::new("abb", InfiniteLength(3, 0))));
        assert!(!dba.accepts(RawWithLength::new("b", InfiniteLength(1, 0))));
        assert!(dba.accepts(upw!("a")));
        assert!(!dba.accepts(upw!("b")))
    }

    #[test]
    #[tracing_test::traced_test]
    fn dfas() {
        let mut dfa = super::DFA::new(Simple::new(['a', 'b']));
        let s0 = dfa.initial();
        dfa.state_mut(s0).unwrap().set_color(true);
        let s1 = dfa.add_state(false);
        let _e0 = dfa.add_edge(s0, 'a', s1, ());
        let _e1 = dfa.add_edge(s0, 'b', s0, ());
        let _e2 = dfa.add_edge(s1, 'a', s1, ());
        let _e3 = dfa.add_edge(s1, 'b', s0, ());

        assert!(dfa.accepts("ababab"));
        assert!(!dfa.accepts("a"));
    }
}
