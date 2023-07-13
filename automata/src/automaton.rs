use std::marker::PhantomData;

use tracing::trace;

use crate::{
    alphabet::{Alphabet, HasAlphabet},
    ts::{
        finite::{ReachedColor, ReachedState},
        infinite::InfinitySet,
        Congruence, HasMutableStates, HasStates, IndexTS, IndexType, Pointed, Sproutable, State,
        StateColored, StateIndex, Successor, TransitionSystem,
    },
    Color, FiniteLength, InfiniteLength, Length, Word,
};

pub trait Acceptor<S, K> {
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = K, Symbol = S>;
}

pub struct Automaton<
    A: Alphabet,
    Q,
    C,
    Acc,
    Idx = usize,
    Ts: Successor<Index = Idx, EdgeColor = C, StateColor = Q, Alphabet = A> = IndexTS<A, Q, C, Idx>,
> {
    ts: Ts,
    initial: Idx,
    _acc: PhantomData<Acc>,
}

impl<
        A: Alphabet,
        Q,
        C,
        Acc,
        Idx,
        Ts: Successor<Index = Idx, EdgeColor = C, StateColor = Q, Alphabet = A>,
    > Automaton<A, Q, C, Acc, Idx, Ts>
{
    pub fn ts_with_initial(ts: Ts, initial: Idx) -> Self {
        Self {
            ts,
            initial,
            _acc: PhantomData,
        }
    }
}

impl<
        A: Alphabet,
        Q: Color,
        C,
        Acc,
        Idx: IndexType,
        Ts: Successor<Index = Idx, EdgeColor = C, StateColor = Q, Alphabet = A>,
    > StateColored for Automaton<A, Q, C, Acc, Idx, Ts>
{
    type Index = Idx;
    type StateColor = Q;

    fn state_color(&self, index: Self::Index) -> &Self::StateColor {
        self.ts.state_color(index)
    }
}

impl<
        A: Alphabet,
        Q: Color,
        C,
        Acc,
        Idx: IndexType,
        Ts: Successor<Index = Idx, EdgeColor = C, StateColor = Q, Alphabet = A>
            + HasStates<Index = Idx, StateColor = Q>,
    > HasStates for Automaton<A, Q, C, Acc, Idx, Ts>
{
    #[doc = " The type of the states."]
    type State<'this> = Ts::State<'this>
    where
        Self: 'this;

    #[doc = " The type of the iterator over the states."]
    type StatesIter<'this> = Ts::StatesIter<'this>
    where
        Self: 'this;

    #[doc = " Returns a reference to the state with the given index, if it exists and `None` otherwise."]
    fn state(&self, index: Self::Index) -> Option<Self::State<'_>> {
        self.ts.state(index)
    }

    #[doc = " Returns an iterator over the states of the implementor."]
    fn states_iter(&self) -> Self::StatesIter<'_> {
        self.ts.states_iter()
    }
}

impl<
        A: Alphabet,
        Q: Color,
        C,
        Acc,
        Idx: IndexType,
        Ts: Successor<Index = Idx, EdgeColor = C, StateColor = Q, Alphabet = A>
            + HasAlphabet<Alphabet = A>,
    > HasAlphabet for Automaton<A, Q, C, Acc, Idx, Ts>
{
    type Alphabet = A;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}

impl<
        A: Alphabet,
        Q: Color,
        C: Color,
        Acc,
        Idx: IndexType,
        Ts: Successor<Index = Idx, EdgeColor = C, StateColor = Q, Alphabet = A>,
    > Successor for Automaton<A, Q, C, Acc, Idx, Ts>
{
    type EdgeColor = C;

    fn successor(
        &self,
        state: Self::Index,
        symbol: crate::alphabet::SymbolOf<Self>,
    ) -> Option<
        crate::ts::Transition<'_, Self::Index, crate::alphabet::SymbolOf<Self>, Self::EdgeColor>,
    > {
        self.ts.successor(state, symbol)
    }
}

impl<
        A: Alphabet,
        Q: Color,
        C: Color,
        Acc,
        Idx: IndexType,
        Ts: Successor<Index = Idx, EdgeColor = C, StateColor = Q, Alphabet = A>,
    > Pointed for Automaton<A, Q, C, Acc, Idx, Ts>
{
    fn initial(&self) -> Self::Index {
        self.initial
    }
}

impl<
        A: Alphabet,
        Q: Color,
        C: Color,
        Acc,
        Idx: IndexType,
        Ts: Successor<Index = Idx, EdgeColor = C, StateColor = Q, Alphabet = A> + HasMutableStates,
    > HasMutableStates for Automaton<A, Q, C, Acc, Idx, Ts>
{
    type StateMut<'this> = Ts::StateMut<'this> where Self:'this;

    fn state_mut(&mut self, index: Self::Index) -> Option<Self::StateMut<'_>> {
        self.ts.state_mut(index)
    }
}

impl<A: Alphabet, Q: Color + Default, C: Color, Acc> Sproutable
    for Automaton<A, Q, C, Acc, usize, IndexTS<A, Q, C, usize>>
{
    fn add_state(&mut self, color: Self::StateColor) -> Self::Index {
        self.ts.add_state(color)
    }

    fn add_edge<X, Y>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: Self::EdgeColor,
    ) -> crate::ts::EdgeIndex
    where
        X: Into<Self::Index>,
        Y: Into<Self::Index>,
    {
        self.ts.add_edge(from, on, to, color)
    }

    fn new_empty(alphabet: Self::Alphabet) -> Self {
        let mut ts = IndexTS::new_empty(alphabet);
        let initial = ts.add_state(Q::default());
        Self {
            initial,
            ts,
            _acc: PhantomData,
        }
    }
}

pub struct ReachabilityAcceptance;
pub type Dfa<A, Idx = usize, C = (), Ts = IndexTS<A, bool, C, Idx>> =
    Automaton<A, bool, C, ReachabilityAcceptance, Idx, Ts>;

impl<
        A: Alphabet,
        Idx: IndexType,
        C: Color,
        Ts: Successor<Index = Idx, StateColor = bool, EdgeColor = C, Alphabet = A>,
    > Acceptor<A::Symbol, FiniteLength> for Automaton<A, bool, C, ReachabilityAcceptance, Idx, Ts>
{
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = FiniteLength, Symbol = A::Symbol>,
    {
        if let Some(ReachedColor(q)) = self.induced(&word, self.initial) {
            q
        } else {
            false
        }
    }
}

pub struct BuchiAcceptance;
pub struct BuchiStateAcceptance;
pub type Dbsa<A, Idx = usize, C = (), Ts = IndexTS<A, bool, C, Idx>> =
    Automaton<A, bool, C, BuchiStateAcceptance, Idx, Ts>;
pub type Dba<A, Idx = usize, C = (), Ts = IndexTS<A, C, bool, Idx>> =
    Automaton<A, C, bool, BuchiAcceptance, Idx, Ts>;

impl<
        A: Alphabet,
        Idx: IndexType,
        Q: Color,
        Ts: Successor<Index = Idx, StateColor = Q, EdgeColor = bool, Alphabet = A>,
    > Acceptor<A::Symbol, InfiniteLength> for Automaton<A, Q, bool, BuchiAcceptance, Idx, Ts>
{
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = InfiniteLength, Symbol = A::Symbol>,
    {
        if let Some(InfinitySet(qs)) = self.induced(&word, self.initial) {
            qs.into_iter().any(|q| q)
        } else {
            false
        }
    }
}

// impl<A: Alphabet, Idx, C: Color> Acceptor<A::Symbol, InfiniteLength>
//     for Automaton<A, bool, C, BuchiStateAcceptance, Idx>
// {
//     fn accepts<W>(&self, word: W) -> bool
//     where
//         W: Word<Length = InfiniteLength, Symbol = A::Symbol>,
//     {
//         todo!()
//     }
// }

// impl<A: Alphabet, Idx: IndexType, C: Color> Acceptor<A::Symbol, InfiniteLength>
//     for Automaton<A, C, bool, BuchiAcceptance, Idx>
// {
//     fn accepts<W>(&self, word: W) -> bool
//     where
//         W: Word<Length = InfiniteLength, Symbol = A::Symbol>,
//     {
//         if let Some(InfinitySet(qs)) = self.ts.induced(&word, self.initial) {
//             trace!("Edge colors in the infinity set are: {qs:?}", qs = qs);
//             qs.iter().any(|q| *q)
//         } else {
//             trace!("No infinite run possible");
//             false
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use crate::{
        alphabet::{self, Simple},
        automaton::{Acceptor, Dfa},
        ts::{HasColorMut, HasMutableStates, IndexTS, Pointed, Sproutable},
        word::RawWithLength,
        InfiniteLength,
    };

    #[test]
    #[tracing_test::traced_test]
    fn dbas() {
        let mut dba = super::Dba::new_empty(Simple::from_iter(['a', 'b']));
        let q1 = dba.add_state(());
        let q0 = dba.initial();

        let _e0 = dba.add_edge(q0, 'a', q1, true);
        let _e1 = dba.add_edge(q0, 'b', q0, false);
        let _e2 = dba.add_edge(q1, 'a', q1, true);
        let _e3 = dba.add_edge(q1, 'b', q0, false);
        assert!(dba.accepts(RawWithLength::new("abb", InfiniteLength(3, 0))));
        assert!(!dba.accepts(RawWithLength::new("b", InfiniteLength(1, 0))))
    }

    #[test]
    #[tracing_test::traced_test]
    fn dfas() {
        let mut dfa = Dfa::new_empty(Simple::from_iter(['a', 'b']));
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
