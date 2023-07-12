use std::marker::PhantomData;

use tracing::trace;

use crate::{
    acceptance::{HasAcceptance, Parity},
    alphabet::{Alphabet, HasAlphabet},
    ts::{
        finite::ReachedState, infinite::InfinitySet, Congruence, HasMutableStates, HasStates,
        IndexTS, IndexTSStates, Pointed, Sproutable, State, StateColored, StateIndex, Successor,
        TransitionSystem,
    },
    Color, FiniteLength, InfiniteLength, Length, Word,
};

pub trait Acceptor<S, K> {
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = K, Symbol = S>;
}

pub struct Automaton<A: Alphabet, Q, C, Acc> {
    ts: IndexTS<A, Q, C>,
    initial: StateIndex,
    _acc: PhantomData<Acc>,
}

impl<A: Alphabet, Q: Color, C: Color, Acc> StateColored for Automaton<A, Q, C, Acc> {
    type StateColor = Q;

    fn state_color(&self, index: StateIndex) -> &Self::StateColor {
        self.ts.state_color(index)
    }
}

impl<A: Alphabet, Q: Color, C: Color, Acc> HasStates for Automaton<A, Q, C, Acc> {
    type State<'this> = &'this State<Q> where Self:'this;

    type StatesIter<'this> = IndexTSStates<'this, Q>
    where
        Self: 'this;

    fn state(&self, index: StateIndex) -> Option<Self::State<'_>> {
        self.ts.state(index)
    }

    fn states_iter(&self) -> Self::StatesIter<'_> {
        self.ts.states_iter()
    }
}

impl<A: Alphabet, Q: Color, C: Color, Acc> HasAlphabet for Automaton<A, Q, C, Acc> {
    type Alphabet = A;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}

impl<A: Alphabet, Q: Color, C: Color, Acc> Successor for Automaton<A, Q, C, Acc> {
    type EdgeColor = C;

    fn successor(
        &self,
        state: StateIndex,
        symbol: crate::alphabet::SymbolOf<Self>,
    ) -> Option<crate::ts::Transition<'_, crate::alphabet::SymbolOf<Self>, Self::EdgeColor>> {
        self.ts.successor(state, symbol)
    }
}

impl<A: Alphabet, Q: Color, C: Color, Acc> Pointed for Automaton<A, Q, C, Acc> {
    fn initial(&self) -> StateIndex {
        self.initial
    }
}

impl<A: Alphabet, Q: Color, C: Color, Acc> Automaton<A, Q, C, Acc> {
    pub fn new(alphabet: A) -> Self
    where
        Q: Default,
    {
        let mut ts = IndexTS::new(alphabet);
        let initial = ts.add_state(Q::default());
        Self {
            ts,
            initial,
            _acc: PhantomData,
        }
    }

    pub fn from_ts(ts: IndexTS<A, Q, C>, initial: StateIndex) -> Self {
        Self {
            ts,
            initial,
            _acc: PhantomData,
        }
    }
}

impl<A: Alphabet, Q: Color, C: Color, Acc> HasMutableStates for Automaton<A, Q, C, Acc> {
    type StateMut<'this> = &'this mut State<Q> where Self:'this;

    fn state_mut(&mut self, index: StateIndex) -> Option<Self::StateMut<'_>> {
        self.ts.state_mut(index)
    }
}

impl<A: Alphabet, Q: Color, C: Color, Acc> Sproutable for Automaton<A, Q, C, Acc> {
    fn add_state(&mut self, color: Self::StateColor) -> StateIndex {
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
        X: Into<StateIndex>,
        Y: Into<StateIndex>,
    {
        self.ts.add_edge(from, on, to, color)
    }
}

impl<A: Alphabet, C: Color, Acc> Acceptor<A::Symbol, FiniteLength> for Automaton<A, bool, C, Acc> {
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = FiniteLength, Symbol = A::Symbol>,
    {
        if let Some(ReachedState(q)) = self.ts.induced(&word, self.initial) {
            trace!("Reached state: {q}");
            *self.ts.state_color(q)
        } else {
            trace!("No reached state");
            false
        }
    }
}

pub struct ReachabilityAcceptance;
pub type Dfa<A, C = ()> = Automaton<A, bool, C, ReachabilityAcceptance>;

pub struct BuchiAcceptance;
pub struct BuchiStateAcceptance;
pub type Dbsa<A, C = ()> = Automaton<A, bool, C, BuchiStateAcceptance>;
pub type Dba<A, C = ()> = Automaton<A, C, bool, BuchiAcceptance>;

impl<A: Alphabet, C: Color> Acceptor<A::Symbol, InfiniteLength>
    for Automaton<A, bool, C, BuchiStateAcceptance>
{
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = InfiniteLength, Symbol = A::Symbol>,
    {
        todo!()
    }
}
impl<A: Alphabet, C: Color> Acceptor<A::Symbol, InfiniteLength>
    for Automaton<A, C, bool, BuchiAcceptance>
{
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = InfiniteLength, Symbol = A::Symbol>,
    {
        if let Some(InfinitySet(qs)) = self.ts.induced(&word, self.initial) {
            trace!("Edge colors in the infinity set are: {qs:?}", qs = qs);
            qs.iter().any(|q| *q)
        } else {
            trace!("No infinite run possible");
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        alphabet::{self, Simple},
        automaton::Dba,
        ts::{HasColorMut, HasMutableStates, IndexTS, Pointed, Sproutable},
        word::RawWithLength,
        InfiniteLength,
    };

    use super::{Acceptor, Dfa};
    #[test]
    #[tracing_test::traced_test]
    fn dbas() {
        let mut dba = Dba::new(Simple::from_iter(['a', 'b']));
        let q0 = dba.initial();
        let q1 = dba.add_state(());

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
        let mut dfa = Dfa::new(Simple::from_iter(['a', 'b']));
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
