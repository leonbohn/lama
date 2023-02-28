use crate::{
    acceptance::AcceptanceCondition,
    ts::{StateIndex, StateOf, TransitionSystem},
    words::Word,
    Alphabet,
};

pub trait Automaton<K> {
    type Q: StateIndex;
    type S: Alphabet;
    type Acc: AcceptanceCondition;

    fn initial(&self) -> Self::Q;

    fn successor(&self, state: Self::Q, symbol: &Self::S) -> Option<Self::Q>;

    fn accepts<W: Word<S = Self::S, Kind = K>>(&self, what: &W) -> bool;
}

pub struct Combined<TS: TransitionSystem, Acc> {
    ts: TS,
    initial: TS::Q,
    acc: Acc,
}

// pub type Dfa<TS = Deterministic<char, u32>> = Combined<TS, ReachabilityAcceptance<OutputOf<TS>>>;
// pub type Dba<TS = Deterministic<char, u32>> = Combined<TS, BuchiAcceptance<OutputOf<TS>>>;
// pub type Dpa<TS = Deterministic<char, u32>> = Combined<TS, ParityAcceptance<OutputOf<TS>>>;
