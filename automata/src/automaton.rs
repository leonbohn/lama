use crate::{
    acceptance::{
        AcceptanceCondition, BuchiAcceptance, HasAcceptanceCondition, ParityAcceptance,
        ReachabilityAcceptance,
    },
    run::Run,
    ts::{
        deterministic::Deterministic, HasTransitionSystem, OutputOf, Pointed, StateIndex,
        TransitionSystem,
    },
    words::Word,
    Alphabet, Boundedness, FiniteKind, InfiniteKind,
};

pub trait Automaton<K> {
    type Q: StateIndex;
    type S: Alphabet;
    type Kind: Boundedness;
    type Acc: AcceptanceCondition;

    fn initial(&self) -> Self::Q;

    fn successor(&self, state: Self::Q, symbol: &Self::S) -> Option<Self::Q>;

    fn accepts<W: Word<S = Self::S, Kind = Self::Kind>>(&self, what: &W) -> bool;
}

pub struct Combined<TS, Acc> {
    ts: TS,
    acc: Acc,
}

impl<TS: Default, Acc: Default> Combined<TS, Acc> {
    pub fn new() -> Self {
        Self {
            ts: TS::default(),
            acc: Acc::default(),
        }
    }
}

pub type Dfa<TS = Deterministic<char, u32>> = Combined<TS, ReachabilityAcceptance<OutputOf<TS>>>;
pub type Dba<TS = Deterministic<char, u32>> = Combined<TS, BuchiAcceptance<OutputOf<TS>>>;
pub type Dpa<TS = Deterministic<char, u32>> = Combined<TS, ParityAcceptance<OutputOf<TS>>>;
