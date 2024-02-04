use std::collections::BTreeSet;

use impl_tools::autoimpl;

use crate::{
    alphabet::Alphabet,
    mapping::Mapping,
    ts::{
        finite::{ReachedColor, ReachedState},
        infinite::InfinitySet,
        Congruence, IndexTS, Induced, StateIndex, Successor, TransitionSystem,
    },
    FiniteLength, InfiniteLength, Length, Word,
};

/// Abstracts the concept of an acceptance condition, which is in essence simply a predicate on some
/// induced object. This means that objects of this type can be classified into accepting and rejecting
/// which is represented by the boolean values true and false.
#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait Acceptance {
    /// The type of induced objects that this acceptance can classify.
    type Over: Induced;

    /// Returns true if the given induced object is accepted by this acceptance condition.
    fn is_accepting(&self, state: Self::Over) -> bool;
}

/// Implementors of this trait possess an acceptance condition.
#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait HasAcceptance {
    /// The type of acceptance condition that this object possesses.
    type Acceptance: Acceptance;

    /// Returns a reference to the acceptance condition of this object.
    fn acceptance(&self) -> &Self::Acceptance;
}

/// Abstracts the concept of a parity, which can either be even or odd.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Parity {
    Even,
    Odd,
}

impl From<Parity> for bool {
    fn from(parity: Parity) -> Self {
        match parity {
            Parity::Even => false,
            Parity::Odd => true,
        }
    }
}

impl From<Parity> for u8 {
    fn from(parity: Parity) -> Self {
        match parity {
            Parity::Even => 0,
            Parity::Odd => 1,
        }
    }
}

/// A reachability condition simply classifies objects based
/// on whether they are included in a set or not. Most notably, this is used to
/// define deterministic finite automata (DFA)s and can be viewed as a predicate
/// on the set of states of a transition system.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReachabilityCondition;

/// Disambiguates between different types of parity conditions. These differ only
/// in the way that they decide acceptance.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(missing_docs)]
pub enum ParityConditionKind {
    MinEven,
    MinOdd,
    MaxEven,
    MaxOdd,
}

/// A parity condition is only relevant for automata over infinite words. It classifies
/// a run based on the set of priorities that occur infinitely often in the run.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParityCondition(pub ParityConditionKind);

/// A Buchi condition can only be used for automata over infinite words and it classifies
/// a run as accepting if and only if it visits a set of states infinitely often.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BuchiCondition;

pub trait Acceptor<A: Alphabet, L: Length> {
    fn accepts<W: Word<Symbol = A::Symbol, Length = L>>(&self, word: &W) -> bool;
}

pub struct Automaton<Ts, Acc> {
    ts: Ts,
    initial: StateIndex,
    acc: Acc,
}

impl<Ts> Acceptor<Ts::Alphabet, FiniteLength> for Automaton<Ts, ReachabilityCondition>
where
    Ts: Successor,
    Ts::StateColor: GivesParity,
{
    fn accepts<W: Word<Symbol = <Ts::Alphabet as Alphabet>::Symbol, Length = FiniteLength>>(
        &self,
        word: &W,
    ) -> bool {
        if let Some(ReachedColor(c)) = self.ts.induced(word, self.initial) {
            c.parity().into()
        } else {
            false
        }
    }
}

pub type DFA<A, Idx = usize, Q = bool, E = Void> =
    Automaton<IndexTS<A, Idx, Q, E>, ReachabilityCondition>;

impl<Ts> Acceptor<Ts::Alphabet, InfiniteLength> for Automaton<Ts, BuchiCondition>
where
    Ts: Successor,
    Ts::EdgeColor: GivesParity,
{
    fn accepts<W: Word<Symbol = <Ts::Alphabet as Alphabet>::Symbol, Length = InfiniteLength>>(
        &self,
        word: &W,
    ) -> bool {
        if let Some(InfinitySet(inf)) = self.ts.induced(word, self.initial) {
            inf.into_iter().any(|c| c.parity().into())
        } else {
            false
        }
    }
}

pub type DBA<A, Idx = usize, Q = Void, E = bool> = Automaton<IndexTS<A, Idx, Q, E>, BuchiCondition>;

impl<Ts> Acceptor<Ts::Alphabet, InfiniteLength> for Automaton<Ts, ParityCondition>
where
    Ts: Successor,
    Ts::EdgeColor: GivesParity + Ord,
{
    fn accepts<W: Word<Symbol = <Ts::Alphabet as Alphabet>::Symbol, Length = InfiniteLength>>(
        &self,
        word: &W,
    ) -> bool {
        if let Some(InfinitySet(inf)) = self.ts.induced(word, self.initial) {
            inf.into_iter()
                .min()
                .expect("Infinity set cannot be empty!")
                .parity()
                .into()
        } else {
            false
        }
    }
}

pub type DPA<A, Idx = usize, Q = Void, E = u8> = Automaton<IndexTS<A, Idx, Q, E>, ParityCondition>;
