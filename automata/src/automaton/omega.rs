use bit_set::BitSet;
use hoars::HoaAutomaton;
use itertools::Itertools;
use tracing::warn;

use crate::{congruence::FORC, hoa::HoaAlphabet, prelude::*, Set};
#[cfg(test)]
use pretty_assertions::assert_eq;

use super::WithInitial;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct AcceptanceMask(BitSet);

impl AcceptanceMask {
    pub fn max(&self) -> Option<usize> {
        self.iter().max()
    }

    pub fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        self.0.iter()
    }

    pub fn min(&self) -> Option<usize> {
        self.iter().min()
    }

    pub fn as_priority(&self) -> usize {
        let mut it = self.iter();
        let Some(priority) = it.next() else {
            panic!("No priority set");
        };
        if it.next().is_some() {
            warn!("more than one priority is set! {:?}", self.0);
        }
        priority
    }
}

impl Show for AcceptanceMask {
    fn show(&self) -> String {
        self.iter().map(|i| format!("{{{i}}}")).join(", ")
    }

    fn show_collection<'a, I>(iter: I) -> String
    where
        Self: 'a,
        I: IntoIterator<Item = &'a Self>,
        I::IntoIter: DoubleEndedIterator,
    {
        unimplemented!()
    }
}

impl From<&hoars::AcceptanceSignature> for AcceptanceMask {
    fn from(value: &hoars::AcceptanceSignature) -> Self {
        Self(BitSet::from_iter(value.iter().map(|&i| {
            i.try_into().expect("Could not cast {i} to usize")
        })))
    }
}

#[derive(Debug, Clone, Eq, Copy, PartialEq, Ord, PartialOrd)]
pub enum OmegaAcceptanceCondition {
    Parity,
    Buchi,
    Rabin,
    Streett,
    MaxParity,
    CoBuchi,
    Reachability,
    Safety,
}

impl OmegaAcceptanceCondition {
    pub fn satisfied(&self, infset: &Set<AcceptanceMask>) -> bool {
        match self {
            OmegaAcceptanceCondition::Parity => infset
                .iter()
                .map(|x| x.as_priority())
                .min()
                .map(|x| x % 2 == 0)
                .unwrap_or(false),
            _ => unimplemented!(),
        }
    }
}

pub struct OmegaAutomaton<A: Alphabet> {
    pub(super) ts: WithInitial<NTS<A, usize, AcceptanceMask>>,
    pub(super) acc: OmegaAcceptanceCondition,
}

pub struct DeterministicOmegaAutomaton<A: Alphabet> {
    pub(super) ts: WithInitial<DTS<A, usize, AcceptanceMask>>,
    pub(super) acc: OmegaAcceptanceCondition,
}

impl<A: Alphabet> OmegaAutomaton<A> {
    pub fn new(
        ts: WithInitial<NTS<A, usize, AcceptanceMask>>,
        acc: OmegaAcceptanceCondition,
    ) -> Self {
        Self { ts, acc }
    }

    pub fn to_deterministic(&self) -> Option<DeterministicOmegaAutomaton<A>> {
        self.try_into().ok()
    }

    pub fn prefix_congruence(&self) -> RightCongruence<A> {
        todo!()
    }

    pub fn underlying_forc(&self) -> FORC<A, bool> {
        todo!()
    }
}

impl<A: Alphabet> DeterministicOmegaAutomaton<A> {
    pub fn new(
        ts: WithInitial<DTS<A, usize, AcceptanceMask>>,
        acc: OmegaAcceptanceCondition,
    ) -> Self {
        Self { ts, acc }
    }
}

impl<A: Alphabet> TryFrom<OmegaAutomaton<A>> for DeterministicOmegaAutomaton<A> {
    /// The only way this can go wrong is if the given automaton is not deterministic.
    type Error = ();

    fn try_from(value: OmegaAutomaton<A>) -> Result<Self, Self::Error> {
        let dts = value.ts.try_into()?;
        Ok(Self::new(dts, value.acc))
    }
}

impl<A: Alphabet> TryFrom<&OmegaAutomaton<A>> for DeterministicOmegaAutomaton<A> {
    /// The only way this can go wrong is if the given automaton is not deterministic.
    type Error = ();

    fn try_from(value: &OmegaAutomaton<A>) -> Result<Self, Self::Error> {
        let dts = (&value.ts).try_into()?;
        Ok(Self::new(dts, value.acc))
    }
}

impl<A: Alphabet> Pointed for OmegaAutomaton<A> {
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

impl<A: Alphabet> Pointed for DeterministicOmegaAutomaton<A> {
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

impl<A: Alphabet> TransitionSystem for OmegaAutomaton<A> {
    type StateIndex = usize;

    type StateColor = usize;

    type EdgeColor = AcceptanceMask;

    type TransitionRef<'this> = <DTS<A, usize, AcceptanceMask> as TransitionSystem>::TransitionRef<'this>
    where
        Self: 'this;

    type EdgesFromIter<'this> = <DTS<A, usize, AcceptanceMask> as TransitionSystem>::EdgesFromIter<'this>
    where
        Self: 'this;

    type StateIndices<'this> = <DTS<A, usize, AcceptanceMask> as TransitionSystem>::StateIndices<'this>
    where
        Self: 'this;

    type Alphabet = A;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts.state_indices()
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        self.ts.edges_from(state.to_index(self)?)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        self.ts.state_color(state)
    }
}

impl<A: Alphabet> TransitionSystem for DeterministicOmegaAutomaton<A> {
    type StateIndex = usize;

    type StateColor = usize;

    type EdgeColor = AcceptanceMask;

    type TransitionRef<'this> = <DTS<A, usize, AcceptanceMask> as TransitionSystem>::TransitionRef<'this>
    where
        Self: 'this;

    type EdgesFromIter<'this> = <DTS<A, usize, AcceptanceMask> as TransitionSystem>::EdgesFromIter<'this>
    where
        Self: 'this;

    type StateIndices<'this> = <DTS<A, usize, AcceptanceMask> as TransitionSystem>::StateIndices<'this>
    where
        Self: 'this;

    type Alphabet = A;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts.state_indices()
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        self.ts.edges_from(state.to_index(self)?)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        self.ts.state_color(state)
    }
}

impl<A: Alphabet> PredecessorIterable for OmegaAutomaton<A> {
    type PreTransitionRef<'this> = <DTS<A, usize, AcceptanceMask> as PredecessorIterable>::PreTransitionRef<'this>
    where
        Self: 'this;

    type EdgesToIter<'this> =  <DTS<A, usize, AcceptanceMask> as PredecessorIterable>::EdgesToIter<'this>
    where
        Self: 'this;

    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        self.ts.predecessors(state)
    }
}

impl<A: Alphabet> PredecessorIterable for DeterministicOmegaAutomaton<A> {
    type PreTransitionRef<'this> = <DTS<A, usize, AcceptanceMask> as PredecessorIterable>::PreTransitionRef<'this>
    where
        Self: 'this;

    type EdgesToIter<'this> =  <DTS<A, usize, AcceptanceMask> as PredecessorIterable>::EdgesToIter<'this>
    where
        Self: 'this;

    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        self.ts.predecessors(state)
    }
}

impl<A: Alphabet> Deterministic for DeterministicOmegaAutomaton<A> {
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        self.ts.transition(state.to_index(self)?, symbol)
    }
}

impl<A: Alphabet> OmegaWordAcceptor<A::Symbol> for DeterministicOmegaAutomaton<A> {
    fn accepts_omega<W: OmegaWord<A::Symbol>>(&self, word: W) -> bool {
        match self.omega_run(word) {
            Ok(r) => self.acc.satisfied(&r.infinity_set(self)),
            Err(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn omega_acceptance_conditions() {
        assert!(true)
    }
}
