use automata::{
    prelude::*, 
    automaton::{
        DeterministicOmegaAutomaton,
        OmegaAcceptanceCondition,
        Buchi
    }
};

use super::OmegaSample;

/// Used to define consistency checks on various types of omega acceptance conditions
/// required by the sprout algorithm for passively learning omega automata
pub trait ConsistencyCheck<A: Alphabet, S: Sproutable> {
    /// Checks if the given transition system is consistent with the sample
    fn consistent(&self, ts: S, sample: OmegaSample) -> bool;
    /// Returns an automaton with underlying transition system ts
    /// that is consistent with the sample
    fn consistent_automaton(&self, ts: S, sample: OmegaSample) -> DeterministicOmegaAutomaton<A>;
}

impl<A: Alphabet, S: Sproutable> ConsistencyCheck<A, S> for Buchi {
    fn consistent(&self, ts: S, sample: OmegaSample) -> bool {
        todo!()
    }
    fn consistent_automaton(&self, ts: S, sample: OmegaSample) -> DeterministicOmegaAutomaton<A> {
        todo!()
    }
}
