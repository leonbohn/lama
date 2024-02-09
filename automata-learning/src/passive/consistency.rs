use itertools::{Either, Itertools};
use std::ops::Not;

use automata::{
    prelude::*, 
    automaton::{
        DeterministicOmegaAutomaton,
        OmegaAcceptanceCondition,
        Buchi
    },
    Set
};

use super::OmegaSample;

/// Used to define consistency checks on various types of omega acceptance conditions
/// required by the sprout algorithm for passively learning omega automata
pub trait ConsistencyCheck<A:Alphabet, T: Deterministic> {
    /// Checks if the given transition system is consistent with the sample
    fn consistent(&self, ts: &T, sample: &OmegaSample, alph: A) -> bool;
    /// Returns an automaton with underlying transition system ts
    /// that is consistent with the sample
    fn consistent_automaton(&self, ts: &T, sample: &OmegaSample) -> DeterministicOmegaAutomaton<alphabet::Simple>;
}

impl<A, T> ConsistencyCheck<A, T> for Buchi
where
    A : Alphabet,
    T: Deterministic + Pointed,
    Reduced<char>: OmegaWord<<<T as TransitionSystem>::Alphabet as Alphabet>::Symbol>,
{
    fn consistent(&self, ts: &T, sample: &OmegaSample<Simple, bool>, alph: A) -> bool {
        // run transition system on sample words and
        // separate in escaping and non-escaping (successful) runs
        let (pos_successful, pos_escaping): (Vec<_>, Vec<_>) = sample
            .positive_words()
            .map(|w| ts.omega_run(w))
            .partition_map(|r| match r {
                Ok(v) => Either::Left(v),
                Err(v) => Either::Right(v),
            });
        let (neg_successful, neg_escaping): (Vec<_>, Vec<_>) = sample
            .negative_words()
            .map(|w| ts.omega_run(w))
            .partition_map(|r| match r {
                Ok(v) => Either::Left(v),
                Err(v) => Either::Right(v),
            });
        
        // reject if a pair escaping from the same state with the same escape string is found
        for (pos_path, neg_path) in pos_escaping.into_iter().cartesian_product(neg_escaping) {
            if pos_path.reached() == neg_path.reached() {
                // check if suffix same
                return false
            }
        }

        // check if the infinity set of a positive word is subset of
        // the union of all infinity sets of negative words (see paper for details)
        let neg_union: Set<_> = neg_successful
            .into_iter()
            .map(|r| {r.into_recurrent_state_indices()})
            .flatten()
            .collect();

        pos_successful
            .into_iter()
            .map(|r| {r.into_recurrent_state_indices().collect::<Set<_>>()})
            .any(|s| {s.is_subset(&neg_union)})
            .not()
    }
    fn consistent_automaton(&self, ts: &T, sample: &OmegaSample) -> DeterministicOmegaAutomaton<alphabet::Simple> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::passive::OmegaSample;
    use automata::{
        prelude::*, 
        automaton::{
            DeterministicOmegaAutomaton,
            OmegaAcceptanceCondition,
            Buchi
        }
    };
    use super::*;

    // default alphabet
    fn sigma() -> Simple {
        alphabet!(simple 'a', 'b')
    }

	#[test]
	fn buchi_both_escaping() {
        // build transition systems
        let ts = NTS::builder()
            .with_transitions([
                (0, 'a', (), 1)
            ])
            .default_color(())
            .deterministic()
            .with_initial(0);
        let ts2 = NTS::builder()
            .with_transitions([
                (0, 'a', (), 0)
            ])
            .default_color(())
            .deterministic()
            .with_initial(0);
        
        // build samples
        let sample1 = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [Reduced::periodic("a")],
            [Reduced::periodic("b")]
        );
        let sample2 = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [Reduced::periodic("a")],
            [Reduced::ultimately_periodic("a", "b")]
        );
        let sample3 = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [Reduced::ultimately_periodic("a", "b")],
            [Reduced::periodic("b")]
        );
        
        // words escape from different states
        assert_eq!(Buchi.consistent(&ts, &sample1, sigma()), true);
        // words escape from same state but with different exit strings
        assert_eq!(Buchi.consistent(&ts, &sample2, sigma()), true);
        // words escape from same state with same exit string
        assert_eq!(Buchi.consistent(&ts2, &sample3, sigma()), false);
	}

    #[test]
	fn buchi_one_escaping() {
        // build transition system
        let ts = NTS::builder()
            .with_transitions([
                (0, 'a', (), 0)
            ])
            .default_color(())
            .deterministic()
            .with_initial(0);
        
        // build sample
        let sample = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [Reduced::periodic("a")],
            [Reduced::periodic("b")]
        );
        
        // one word is escaping, the other is not
        assert_eq!(Buchi.consistent(&ts, &sample, sigma()), true);
	}

    #[test]
	fn buchi_consistency() {
        // build transition systems
        let ts = NTS::builder()
            .with_transitions([
                (0, 'b', (), 0),
                (0, 'a', (), 1),
                (1, 'b', (), 1)
            ])
            .default_color(())
            .deterministic()
            .with_initial(0);
        let ts2 = NTS::builder()
            .with_transitions([
                (0, 'b', (), 0),
                (0, 'a', (), 1),
                (1, 'a', (), 0)
            ])
            .default_color(())
            .deterministic()
            .with_initial(0);
        
        // build samples
        let sample1 = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [Reduced::ultimately_periodic("a", "b")],
            [Reduced::periodic("b")]
        );
        let sample2 = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [Reduced::periodic("b")],
            [Reduced::periodic("aab")]
        );
        let sample3 = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [Reduced::periodic("aab")],
            [Reduced::periodic("b")]
        );
        
        assert_eq!(Buchi.consistent(&ts, &sample1, sigma()), true);
        assert_eq!(Buchi.consistent(&ts2, &sample2, sigma()), false);
        assert_eq!(Buchi.consistent(&ts2, &sample3, sigma()), true);
	}
}
