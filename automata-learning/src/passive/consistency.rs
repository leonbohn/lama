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
pub trait ConsistencyCheck<T: Deterministic> {
    /// Checks if the given transition system is consistent with the sample
    fn consistent(&self, ts: &T, sample: &OmegaSample) -> bool;
    /// Returns an automaton with underlying transition system ts
    /// that is consistent with the sample
    fn consistent_automaton(&self, ts: &T, sample: &OmegaSample) -> DeterministicOmegaAutomaton<alphabet::Simple>;
}

impl<T: Deterministic> ConsistencyCheck<T> for Buchi {
    fn consistent(&self, ts: &T, sample: &OmegaSample) -> bool {
        // for pairs of positive and negative words
        // collect infinity sets for non escaping words
        // if a pair escapes from same state with same escape string, false
        // check if infinity sets valid: see paper
        todo!()
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
            .deterministic();
        let ts2 = NTS::builder()
            .with_transitions([
                (0, 'a', (), 0)
            ])
            .default_color(())
            .deterministic();
        
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
        assert_eq!(Buchi.consistent(&ts, &sample1), true);
        // words escape from same state but with different exit strings
        assert_eq!(Buchi.consistent(&ts, &sample2), true);
        // words escape from same state with same exit string
        assert_eq!(Buchi.consistent(&ts2, &sample3), false);
	}

    #[test]
	fn buchi_one_escaping() {
        // build transition system
        let ts = NTS::builder()
            .with_transitions([
                (0, 'a', (), 0)
            ])
            .default_color(())
            .deterministic();
        
        // build sample
        let sample = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [Reduced::periodic("a")],
            [Reduced::periodic("b")]
        );
        
        // one word is escaping, the other is not
        assert_eq!(Buchi.consistent(&ts, &sample), true);
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
            .deterministic();
        let ts2 = NTS::builder()
            .with_transitions([
                (0, 'b', (), 0),
                (0, 'a', (), 1),
                (1, 'a', (), 0)
            ])
            .default_color(())
            .deterministic();
        
        // build samples
        let sample1 = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [Reduced::ultimately_periodic("a", "b")],
            [Reduced::periodic("b")]
        );
        let sample2 = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [Reduced::periodic("b")],
            [Reduced::periodic("aba")]
        );
        let sample3 = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [Reduced::periodic("aba")],
            [Reduced::periodic("b")]
        );
        
        assert_eq!(Buchi.consistent(&ts, &sample1), true);
        assert_eq!(Buchi.consistent(&ts2, &sample2), false);
        assert_eq!(Buchi.consistent(&ts2, &sample3), false);
	}
}
