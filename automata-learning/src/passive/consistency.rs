use itertools::{Either, Itertools};
use std::iter;
use std::ops::Not;

use automata::{automaton::Buchi, prelude::*, ts::path::Edge, Set};

use super::OmegaSample;

/// Used to define consistency checks on various types of omega acceptance conditions
/// required by the sprout algorithm for passively learning omega automata
pub trait ConsistencyCheck<T: Deterministic> {
    type Aut;
    /// Checks if the given transition system is consistent with the sample
    fn consistent(&self, ts: &T, sample: &OmegaSample) -> bool;
    /// If the transition system is consistent with the sample,
    /// returns an automaton with underlying transition system ts
    /// that is consistent with the sample
    fn consistent_automaton(&self, ts: &T, sample: &OmegaSample) -> Self::Aut;
}

impl<T> ConsistencyCheck<T> for Buchi
where
    T: TransitionSystem<Alphabet = CharAlphabet, StateIndex = usize> + Deterministic + Pointed,
    <T as TransitionSystem>::EdgeColor: Eq + std::hash::Hash,
{
    type Aut = DBA;
    fn consistent(&self, ts: &T, sample: &OmegaSample) -> bool {
        // run transition system on sample words and
        // separate in escaping and non-escaping (successful) runs
        let (pos_successful, pos_escaping): (Vec<_>, Vec<_>) = sample
            .positive_words()
            .map(|w| (ts.omega_run(w), w))
            .partition_map(|r| match r {
                (Ok(v), _) => Either::Left(v),
                (Err(v), w) => Either::Right((v, w)),
            });
        let (neg_successful, neg_escaping): (Vec<_>, Vec<_>) = sample
            .negative_words()
            .map(|w| (ts.omega_run(w), w))
            .partition_map(|r| match r {
                (Ok(v), _) => Either::Left(v),
                (Err(v), w) => Either::Right((v, w)),
            });

        // reject if a pair escaping from the same state with the same escape string is found
        for ((pos_path, w0), (neg_path, w1)) in
            pos_escaping.into_iter().cartesian_product(neg_escaping)
        {
            let pos_esc_str = w0.offset(pos_path.len());
            let neg_esc_str = w1.offset(neg_path.len());
            if pos_path.reached() == neg_path.reached() && pos_esc_str.equals(neg_esc_str) {
                return false;
            }
        }

        // check if the infinity set of a positive word is subset of
        // the union of all infinity sets of negative words (see paper for details)
        let neg_union: Set<_> = neg_successful
            .into_iter()
            .map(|r| r.into_recurrent_transitions())
            .flatten()
            .collect();

        pos_successful
            .into_iter()
            .map(|r| r.into_recurrent_transitions().collect::<Set<_>>())
            .any(|s| s.is_subset(&neg_union))
            .not()
    }

    fn consistent_automaton(&self, ts: &T, sample: &OmegaSample) -> Self::Aut {
        // check consistency
        assert!(self.consistent(ts, sample));

        // derive acceptance condition: accepting transitions
        // -> all transitions besides the union of negative infinity sets
        let (neg_successful, neg_escaping): (Vec<_>, Vec<_>) = sample
            .negative_words()
            .map(|w| ts.omega_run(w))
            .partition_map(|r| match r {
                Ok(v) => Either::Left(v),
                Err(v) => Either::Right(v),
            });
        let neg_union: Set<_> = neg_successful
            .into_iter()
            .map(|r| r.into_recurrent_transitions())
            .flatten()
            .collect();

        let all_transitions: Set<_> = ts
            .transitions()
            .map(|t| t.into_tuple())
            .map(|(a, b, c, d)| Edge::new(a, *b, d, c))
            .collect();

        let accepting: Set<_> = all_transitions.difference(&neg_union).collect();

        // make DBA
        let mut aut = ts
            .map_edge_colors_full(move |a, b, c, d| accepting.contains(&Edge::new(a, *b, c, d)))
            .erase_state_colors();

        aut.collect_pointed::<DTS<CharAlphabet, Void, bool>>()
            .0
            .into_dba()
        // send missing transitions to initial state
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::passive::OmegaSample;
    use automata::{
        automaton::{Buchi, DeterministicOmegaAutomaton, OmegaAcceptanceCondition},
        prelude::*,
    };

    // default alphabet
    fn sigma() -> CharAlphabet {
        alphabet!(simple 'a', 'b')
    }

    #[test]
    fn buchi_both_escaping() {
        // build transition systems
        let ts = NTS::builder()
            .with_transitions([(0, 'a', Void, 1)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);
        let ts2 = NTS::builder()
            .with_transitions([(0, 'a', Void, 0)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);

        // build samples
        let sample1 = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("a")], [upw!("b")]);
        let sample2 = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("a")], [upw!("a", "b")]);
        let sample3 = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("a", "b")], [upw!("b")]);

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
            .with_transitions([(0, 'a', Void, 0)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);

        // build sample
        let sample = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("a")], [upw!("b")]);

        // one word is escaping, the other is not
        assert_eq!(Buchi.consistent(&ts, &sample), true);
    }

    #[test]
    fn buchi_consistency() {
        // build transition systems
        let ts = NTS::builder()
            .with_transitions([(0, 'b', Void, 0), (0, 'a', Void, 1), (1, 'b', Void, 1)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);
        let ts2 = NTS::builder()
            .with_transitions([(0, 'b', Void, 0), (0, 'a', Void, 1), (1, 'a', Void, 0)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);
        let ts3 = NTS::builder()
            .with_transitions([(0, 'a', Void, 0), (0, 'b', Void, 0)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);

        // build samples
        let sample1 = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("a", "b")], [upw!("b")]);
        let sample2 = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("b")], [upw!("aab")]);
        let sample3 = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("aab")], [upw!("b")]);
        let sample4 = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("a")], [upw!("b")]);

        assert_eq!(Buchi.consistent(&ts, &sample1), true);
        assert_eq!(Buchi.consistent(&ts2, &sample2), false);
        assert_eq!(Buchi.consistent(&ts2, &sample3), true);
        assert_eq!(Buchi.consistent(&ts3, &sample4), true);
    }

    #[test]
    fn buchi_consistent_automaton() {
        // build transition system
        let ts = NTS::builder()
            .with_transitions([(0, 'b', Void, 0), (0, 'a', Void, 1), (1, 'b', Void, 1)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);

        // build sample
        let sample1 = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("a", "b")], [upw!("b")]);

        // build automaton
        let dba = NTS::builder()
            .with_transitions([(0, 'b', false, 0), (0, 'a', true, 1), (1, 'b', true, 1)])
            .default_color(Void)
            .deterministic()
            .with_initial(0)
            .into_dba();

        assert!(Buchi.consistent_automaton(&ts, &sample1).eq(&dba));
    }
}
