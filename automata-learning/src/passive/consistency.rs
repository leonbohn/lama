use itertools::{all, Either, Itertools};
use std::iter;
use std::ops::Not;

use automata::{
    prelude::*,
    transition_system::{path::Lasso, Edge},
    Set,
};

use crate::prefixtree::prefix_tree;

use super::{Buchi, OmegaSample, Parity};

/// Used to define consistency checks on various types of omega acceptance conditions
/// required by the sprout algorithm for passively learning omega automata
pub trait ConsistencyCheck<T> {
    /// the type of the automaton to be returned
    type Aut;
    /// Checks if the given transition system is consistent with the sample
    fn consistent(&self, ts: &T, sample: &OmegaSample) -> bool;
    /// If the transition system is consistent with the sample,
    /// returns an automaton with underlying transition system ts
    /// that is consistent with the sample
    fn consistent_automaton(&self, ts: &T, sample: &OmegaSample) -> Self::Aut;
    /// Automaton that accepts precisely the positive example words
    /// in case no other solution can be found
    fn default_automaton(&self, sample: &OmegaSample) -> Self::Aut;
}

impl<T> ConsistencyCheck<T> for Buchi
where
    T: TransitionSystem<Alphabet = CharAlphabet, StateIndex = usize> + Deterministic + Pointed,
    <T as TransitionSystem>::EdgeColor: Eq + std::hash::Hash,
{
    type Aut = DBA;
    fn consistent(&self, ts: &T, sample: &OmegaSample) -> bool {
        if let Some((pos_successful, neg_successful)) = successful_runs(ts, sample) {
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
        } else {
            // bad pair was found when runnning sample words on transition system
            false
        }
    }

    fn consistent_automaton(&self, ts: &T, sample: &OmegaSample) -> Self::Aut {
        // check consistency
        assert!(self.consistent(ts, sample));

        // derive acceptance condition: accepting transitions
        // -> all transitions besides the union of negative infinity sets
        let (_, neg_successful) =
            successful_runs(ts, sample).expect("ts cannot be consistent with sample");

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

        let mut dba = aut
            .collect_pointed::<DTS<CharAlphabet, Void, bool>>()
            .0
            .into_dba();

        // complete with sink state
        dba.complete_with_colors(Void, false);
        dba
    }

    fn default_automaton(&self, sample: &OmegaSample) -> Self::Aut {
        let mut dba = prefix_tree(sample.alphabet().clone(), sample.positive_words())
            .map_edge_colors(|_| true)
            .erase_state_colors()
            .collect_dts()
            .with_initial(0)
            .into_dba();
        dba.complete_with_colors(Void, false);
        dba
    }
}

impl<T> ConsistencyCheck<T> for Parity
where
    T: TransitionSystem<Alphabet = CharAlphabet, StateIndex = usize> + Deterministic + Pointed,
    <T as TransitionSystem>::EdgeColor: Eq + std::hash::Hash,
{
    type Aut = DPA;

    fn consistent(&self, ts: &T, sample: &OmegaSample) -> bool {
        if let Some((pos_successful, neg_successful)) = successful_runs(ts, sample) {
            // convert runs to infinity sets with elements of the form (source, symbol)
            let pos_sets: Vec<Set<(usize, char)>> = pos_successful
                .iter()
                .map(|run| {
                    run.recurrent_transitions()
                        .map(|e| {
                            let (src, &sym, _, _) = e.into_tuple();
                            (src, sym)
                        })
                        .collect()
                })
                .collect();
            let neg_sets: Vec<Set<(usize, char)>> = neg_successful
                .iter()
                .map(|run| {
                    run.recurrent_transitions()
                        .map(|e| {
                            let (src, &sym, _, _) = e.into_tuple();
                            (src, sym)
                        })
                        .collect()
                })
                .collect();
            // check how set with all transitions should be handled
            let all_transitions: Set<(usize, char)> = ts
                .state_indices()
                .cartesian_product(ts.alphabet().universe())
                .collect();
            let p = pos_sets.contains(&all_transitions);
            let n = neg_sets.contains(&all_transitions);
            match (p, n) {
                (true, false) => {
                    // set of all transitions is accepting
                    has_zielonka_path(pos_sets, neg_sets, all_transitions, true)
                }
                (false, true) => {
                    // set of all transitions is non-accepting
                    has_zielonka_path(pos_sets, neg_sets, all_transitions, false)
                }
                (false, false) => {
                    // class of set of all transitions not clear, check both options
                    has_zielonka_path(
                        pos_sets.clone(),
                        neg_sets.clone(),
                        all_transitions.clone(),
                        false,
                    ) || has_zielonka_path(pos_sets, neg_sets, all_transitions, true)
                }
                (true, true) => {
                    // this shouldn't happen, pos and neg induce same infinity set
                    panic!("Set of all transitions cannot be both accepting and non-accepting")
                }
            }
        } else {
            // bad pair was found when runnning sample words on transition system
            false
        }
    }

    fn consistent_automaton(&self, ts: &T, sample: &OmegaSample) -> Self::Aut {
        todo!()
    }

    fn default_automaton(&self, sample: &OmegaSample) -> Self::Aut {
        todo!()
    }
}

/// Check if it is possible to construct a valid zielonka path from the given classified sets.
/// `class` is the classification to use for the set of all transitions.
fn has_zielonka_path(
    mut pos_sets: Vec<Set<(usize, char)>>,
    mut neg_sets: Vec<Set<(usize, char)>>,
    all_transitions: Set<(usize, char)>,
    mut class: bool,
) -> bool {
    // check if class of set with all transitions is valid
    if class {
        assert!(!neg_sets.contains(&all_transitions));
    } else {
        assert!(!pos_sets.contains(&all_transitions));
    }

    let mut z = all_transitions;
    while !z.is_empty() {
        // set new Z to union of subsets with different classification
        let z_new: Set<(usize, char)>;
        if class {
            // Z accepting
            neg_sets = neg_sets.into_iter().filter(|s| s.is_subset(&z)).collect();
            z_new = neg_sets.iter().cloned().flatten().collect();
        } else {
            // Z non-accepting
            pos_sets = pos_sets.into_iter().filter(|s| s.is_subset(&z)).collect();
            z_new = pos_sets.iter().cloned().flatten().collect();
        }
        if z == z_new {
            return false;
        }
        z = z_new;
        class = !class;
    }
    true
}

/// Run positive and negative sample words on the given transition system.
/// If there is a pair of words escaping with the same escape string from the same state, return None.
/// Otherwise return non-escaping runs of positive and negative example words
fn successful_runs<T>(
    ts: T,
    sample: &OmegaSample,
) -> Option<(
    Vec<
        Lasso<
            CharAlphabet,
            usize,
            <T as TransitionSystem>::StateColor,
            <T as TransitionSystem>::EdgeColor,
        >,
    >,
    Vec<
        Lasso<
            CharAlphabet,
            usize,
            <T as TransitionSystem>::StateColor,
            <T as TransitionSystem>::EdgeColor,
        >,
    >,
)>
where
    T: TransitionSystem<Alphabet = CharAlphabet, StateIndex = usize> + Deterministic + Pointed,
    <T as TransitionSystem>::EdgeColor: Eq + std::hash::Hash,
{
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
    for ((pos_path, w0), (neg_path, w1)) in pos_escaping.into_iter().cartesian_product(neg_escaping)
    {
        let pos_esc_str = w0.skip(pos_path.len());
        let neg_esc_str = w1.skip(neg_path.len());
        if pos_path.reached() == neg_path.reached() && pos_esc_str.equals(neg_esc_str) {
            return None;
        }
    }
    Some((pos_successful, neg_successful))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::passive::{Buchi, OmegaSample, Parity};
    use automata::{
        automaton::{DeterministicOmegaAutomaton, OmegaAcceptanceCondition},
        prelude::*,
    };

    // default alphabet
    fn sigma() -> CharAlphabet {
        alphabet!(simple 'a', 'b')
    }

    #[test]
    fn both_escaping() {
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
        assert_eq!(Parity.consistent(&ts, &sample1), true);
        // words escape from same state but with different exit strings
        assert_eq!(Buchi.consistent(&ts, &sample2), true);
        assert_eq!(Parity.consistent(&ts, &sample2), true);
        // words escape from same state with same exit string
        assert_eq!(Buchi.consistent(&ts2, &sample3), false);
        assert_eq!(Parity.consistent(&ts2, &sample3), false);
    }

    #[test]
    fn one_escaping() {
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
        assert_eq!(Parity.consistent(&ts, &sample), true);
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
        let sample = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("a", "b")], [upw!("b")]);

        // build automaton
        let dba = NTS::builder()
            .with_transitions([
                (0, 'b', false, 0),
                (0, 'a', true, 1),
                (1, 'b', true, 1),
                (1, 'a', false, 2),
                (2, 'a', false, 2),
                (2, 'b', false, 2),
            ])
            .default_color(Void)
            .deterministic()
            .with_initial(0)
            .into_dba();

        let res = Buchi.consistent_automaton(&ts, &sample);
        assert_eq!(format!("{:?}", res), format!("{:?}", dba));
    }

    #[test]
    fn parity_consistency() {
        // build transition systems
        let ts = NTS::builder()
            .with_transitions([(0, 'a', Void, 0)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);
        let ts2 = NTS::builder()
            .with_transitions([(0, 'a', Void, 0), (0, 'b', Void, 0)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);
        let ts3 = NTS::builder()
            .with_transitions([
                (0, 'a', Void, 0),
                (0, 'b', Void, 1),
                (1, 'a', Void, 0),
                (1, 'b', Void, 0),
            ])
            .default_color(Void)
            .deterministic()
            .with_initial(0);
        let ts4 = NTS::builder()
            .with_transitions([(0, 'a', Void, 0), (0, 'b', Void, 0), (0, 'c', Void, 0)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);

        // build samples
        let sample1 = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("a")], [upw!("b")]);
        let sample2 = OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("a")], [upw!("ab")]);
        let sample3 =
            OmegaSample::new_omega_from_pos_neg(sigma(), [upw!("a"), upw!("b")], [upw!("ab")]);
        let sample4 = OmegaSample::new_omega_from_pos_neg(
            sigma(),
            [upw!("ababb")],
            [upw!("ba"), upw!("bba")],
        );
        let sample5 = OmegaSample::new_omega_from_pos_neg(
            alphabet!(simple 'a', 'b', 'c'),
            [upw!("a"), upw!("b"), upw!("c")],
            [upw!("ab")],
        );
        let sample6 = OmegaSample::new_omega_from_pos_neg(
            alphabet!(simple 'a', 'b', 'c'),
            [upw!("ab"), upw!("b"), upw!("c")],
            [upw!("a")],
        );

        // assert_eq!(Parity.consistent(&ts, &sample1), true);
        // assert_eq!(Parity.consistent(&ts2, &sample2), true);
        // assert_eq!(Parity.consistent(&ts2, &sample3), false);
        // assert_eq!(Parity.consistent(&ts3, &sample4), false);
        // assert_eq!(Parity.consistent(&ts4, &sample5), false);
        assert_eq!(Parity.consistent(&ts4, &sample6), true);
    }
}
