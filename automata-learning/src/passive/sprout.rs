use std::collections::VecDeque;

use automata::{
    alphabet::Symbol,
    automaton::IsDfa,
    ts::{
        operations::ProductIndex, Congruence, FiniteState, Product, Sproutable, ToDot,
        TransitionSystem,
    },
    Alphabet, Class, InfiniteLength, Pointed, RightCongruence, Set, Successor, Word,
};
use itertools::Itertools;
use tracing::trace;

use crate::{prefixtree::prefix_tree, Sample};

use owo_colors::OwoColorize;

use super::{OmegaSample, SplitOmegaSample};

/// Stores two DFAs and a set of conflicts between them.
#[derive(Clone)]
pub struct ConflictRelation<A: Alphabet> {
    dfas: [RightCongruence<A>; 2],
    conflicts: Set<(usize, usize)>,
}

impl<A: Alphabet> ConflictRelation<A> {
    /// Verifies that a given congruence is consistent with the conflicts.
    pub fn consistent(&self, cong: &RightCongruence<A>) -> bool {
        let left = cong.ts_product(&self.dfas[0]);
        let right = cong.ts_product(&self.dfas[1]);

        for ProductIndex(lcong, ldfa) in left.reachable_state_indices() {
            for ProductIndex(rcong, rdfa) in right.reachable_state_indices() {
                if lcong == rcong && self.conflicts.contains(&(ldfa, rdfa)) {
                    let lname = self.dfas[0].state_color(ldfa);
                    let rname = self.dfas[1].state_color(rdfa);
                    let congname = cong.state_color(lcong);
                    trace!("\t\tConflict found, ({congname}, {lname}) and ({congname}, {rname}) reachable with ({lname}, {rname}) in conflicts");
                    return false;
                }
            }
        }
        true
    }

    pub fn threshold(&self) -> usize {
        self.dfas[0].size() * self.dfas[1].size()
    }
}

pub fn iteration_consistency_conflicts<A: Alphabet>(
    samples: &SplitOmegaSample<'_, A, bool>,
    class: Class<A::Symbol>,
) -> ConflictRelation<A> {
    let Some(sample) = samples.get(&class) else {
        panic!("Sample for class {:?} does not exist!", class)
    };
    let periodic_sample = sample.to_periodic_sample();
    trace!(
        "Positive periodic words: {}\nNegative periodic words: {}",
        periodic_sample
            .positive()
            .map(|w| format!("{:?}", w))
            .join(","),
        periodic_sample
            .negative()
            .map(|w| format!("{:?}", w))
            .join(",")
    );

    let looping_words = sample.right_congruence().looping_words(&class);

    let left_pta = prefix_tree(sample.alphabet.clone(), periodic_sample.positive())
        .map_colors(|mr| !mr.is_empty() && periodic_sample.classify(mr.omega_power()) == Some(true))
        .intersection(&looping_words);

    let right_pta = prefix_tree(sample.alphabet.clone(), periodic_sample.negative())
        .map_colors(|mr| {
            !mr.is_empty() && periodic_sample.classify(mr.omega_power()) == Some(false)
        })
        .intersection(&looping_words);

    let mut conflicts = Set::new();
    let mut queue = VecDeque::from_iter(
        left_pta
            .accepting_states()
            .into_iter()
            .cartesian_product(right_pta.accepting_states()),
    );
    while let Some((left, right)) = queue.pop_front() {
        if !conflicts.insert((left, right)) {
            continue;
        }
        for (left_predecessor, left_expression, _) in left_pta.predecessors(left) {
            for (right_predecessor, right_expression, _) in right_pta.predecessors(right) {
                if left_expression == right_expression {
                    queue.push_back((left_predecessor, right_predecessor));
                }
            }
        }
    }

    let (left, left_map) = left_pta.build_right_congruence();
    println!(
        "left size old {}, left size new {}",
        left.size(),
        left_pta.size()
    );
    debug_assert!(left.size() == left_pta.size());
    let (right, right_map) = right_pta.build_right_congruence();

    ConflictRelation {
        dfas: [left, right],
        conflicts: conflicts
            .into_iter()
            .map(|(l, r)| (left_map[&l], right_map[&r]))
            .collect(),
    }
}

/// Computes a conflict relation encoding prefix consistency.
pub fn prefix_consistency_conflicts<
    A: Alphabet,
    S: std::borrow::Borrow<Sample<A, InfiniteLength, bool>>,
>(
    alphabet: &A,
    sample: S,
) -> ConflictRelation<A> {
    let sample = sample.borrow();
    let left_pta = prefix_tree(alphabet.clone(), sample.positive_words());
    let right_pta = prefix_tree(alphabet.clone(), sample.negative_words());

    let dfa = (&left_pta).ts_product(&right_pta);

    let sccs = dfa.sccs();
    let states_with_infinite_run: Vec<(usize, usize)> = sccs
        .iter()
        .filter_map(|scc| {
            if !scc.is_trivial() {
                Some(scc.clone().into_iter().map(Into::into))
            } else {
                None
            }
        })
        .flatten()
        .collect();

    let mut conflicts = Set::new();
    for ProductIndex(l, r) in dfa.state_indices() {
        let reachable = dfa
            .reachable_state_indices_from(ProductIndex(l, r))
            .collect_vec();
        if reachable
            .iter()
            .any(|ProductIndex(p, q)| states_with_infinite_run.contains(&(*p, *q)))
        {
            conflicts.insert((l, r));
        }
    }

    ConflictRelation {
        dfas: [left_pta, right_pta],
        conflicts,
    }
}

/// Runs the omega-sprout algorithm on a given conflict relation.
pub fn omega_sprout_conflicts<A: Alphabet>(
    alphabet: A,
    conflicts: ConflictRelation<A>,
    allow_transitions_into_epsilon: bool,
) -> RightCongruence<A> {
    let mut cong = RightCongruence::new(alphabet.clone());
    let initial = cong.initial();
    let threshold = conflicts.threshold();

    // We maintain a set of missing transitions and go through them in order of creation for the states and in order
    // give by alphabet for the symbols for one state (this amouts to BFS).
    let mut queue: VecDeque<_> = alphabet.universe().map(|sym| (initial, sym)).collect();
    'outer: while let Some((source, &sym)) = queue.pop_front() {
        trace!(
            "Trying to add transition from {} on {}",
            cong.state_color(source).blue(),
            sym.show().blue()
        );
        for target in cong.state_indices() {
            if !allow_transitions_into_epsilon && target == initial {
                continue;
            }
            cong.add_edge(source, A::expression(sym), target, ());

            if conflicts.consistent(&cong) {
                trace!(
                    "\tTransition {}--{}-->{} is consistent",
                    cong.state_color(source).green(),
                    sym.show(),
                    cong.state_color(target).green()
                );
                continue 'outer;
            } else {
                trace!(
                    "\tTransition {}--{}-->{} is not consistent",
                    cong.state_color(source).red(),
                    sym.show(),
                    cong.state_color(target).red()
                );
                cong.undo_add_edge();
            }
        }

        let mut new_state_label = cong.state_color(source).clone();
        new_state_label.push(sym);
        trace!(
            "No consistent transition found, adding new state [{}]",
            new_state_label
                .iter()
                .map(|c| format!("{:?}", c))
                .join("")
                .blue()
        );

        let new_state = cong.add_state(new_state_label);
        if new_state > threshold {
            panic!("TOO MANY STATES")
        }
        cong.add_edge(source, A::expression(sym), new_state, ());
        queue.extend(std::iter::repeat(new_state).zip(alphabet.universe()))
    }

    cong
}

#[cfg(test)]
mod tests {
    use automata::{
        alphabet::{self, Simple},
        nupw, simple,
        ts::{
            finite::{ReachedColor, ReachedState},
            FiniteState, Sproutable, ToDot,
        },
        Class, Pointed, RightCongruence, Successor,
    };
    use itertools::Itertools;
    use tracing_test::traced_test;

    use crate::{passive::OmegaSample, Sample};

    fn testing_larger_forc_sample() -> (Simple, OmegaSample<Simple, bool>) {
        let alphabet = simple!('a', 'b', 'c');
        (
            alphabet.clone(),
            Sample::new_omega_from_pos_neg(
                alphabet,
                [
                    nupw!("b"),
                    nupw!("cbb"),
                    nupw!("bbc"),
                    nupw!("a", "c"),
                    nupw!("aa", "c"),
                    nupw!("ab", "a"),
                    nupw!("abb", "a"),
                    nupw!("", "abc"),
                ],
                [nupw!("c"), nupw!("a"), nupw!("a", "b"), nupw!("cb")],
            ),
        )
    }

    fn testing_smaller_forc_smaple() -> (Simple, OmegaSample<Simple, bool>) {
        let alphabet = simple!('a', 'b', 'c');
        (
            alphabet.clone(),
            Sample::new_omega_from_pos_neg(
                alphabet,
                [
                    nupw!("a"),
                    nupw!("baa"),
                    nupw!("aca"),
                    nupw!("caab"),
                    nupw!("abca"),
                ],
                [
                    nupw!("b"),
                    nupw!("c"),
                    nupw!("ab"),
                    nupw!("ac"),
                    nupw!("abc"),
                ],
            ),
        )
    }

    #[test]
    #[traced_test]
    fn learn_small_forc() {
        let (alphabet, sample) = testing_smaller_forc_smaple();
        let cong = sample.right_congruence();
        assert_eq!(cong.size(), 1);

        let split_sample = sample.split(&cong);
        let eps = Class::epsilon();
        let eps_sample = split_sample.get(&eps).unwrap();

        let conflicts = super::iteration_consistency_conflicts(&split_sample, eps);
        conflicts.dfas[0].display_rendered(Some(conflicts.dfas[0].initial()));
        conflicts.dfas[1].display_rendered(Some(conflicts.dfas[1].initial()));
        println!(
            "{}",
            conflicts
                .conflicts
                .iter()
                .map(|(l, r)| format!("({l},{r})"))
                .join(", ")
        );
        let prc_eps = super::omega_sprout_conflicts(alphabet, conflicts, false);
        prc_eps.display_rendered(Some(prc_eps.initial()));
    }

    #[test]
    fn learn_larger_forc() {
        let (alphabet, sample) = testing_larger_forc_sample();

        let cong = sample.right_congruence();
        assert_eq!(cong.size(), 3);
        assert!(cong.can_separate(&"a", &""));
        assert!(cong.can_separate(&"", &"ab"));
        assert!(cong.can_separate(&"a", &"ab"));
        cong.display_rendered(Some(cong.initial()));

        let class_sample = sample.split(&cong);
        let eps = Class::epsilon();
        let epsilon_sample = class_sample.get(&eps).unwrap();

        println!("{:?}", epsilon_sample);
        let conflicts = super::iteration_consistency_conflicts(&class_sample, eps);
        let prc_eps = super::omega_sprout_conflicts(alphabet, conflicts, false);
        prc_eps.display_rendered(Some(prc_eps.initial()));
    }

    #[test]
    fn prefix_consistency_sprout_two() {
        let alphabet = simple!('a', 'b');
        let sample = Sample::new_omega(
            alphabet.clone(),
            vec![
                (("b", 0), true),
                (("abab", 3), true),
                (("abbab", 4), true),
                (("ab", 1), false),
                (("a", 0), false),
            ],
        );
        let mut expected_cong = RightCongruence::new(simple!('a', 'b'));
        let q0 = expected_cong.initial();
        let q1 = expected_cong.add_state(vec!['a'].into());
        expected_cong.add_edge(q0, 'a', q1, ());
        expected_cong.add_edge(q1, 'a', q0, ());
        expected_cong.add_edge(q0, 'b', q0, ());
        expected_cong.add_edge(q1, 'b', q1, ());

        let conflicts = super::prefix_consistency_conflicts(&alphabet, sample);

        let cong = super::omega_sprout_conflicts(alphabet, conflicts, true);

        assert_eq!(cong.size(), expected_cong.size());
        for word in ["aba", "abbabb", "baabaaba", "bababaaba", "b", "a", ""] {
            assert_eq!(
                cong.reached_color(&word),
                expected_cong.reached_color(&word)
            )
        }
    }

    #[test]
    fn prefix_consistency_sprout_one() {
        let alphabet = simple!('a', 'b');
        let sample = Sample::new_omega(alphabet.clone(), vec![(("a", 0), false), (("b", 0), true)]);
        let conflicts = super::prefix_consistency_conflicts(&alphabet, sample);
        let cong = super::omega_sprout_conflicts(alphabet, conflicts, true);

        assert_eq!(cong.size(), 1);
        assert!(cong.contains_state_color(&vec![].into()));
    }

    #[test]
    fn prefix_consistency_sprout_four() {
        let alphabet = simple!('a', 'b', 'c');
        let sample = Sample::new_omega(
            alphabet.clone(),
            vec![
                (("aac", 2), true),
                (("ab", 1), true),
                (("aab", 2), true),
                (("abaac", 4), true),
                (("abbaac", 5), true),
                (("abc", 2), false),
                (("c", 0), false),
                (("ac", 1), false),
                (("b", 0), false),
                (("abac", 3), false),
                (("abbc", 3), false),
            ],
        );

        let conflicts = super::prefix_consistency_conflicts(&alphabet, sample);
        println!(
            "{{{}}}",
            conflicts
                .conflicts
                .iter()
                .map(|(a, b)| format!(
                    "({:?}, {:?})",
                    conflicts.dfas[0].state_color(*a),
                    conflicts.dfas[1].state_color(*b)
                ))
                .join(", ")
        );

        let cong = super::omega_sprout_conflicts(alphabet, conflicts, true);

        assert_eq!(cong.size(), 4);
        for class in ["", "a", "ab", "aa"] {
            assert!(cong.contains_state_color(&class.into()))
        }
    }
}
