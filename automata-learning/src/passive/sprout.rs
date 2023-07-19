use std::collections::VecDeque;

use automata::{
    ts::{operations::ProductIndex, FiniteState, Product, Sproutable, ToDot},
    Alphabet, InfiniteLength, Pointed, RightCongruence, Successor,
};
use itertools::Itertools;
use tracing::trace;

use crate::{prefixtree::prefix_tree, Sample};

use owo_colors::OwoColorize;

#[derive(Clone)]
pub struct ConflictRelation<A: Alphabet> {
    dfas: [RightCongruence<A>; 2],
    conflicts: Vec<(usize, usize)>,
}

impl<A: Alphabet> ConflictRelation<A> {
    pub fn consistent(&self, cong: &RightCongruence<A>) -> bool {
        let left = cong.product(&self.dfas[0]);
        let right = cong.product(&self.dfas[1]);

        for ProductIndex(lcong, ldfa) in left.reachable_state_indices() {
            for ProductIndex(rcong, rdfa) in right.reachable_state_indices() {
                if lcong == rcong && self.conflicts.contains(&(ldfa, rdfa)) {
                    trace!("\t\tConflict found, ({lcong}, {ldfa}) and ({rcong}, {rdfa}) reachable with ({ldfa}, {rdfa}) in conflicts");
                    return false;
                }
            }
        }
        true
    }
}

fn prefix_consistency_conflicts<A: Alphabet>(
    alphabet: &A,
    sample: Sample<A, InfiniteLength, bool>,
) -> ConflictRelation<A> {
    let left_pta = prefix_tree(alphabet.clone(), sample.positive_words().cloned().collect());
    let right_pta = prefix_tree(alphabet.clone(), sample.negative_words().cloned().collect());

    let dfa = (&left_pta).product(&right_pta);

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

    let mut conflicts = vec![];
    for ProductIndex(l, r) in dfa.state_indices() {
        let reachable = dfa
            .reachable_state_indices_from(ProductIndex(l, r))
            .collect_vec();
        if reachable
            .iter()
            .any(|ProductIndex(p, q)| states_with_infinite_run.contains(&(*p, *q)))
        {
            conflicts.push((l, r))
        }
    }

    ConflictRelation {
        dfas: [left_pta, right_pta],
        conflicts,
    }
}

pub fn omega_sprout<A: Alphabet>(
    alphabet: A,
    conflicts: ConflictRelation<A>,
) -> RightCongruence<A> {
    let mut cong = RightCongruence::new(alphabet.clone());
    let initial = cong.initial();

    // We maintain a set of missing transitions and go through them in order of creation for the states and in order
    // give by alphabet for the symbols for one state (this amouts to BFS).
    let mut queue: VecDeque<_> = alphabet.universe().map(|sym| (initial, sym)).collect();
    'outer: while let Some((source, &sym)) = queue.pop_front() {
        trace!("Trying to add transition from {} on {:?}", source, sym);
        for target in cong.state_indices() {
            cong.add_edge(source, A::expression(sym), target, ());

            if conflicts.consistent(&cong) {
                trace!("\tTransition to {} is consistent", target.green());
                continue 'outer;
            } else {
                trace!("\tTransition to {} is not consistent", target.red());
                cong.undo_add_edge();
            }
        }

        let mut new_state_label = cong.state_color(source).clone();
        new_state_label.push(sym);
        trace!(
            "No consistent transition found, adding new state [{}]",
            new_state_label.iter().map(|c| format!("{:?}", c)).join("")
        );

        let new_state = cong.add_state(new_state_label);
        if new_state > 4 {
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
        alphabet, simple,
        ts::{
            finite::{ReachedColor, ReachedState},
            FiniteState, Sproutable, ToDot,
        },
        Pointed, RightCongruence, Successor,
    };
    use itertools::Itertools;
    use tracing_test::traced_test;

    use crate::Sample;

    #[test]
    #[traced_test]
    fn prefix_consistency_sprout_two() {
        let alphabet = simple!('a', 'b');
        let sample = Sample::new_omega(
            alphabet.clone(),
            vec![
                ("b", 0, true),
                ("abab", 3, true),
                ("abbab", 4, true),
                ("ab", 1, false),
                ("a", 0, false),
            ],
        );
        let mut expected_cong = RightCongruence::new(simple!('a', 'b'));
        let q0 = expected_cong.initial();
        let q1 = expected_cong.add_state(vec!['a']);
        expected_cong.add_edge(q0, 'a', q1, ());
        expected_cong.add_edge(q1, 'a', q0, ());
        expected_cong.add_edge(q0, 'b', q0, ());
        expected_cong.add_edge(q1, 'b', q1, ());

        let conflicts = super::prefix_consistency_conflicts(&alphabet, sample);
        println!(
            "{{{}}}",
            conflicts
                .conflicts
                .iter()
                .map(|(a, b)| format!("({}, {})", a, b))
                .join(", ")
        );

        let cong = super::omega_sprout(alphabet, conflicts);

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
        let sample = Sample::new_omega(alphabet.clone(), vec![("a", 0, false), ("b", 0, true)]);
        let conflicts = super::prefix_consistency_conflicts(&alphabet, sample);
        let cong = super::omega_sprout(alphabet, conflicts);

        assert_eq!(cong.size(), 1);
        assert!(cong.contains_state_color(&vec![]));
    }

    #[test]
    #[traced_test]
    fn prefix_consistency_sprout_four() {
        let alphabet = simple!('a', 'b', 'c');
        let sample = Sample::new_omega(
            alphabet.clone(),
            vec![
                ("aac", 2, true),
                ("ab", 1, true),
                ("aab", 2, true),
                ("abaac", 4, true),
                ("abbaac", 5, true),
                ("c", 0, false),
                ("ac", 1, false),
                ("b", 0, false),
                ("abac", 3, false),
                ("abbc", 3, false),
            ],
        );

        let conflicts = super::prefix_consistency_conflicts(&alphabet, sample);
        let cong = super::omega_sprout(alphabet, conflicts);

        assert_eq!(cong.size(), 4);
        for class in ["", "a", "ab", "aa"] {
            assert!(cong.contains_state_color(&class.chars().collect()))
        }
    }
}
