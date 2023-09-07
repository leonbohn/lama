use std::{
    collections::{BTreeSet, VecDeque},
    fmt::Display,
};

use automata::{
    alphabet::Symbol,
    automaton::IsDfa,
    ts::{
        operations::{Product, ProductIndex},
        transition_system::IsPreTransition,
        Congruence, FiniteState, Sproutable, ToDot,
    },
    Alphabet, Class, InfiniteLength, Map, Pointed, RightCongruence, Set, TransitionSystem, Word,
};
use itertools::Itertools;
use tracing::trace;

use crate::{prefixtree::prefix_tree, Sample};

use owo_colors::OwoColorize;

use super::{ClassOmegaSample, OmegaSample, SplitOmegaSample};

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
        let right_reachable = right.reachable_state_indices().collect_vec();

        for ProductIndex(lcong, ldfa) in left.reachable_state_indices() {
            for ProductIndex(rcong, rdfa) in right_reachable
                .iter()
                .filter(|ProductIndex(rcong, _)| rcong == &lcong)
            {
                if lcong == *rcong && self.conflicts.contains(&(ldfa, *rdfa)) {
                    let lname = self.dfas[0].state_color(ldfa);
                    let rname = self.dfas[1].state_color(*rdfa);
                    let congname = cong.state_color(lcong);
                    trace!("\t\tConflict found, ({congname}, {lname}) and ({congname}, {rname}) reachable with ({lname}, {rname}) in conflicts");
                    return false;
                }
            }
        }
        true
    }

    /// Returns a reference to the underlying alphabet of one of the DFAs. We assure that both DFAs use the same
    /// alphabet.
    pub fn alphabet(&self) -> &A {
        self.dfas[0].alphabet()
    }

    /// Returns a preliminary threshold for the number of states in the product of the two DFAs.
    pub fn threshold(&self) -> usize {
        2 * self.dfas[0].size() * self.dfas[1].size()
    }
}

impl<A: Alphabet> ToDot for ConflictRelation<A>
where
    A::Symbol: Display,
{
    fn dot_representation(&self) -> String {
        format!("digraph A {{\n{}\n{}\n}}\n", self.header(), self.body(""),)
    }

    fn header(&self) -> String {
        [
            "compund=true".to_string(),
            "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
            "init [label=\"\", shape=none]".into(),
            "node [shape=rect]".into(),
        ]
        .join("\n")
    }

    fn body(&self, prefix: &str) -> String {
        let left = format!(
            "subgraph cluster_left {{\nlabel=\"A\"\n{}\n{}\n}}",
            self.dfas[0].header(),
            self.dfas[0].body("A"),
        );
        let right = format!(
            "subgraph cluster_right {{\nlabel=\"B\"\n{}\n{}\n}}",
            self.dfas[0].header(),
            self.dfas[0].body("B"),
        );
        let conflicts = self
            .conflicts
            .iter()
            .fold(Map::default(), |mut acc, (l, r)| {
                acc.entry(*l).or_insert(BTreeSet::new()).insert(*r);
                acc
            });
        format!(
            "label=\"Conflicts:\n{}\";\n{}\n{}\n",
            conflicts
                .into_iter()
                .map(|(l, rs)| format!(
                    "{}:{{{}}}",
                    self.dfas[0].state_color(l),
                    rs.into_iter()
                        .map(|i| self.dfas[1].state_color(i))
                        .join(",")
                ))
                .join("\\l"),
            left,
            right
        )
    }
}

/// Computes a conflict relation encoding iteration consistency. For more details on the construction,
/// see Lemma 29 in [this paper](https://arxiv.org/pdf/2302.11043.pdf).
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

    let looping_words = samples.cong().looping_words(&class);

    let left_pta = prefix_tree(sample.alphabet.clone(), periodic_sample.positive())
        .map_colors(|mr| !mr.is_empty() && periodic_sample.classify(mr.omega_power()) == Some(true))
        .intersection(&looping_words);

    let right_pta = prefix_tree(sample.alphabet.clone(), periodic_sample.negative())
        .map_colors(|mr| {
            !mr.is_empty() && periodic_sample.classify(mr.omega_power()) == Some(false)
        })
        .intersection(&looping_words);

    let mut conflicts = Set::default();
    let mut queue = VecDeque::from_iter(
        left_pta
            .accepting_states()
            .into_iter()
            .cartesian_product(right_pta.accepting_states()),
    );

    let mut left_cache: std::collections::HashMap<_, _> = std::collections::HashMap::default();
    let mut right_cache: std::collections::HashMap<_, _> = std::collections::HashMap::default();

    while let Some((left, right)) = queue.pop_front() {
        if !conflicts.insert((left, right)) {
            continue;
        }
        let left_pred = left_cache
            .entry(left)
            .or_insert_with(|| left_pta.predecessors(left).unwrap().collect_vec());
        let right_pred = right_cache
            .entry(right)
            .or_insert_with(|| right_pta.predecessors(right).unwrap().collect_vec());

        for l in left_pred {
            for r in right_pred
                .iter()
                .filter(|o| o.expression() == l.expression())
            {
                queue.push_back((l.source(), r.source()));
            }
        }
    }

    let (left, left_map) = left_pta.build_right_congruence();
    debug_assert!(left.size() == left_pta.size());
    let (right, right_map) = right_pta.build_right_congruence();
    debug_assert!(right.size() == right_pta.size());

    ConflictRelation {
        dfas: [left, right],
        conflicts: conflicts
            .into_iter()
            .map(|(l, r)| (left_map[&l], right_map[&r]))
            .collect(),
    }
}

/// Computes a conflict relation encoding prefix consistency. For more details on how this works, see
/// Lemma 28 in [this paper](https://arxiv.org/pdf/2302.11043.pdf).
pub fn prefix_consistency_conflicts<
    A: Alphabet,
    S: std::borrow::Borrow<Sample<A, InfiniteLength, bool>>,
>(
    sample: S,
) -> ConflictRelation<A> {
    let sample = sample.borrow();
    let left_pta = prefix_tree(sample.alphabet.clone(), sample.positive_words());
    let right_pta = prefix_tree(sample.alphabet.clone(), sample.negative_words());

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

    let mut conflicts = Set::default();
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

/// We allow additional constraints to be added to the omega-sprout algorithm. This is useful for example to ensure
/// that the learned automaton separates idempotents.
pub trait AdditionalConstraint<A: Alphabet> {
    /// This is the main function that is called to ensure that the constraint is satisfied.
    fn satisfied(&self, cong: &RightCongruence<A>) -> bool;
}

impl<A: Alphabet> AdditionalConstraint<A> for () {
    fn satisfied(&self, cong: &RightCongruence<A>) -> bool {
        true
    }
}

/// This constraint ensures that the learned automaton separates idempotents.
#[derive(Clone, Debug)]
pub struct SeparatesIdempotents<'a, A: Alphabet> {
    sample: &'a ClassOmegaSample<'a, A, bool>,
}

impl<'a, A: Alphabet> SeparatesIdempotents<'a, A> {
    /// Creates a new instance of the constraint.
    pub fn new(sample: &'a ClassOmegaSample<'a, A, bool>) -> Self {
        Self { sample }
    }
}

impl<'a, A: Alphabet> AdditionalConstraint<A> for SeparatesIdempotents<'a, A> {
    fn satisfied(&self, cong: &RightCongruence<A>) -> bool {
        true
    }
}

/// Runs the omega-sprout algorithm on a given conflict relation.
pub fn omega_sprout_conflicts<A, F>(
    conflicts: ConflictRelation<A>,
    additional_constraint: F,
    allow_transitions_into_epsilon: bool,
) -> RightCongruence<A>
where
    A: Alphabet,
    F: AdditionalConstraint<A>,
{
    let mut cong = RightCongruence::new(conflicts.alphabet().clone());
    let initial = cong.initial();
    let threshold = conflicts.threshold();

    // We maintain a set of missing transitions and go through them in order of creation for the states and in order
    // give by alphabet for the symbols for one state (this amouts to BFS).
    let mut queue: VecDeque<_> = conflicts
        .alphabet()
        .universe()
        .map(|sym| (initial, sym))
        .collect();
    'outer: while let Some((source, &sym)) = queue.pop_front() {
        trace!(
            "Trying to add transition from {} on {}",
            cong.state_color(source).blue(),
            sym.show().blue()
        );

        // FIXME: This is a hack to avoid lifetime issues, find a better way...
        // TODO: figure out if this is the best way, we just take the upper bound on the number and assume that all states have sequential ids...
        for target in (0..cong.size()) {
            if !allow_transitions_into_epsilon && target == initial {
                continue;
            }
            let old_edge = cong.add_edge(source, A::expression(sym), target, ());

            if conflicts.consistent(&cong) && additional_constraint.satisfied(&cong) {
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
                if let Some((old_target, _)) = old_edge {
                    cong.remove_edge(source, A::expression(sym));
                }
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
        queue.extend(std::iter::repeat(new_state).zip(conflicts.alphabet().universe()))
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
        Class, Pointed, RightCongruence, TransitionSystem,
    };
    use itertools::Itertools;
    use tracing_test::traced_test;

    use crate::{passive::OmegaSample, Sample};

    fn testing_larger_forc_sample() -> (Simple, OmegaSample<Simple, bool>) {
        let Ok(sample) = OmegaSample::try_from(
            r#"omega
        alphabet: a,b
        positive:
        bbabab
        ab
        baa
        abbab
        babab
        babba
        bbaba
        babab
        babba
        aba
        aab
        abaabb
        ababb
        a
        abab
        baba
        ba
        bbaba
        abbab
        babbba
        abbab
        abbaab
        babbbba
        negative:
        bba
        abba
        baab
        bbba
        abb
        abbba
        bab
        bba
        babb
        bbab
        b
        bb
        abba
        bbaab
        abbb
        bbaa
        abbaa
        babbab
        bbabba
        babbb
        bbabb
        "#,
        ) else {
            panic!("Cannot parse sample");
        };
        (sample.alphabet.clone(), sample)
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
    #[ignore]
    fn display_conflict_relation() {
        let (alphabet, sample) = testing_larger_forc_sample();
        let conflicts = super::prefix_consistency_conflicts(sample);
        conflicts.display_rendered();
    }

    #[test]
    #[ignore]
    fn learn_small_forc() {
        let (alphabet, sample) = testing_smaller_forc_smaple();
        let cong = sample.infer_right_congruence();
        assert_eq!(cong.size(), 1);

        let split_sample = sample.split(&cong);
        let eps = Class::epsilon();
        let eps_sample = split_sample.get(&eps).unwrap();

        let conflicts = super::iteration_consistency_conflicts(&split_sample, eps);
        conflicts.dfas[0].display_rendered();
        conflicts.dfas[1].display_rendered();
        println!(
            "{}",
            conflicts
                .conflicts
                .iter()
                .map(|(l, r)| format!("({l},{r})"))
                .join(", ")
        );
        let prc_eps = super::omega_sprout_conflicts(conflicts, (), false);
        prc_eps.display_rendered();
    }

    #[test]
    fn learn_larger_forc() {
        let (alphabet, sample) = testing_larger_forc_sample();

        let forc = sample.infer_forc();
        let prc_eps = forc.prc(Class::epsilon()).unwrap();
        println!("{}", prc_eps.dot_representation());
        assert_eq!(prc_eps.size(), 13);
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

        let conflicts = super::prefix_consistency_conflicts(sample);

        let cong = super::omega_sprout_conflicts(conflicts, (), true);

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
        let conflicts = super::prefix_consistency_conflicts(sample);
        let cong = super::omega_sprout_conflicts(conflicts, (), true);

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

        let conflicts = super::prefix_consistency_conflicts(sample);
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

        let cong = super::omega_sprout_conflicts(conflicts, (), true);

        assert_eq!(cong.size(), 4);
        for class in ["", "a", "ab", "aa"] {
            assert!(cong.contains_state_color(&class.into()))
        }
    }
}
