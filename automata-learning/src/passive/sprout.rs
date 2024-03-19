use std::{
    collections::{BTreeSet, VecDeque},
    fmt::Display,
};

use automata::{prelude::*, transition_system::operations::ProductIndex, Map, Set};
use itertools::Itertools;
use tracing::trace;

use crate::{
    passive::{ClassOmegaSample, FiniteSample, OmegaSample, Sample, SplitOmegaSample},
    prefixtree::prefix_tree,
};

use owo_colors::OwoColorize;

/// Represents a consistency check that can be performed on a congruence. This is used in the
/// omega-sprout algorithm to ensure in each iteration, that the produced congruence relation
/// is consistent with the given constraints. The constraints can either be given by a conflict
/// relation, by a list or they could be verified against a sample (or other form of data).
#[impl_tools::autoimpl(for<T: trait> &T)]
pub trait ConsistencyCheck<A: Alphabet> {
    /// Verifies that `cong` is consistent with the constraint.
    fn consistent(&self, cong: &RightCongruence<A>) -> bool;
    /// Returns an approximate threshold for the number of classes in the congruence. This is
    /// useful for algorithms to detect infinite loops.
    fn threshold(&self) -> usize;
    /// Returns a reference to the alphabet used by the constraint.
    fn alphabet(&self) -> &A;
}

impl<A: Alphabet> ConsistencyCheck<A> for FiniteSample<A, bool> {
    fn consistent(&self, cong: &RightCongruence<A>) -> bool {
        let positive_indices: Set<_> = self
            .positive_words()
            .filter_map(|w| cong.reached_state_index(w))
            .collect();
        let negative_indices: Set<_> = self
            .negative_words()
            .filter_map(|w| cong.reached_state_index(w))
            .collect();
        positive_indices.is_disjoint(&negative_indices)
    }

    fn threshold(&self) -> usize {
        self.max_word_len() * 2
    }

    fn alphabet(&self) -> &A {
        &self.alphabet
    }
}

/// Stores two DFAs and a set of conflicts between them.
#[derive(Clone)]
pub struct ConflictRelation<A: Alphabet> {
    dfas: [RightCongruence<A>; 2],
    conflicts: Set<(usize, usize)>,
}

impl<A: Alphabet> ConsistencyCheck<A> for ConflictRelation<A> {
    fn alphabet(&self) -> &A {
        self.dfas[0].alphabet()
    }
    /// Verifies that a given congruence is consistent with the conflicts.
    fn consistent(&self, cong: &RightCongruence<A>) -> bool {
        let left = cong.ts_product(&self.dfas[0]);
        let right = cong.ts_product(&self.dfas[1]);
        let right_reachable = right.reachable_state_indices().collect_vec();

        for ProductIndex(lcong, ldfa) in left.reachable_state_indices() {
            for ProductIndex(rcong, rdfa) in right_reachable
                .iter()
                .filter(|ProductIndex(rcong, _)| rcong == &lcong)
            {
                if lcong == *rcong && self.conflicts.contains(&(ldfa, *rdfa)) {
                    let lname = self.dfas[0]
                        .state_color(ldfa)
                        .expect("Every state must have a color!");
                    let rname = self.dfas[1]
                        .state_color(*rdfa)
                        .expect("Every state must have a color!");
                    let congname = cong
                        .state_color(lcong)
                        .expect("Every state must have a color!");
                    trace!("\t\tConflict found, ({congname}, {lname}) and ({congname}, {rname}) reachable with ({lname}, {rname}) in conflicts");
                    return false;
                }
            }
        }
        true
    }

    /// Returns a preliminary threshold for the number of states in the product of the two DFAs.
    fn threshold(&self) -> usize {
        2 * self.dfas[0].size() * self.dfas[1].size()
    }
}

impl<A: Alphabet> std::fmt::Debug for ConflictRelation<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\t\t{}\n{:?}", "first DFA".blue(), self.dfas[0])?;
        write!(f, "\t\t{}\n{:?}", "SECOND DFA".cyan(), self.dfas[1])?;
        write!(
            f,
            "Conflicts\n{}",
            self.conflicts
                .iter()
                .map(|(c, d)| format!("({c} | {d})"))
                .join(", ")
        )
    }
}

impl<A: Alphabet> ConflictRelation<A> {
    /// Returns a reference to the underlying alphabet of one of the DFAs. We assure that both DFAs use the same
    /// alphabet.
    pub fn alphabet(&self) -> &A {
        self.dfas[0].alphabet()
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
        .map_state_colors(|mr| {
            !mr.class().is_empty()
                && periodic_sample.classify(mr.class().omega_power()) == Some(true)
        })
        .intersection(&looping_words)
        .collect_dfa();

    let right_pta = prefix_tree(sample.alphabet.clone(), periodic_sample.negative())
        .map_state_colors(|mr| {
            !mr.class().is_empty()
                && periodic_sample.classify(mr.class().omega_power()) == Some(false)
        })
        .intersection(&looping_words)
        .collect_dfa();

    let mut conflicts = Set::default();
    let mut queue = VecDeque::from_iter(
        left_pta
            .accepting_states()
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
pub fn prefix_consistency_conflicts<A: Alphabet, S: std::borrow::Borrow<OmegaSample<A, bool>>>(
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
            if !scc.is_transient() {
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

impl<A: Alphabet> ConsistencyCheck<A> for () {
    fn consistent(&self, cong: &RightCongruence<A>) -> bool {
        true
    }

    fn threshold(&self) -> usize {
        0
    }

    fn alphabet(&self) -> &A {
        unimplemented!("This does not make sense, you should not call this function directly")
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

impl<'a, A: Alphabet> ConsistencyCheck<A> for SeparatesIdempotents<'a, A> {
    fn consistent(&self, cong: &RightCongruence<A>) -> bool {
        true
    }

    fn threshold(&self) -> usize {
        todo!()
    }

    fn alphabet(&self) -> &A {
        todo!()
    }
}

/// Runs the omega-sprout algorithm on a given conflict relation.
pub fn sprout<A, C>(
    conflicts: C,
    additional_constraints: Vec<Box<dyn ConsistencyCheck<A>>>,
    allow_transitions_into_epsilon: bool,
) -> RightCongruence<A>
where
    A: Alphabet,
    C: ConsistencyCheck<A>,
{
    let mut cong = RightCongruence::new(conflicts.alphabet().clone());
    let initial = cong.add_state((vec![], Void));
    let threshold = conflicts.threshold();

    // We maintain a set of missing transitions and go through them in order of creation for the states and in order
    // give by alphabet for the symbols for one state (this amouts to BFS).
    let mut queue: VecDeque<_> = conflicts
        .alphabet()
        .universe()
        .map(|sym| (initial, sym))
        .collect();
    'outer: while let Some((source, sym)) = queue.pop_front() {
        trace!(
            "Trying to add transition from {} on {}, cong size is {}",
            cong.state_color(source)
                .expect("Every state must be colored!")
                .blue(),
            sym.show().blue(),
            cong.size(),
        );

        // FIXME: This is a hack to avoid lifetime issues, find a better way...
        // TODO: figure out if this is the best way, we just take the upper bound on the number and assume that all states have sequential ids...
        for target in (0..cong.size()) {
            if !allow_transitions_into_epsilon && target == initial {
                continue;
            }
            let old_edge = cong.add_edge(source, A::expression(sym), target, Void);

            if conflicts.consistent(&cong)
                && additional_constraints.iter().all(|c| c.consistent(&cong))
            {
                trace!(
                    "\tTransition {}--{}-->{} is consistent",
                    cong.state_color(source)
                        .expect("We expect every state to be colored")
                        .green(),
                    sym.show(),
                    cong.state_color(target)
                        .expect("We expect every state to be colored")
                        .green()
                );
                continue 'outer;
            } else {
                trace!(
                    "\tTransition {}--{}-->{} is not consistent",
                    cong.state_color(source)
                        .expect("We expect every state to be colored")
                        .red(),
                    sym.show(),
                    cong.state_color(target)
                        .expect("We expect every state to be colored")
                        .red()
                );
                cong.remove_edges(source, A::expression(sym));
            }
        }

        let mut new_state_label = cong
            .state_color(source)
            .expect("We expect every state to be colored")
            .class()
            .clone();
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
        cong.add_edge(source, A::expression(sym), new_state, Void);
        queue.extend(std::iter::repeat(new_state).zip(conflicts.alphabet().universe()))
    }

    cong
}

#[cfg(test)]
pub(crate) mod tests {
    use automata::{
        alphabet,
        alphabet::CharAlphabet,
        congruence::FORC,
        prelude::*,
        transition_system::{Dottable, Sproutable},
        Class, Pointed, RightCongruence, TransitionSystem,
    };
    use itertools::Itertools;

    use crate::passive::{sample::OmegaSample, sprout::ConflictRelation, Sample};

    pub fn inf_aba_sample() -> (CharAlphabet, OmegaSample<CharAlphabet, bool>) {
        let Ok(sample) = OmegaSample::try_from(
            r#"omega
            alphabet: a,b
            positive:
            abab
            aba
            ababb
            abaa
            abaab
            negative:
            bab
            abba
            bbab
            aaabb
            a
            abb
            aabb
            babb
            b
            abbba
            abbb"#,
        ) else {
            panic!("Cannot parse sample")
        };
        (sample.alphabet.clone(), sample)
    }

    pub fn testing_larger_forc_sample() -> (CharAlphabet, OmegaSample<CharAlphabet, bool>) {
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

    fn testing_smaller_forc_smaple() -> (CharAlphabet, OmegaSample<CharAlphabet, bool>) {
        let alphabet = alphabet!(simple 'a', 'b', 'c');
        (
            alphabet.clone(),
            Sample::new_omega_from_pos_neg(
                alphabet,
                [
                    upw!("a"),
                    upw!("baa"),
                    upw!("aca"),
                    upw!("caab"),
                    upw!("abca"),
                ],
                [upw!("b"), upw!("c"), upw!("ab"), upw!("ac"), upw!("abc")],
            ),
        )
    }

    #[test]
    #[ignore]
    fn conflicts_inf_aba() {
        let (alphabet, sample) = inf_aba_sample();
        let conflicts = super::prefix_consistency_conflicts(sample);
        // conflicts.deprecated_display_rendered();
        todo!()
    }

    #[test]
    #[ignore]
    fn display_conflict_relation() {
        let (alphabet, sample) = testing_larger_forc_sample();
        let conflicts = super::prefix_consistency_conflicts(sample);
        // conflicts.deprecated_display_rendered();
        todo!()
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

        let conflicts: ConflictRelation<CharAlphabet> =
            super::iteration_consistency_conflicts(&split_sample, eps);
        // conflicts.dfas[0].deprecated_display_rendered();
        // conflicts.dfas[1].deprecated_display_rendered();
        println!(
            "{}",
            conflicts
                .conflicts
                .iter()
                .map(|(l, r)| format!("({l},{r})"))
                .join(", ")
        );
        let prc_eps = super::sprout(conflicts, vec![], false);
        // prc_eps.deprecated_display_rendered();
        todo!()
    }

    #[test]
    fn learn_larger_forc() {
        let (alphabet, sample) = testing_larger_forc_sample();
        let cong = sample.infer_right_congruence();
        let split = sample.split(&cong);
        let forc = split.infer_forc();
        let prc_eps = forc.prc(Class::epsilon()).unwrap();
        println!("{}", prc_eps.dot_representation());
        assert_eq!(prc_eps.size(), 13);
    }

    #[test]
    fn prefix_consistency_sprout_two() {
        let alphabet = alphabet!(simple 'a', 'b');
        let sample = Sample::new_omega(
            alphabet.clone(),
            vec![
                (upw!("a"), true),
                (upw!("b", "a"), false),
                (upw!("bb", "a"), true),
            ],
        );
        let mut expected_cong: RightCongruence<CharAlphabet, Void, Void> =
            RightCongruence::new(alphabet!(simple 'a', 'b'));
        let q0 = expected_cong.add_state(vec![]);
        let q1 = expected_cong.add_state(vec!['b']);
        expected_cong.add_edge(q0, 'b', q1, ());
        expected_cong.add_edge(q1, 'b', q0, ());
        expected_cong.add_edge(q0, 'a', q0, ());
        expected_cong.add_edge(q1, 'a', q1, ());

        let conflicts = super::prefix_consistency_conflicts(sample);

        let cong = super::sprout(conflicts, vec![], true);

        assert_eq!(cong.size(), expected_cong.size());
        for word in ["aba", "abbabb", "baabaaba", "bababaaba", "b", "a", ""] {
            let reached = cong.reached_state_color(word).unwrap();
            let expected = expected_cong.reached_state_color(word).unwrap();
            assert_eq!(
                reached, expected,
                "{} reached {}, expected was {}",
                word, reached, expected
            )
        }
    }

    #[test]
    fn prefix_consistency_sprout_one() {
        let alphabet = alphabet!(simple 'a', 'b');
        let sample = Sample::new_omega(
            alphabet.clone(),
            vec![(upw!("a"), false), (upw!("b"), true)],
        );
        let conflicts = super::prefix_consistency_conflicts(sample);
        let cong = super::sprout(conflicts, vec![], true);

        assert_eq!(cong.size(), 1);
        assert!(cong.contains_state_color(&vec![].into()));
    }

    #[test]
    fn prefix_consistency_sprout_four() {
        let alphabet = alphabet!(simple 'a', 'b');
        let sample = Sample::new_omega(
            alphabet.clone(),
            vec![
                (upw!("a"), false),
                (upw!("b", "a"), false),
                (upw!("bb", "a"), false),
                (upw!("bbb", "a"), true),
            ],
        );

        let conflicts = super::prefix_consistency_conflicts(sample);
        println!("{:?}", conflicts);

        let cong = super::sprout(conflicts, vec![], true);

        assert_eq!(cong.size(), 4);
        for class in ["", "b", "bb", "bbb"] {
            assert!(cong.contains_state_color(&class.into()))
        }
    }
}
