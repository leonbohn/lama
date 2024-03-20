use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    collections::{BTreeSet, VecDeque},
    fmt::{Debug, Display},
    hash::Hash,
    sync::Arc,
};

use automata::{
    automaton::{IntoDPA, OmegaAcceptanceCondition},
    prelude::*,
    transition_system::{operations::Quotient, reachable::ReachableStateIndices},
    word::Concat,
    Map, Set,
};
use bimap::BiMap;
use itertools::Itertools;
use tracing::{debug, info, trace};

use crate::{
    active::{oracle::MealyOracle, LStar},
    passive::fwpm::FWPM,
    priority_mapping::{CongruentPriorityMapping, PriorityMapping},
};

use super::{FiniteSample, OmegaSample, Sample};
type DFASample<D> = FiniteSample<<D as TransitionSystem>::Alphabet, bool>;

fn dpa<M: DPALike>(dpa: IntoDPA<M>) -> OmegaSample<M::Alphabet, bool>
where
    M: Clone,
{
    let start = std::time::Instant::now();
    let quot = dpa.prefix_congruence();
    let cong = quot.collect_right_congruence_bare();
    let mut sample = right_congruence_by_omega_words(&quot);

    for (i, class) in quot.partition().iter().enumerate() {
        let class_pm = priority_mapping_set_backed(&dpa, class);
        let cpm = CongruentPriorityMapping::new(&cong, i, class_pm.push_colors_to_outgoing_edges());
        let class_sample = priority_mapping_by_omega_words(cpm);
        sample.append(class_sample);
    }

    info!(
        "Computation of right congruence by prefix words took {}μs",
        start.elapsed().as_micros()
    );
    sample
}

fn right_congruence_by_omega_words<M: DPALike>(
    cong: &Quotient<M>,
) -> OmegaSample<M::Alphabet, bool> {
    let start = std::time::Instant::now();

    let mut pos = Set::default();
    let mut neg = Set::default();

    let class_count = cong.partition().size();
    for mtr in cong.minimal_transition_representatives() {
        let reached = cong
            .reached_state_index(&mtr)
            .expect("State must be present");
        trace!(
            "Considering transition representative {} with id {}",
            mtr.show(),
            reached
        );
        for (mr, separate) in cong.minimal_representatives() {
            trace!(
                "Considering minimal representative {} with id {}",
                mr.show(),
                separate
            );
            if reached == separate {
                continue;
            }
            let q = cong.unwrap_class_representative(reached);
            let p = cong.unwrap_class_representative(separate);
            assert!(p != q, "Same state cannot be in two different classes");

            let suffix = cong
                .ts()
                .into_dpa()
                .separate(p, q)
                .expect("Separation must be possible");

            let w1 = Concat(&mtr, &suffix).reduced();
            let w2 = Concat(&mr, suffix).reduced();

            let b1 = cong.ts().into_dpa().accepts(&w1);
            let b2 = cong.ts().into_dpa().accepts(&w2);
            assert_ne!(b1, b2);

            if b1 {
                pos.insert(w1);
            } else {
                neg.insert(w1);
            }
            if b2 {
                pos.insert(w2);
            } else {
                neg.insert(w2);
            }
        }
    }

    debug!(
        "Computation of right congruence by omega words took {}μs",
        start.elapsed().as_micros()
    );
    OmegaSample::new_omega_from_pos_neg(cong.alphabet().clone(), pos, neg)
}

fn for_mtr_mr_pairs<D: Deterministic + Pointed, F>(cong: D, mut f: F)
where
    F: FnMut(&[SymbolOf<D>], D::StateIndex, &[SymbolOf<D>], D::StateIndex),
{
    for mtr in cong.minimal_transition_representatives() {
        let reached = cong
            .reached_state_index(&mtr)
            .expect("State must be present");
        for (mr, idx) in cong.minimal_representatives() {
            if reached == idx {
                continue;
            }
            (f)(&mtr, reached, &mr, idx);
        }
    }
}

fn priority_mapping_by_omega_words<A: Alphabet>(
    cpm: CongruentPriorityMapping<'_, A>,
) -> OmegaSample<A, bool> {
    let start = std::time::Instant::now();

    let idempotents = cpm.mm().minimal_representatives().filter_map(|(rep, q)| {
        let Some(reached) = cpm.mm().reached_state_index_from(&rep, q) else {
            panic!("Input must be deterministic");
        };
        if q == reached {
            Some(rep)
        } else {
            None
        }
    });

    for_mtr_mr_pairs(cpm.mm(), |mtr, reached, mr, idx| {
        if reached == idx || mtr == mr {
            return; // from closure
        }
        trace!(
            "Considering pair {}@{reached} and {}@{idx}",
            Show::show(mtr),
            mr.show()
        );
        todo!()
    });

    todo!()
}

trait StateCollection<Idx, C>: Hash + Eq + Clone {
    fn pair_iter(&self) -> impl Iterator<Item = (Idx, C)>;
    fn from_pair_iter(iter: impl IntoIterator<Item = (Idx, C)>) -> Self;
    fn sink() -> Self {
        Self::from_pair_iter(std::iter::empty())
    }
    fn show(&self) -> String
    where
        Idx: Display,
        C: Show,
    {
        format!(
            "{{{}}}",
            self.pair_iter()
                .map(|(q, c)| format!("q{}|{}", q, c.show()))
                .join(", ")
        )
    }
}

impl<Idx: IndexType, C: Color> StateCollection<Idx, C> for Vec<(Idx, C)> {
    fn pair_iter(&self) -> impl Iterator<Item = (Idx, C)> {
        self.iter().cloned()
    }

    fn from_pair_iter(iter: impl IntoIterator<Item = (Idx, C)>) -> Self {
        iter.into_iter().collect()
    }
}

impl<Idx: IndexType, C: Color> StateCollection<Idx, C> for BTreeSet<(Idx, C)> {
    fn pair_iter(&self) -> impl Iterator<Item = (Idx, C)> {
        self.iter().cloned()
    }

    fn from_pair_iter(iter: impl IntoIterator<Item = (Idx, C)>) -> Self {
        iter.into_iter().collect()
    }
}

fn priority_mapping_set_backed<D: DPALike, X: Borrow<D::StateIndex>, I: IntoIterator<Item = X>>(
    dpa: D,
    initial: I,
) -> MooreMachine<D::Alphabet, D::EdgeColor> {
    let neutral_high = dpa
        .edge_colors()
        .max()
        .expect("There needs to be at least one color");
    build_generic_priority_mapping(
        dpa,
        initial
            .into_iter()
            .map(|q| (*q.borrow(), neutral_high))
            .collect::<BTreeSet<_>>(),
    )
}
fn priority_mapping_vec_backed<D: DPALike, I: IntoIterator<Item = D::StateIndex>>(
    dpa: D,
    initial: I,
) -> MooreMachine<D::Alphabet, D::EdgeColor> {
    let neutral_high = dpa
        .edge_colors()
        .max()
        .expect("There needs to be at least one color");
    build_generic_priority_mapping(
        dpa,
        initial
            .into_iter()
            .map(|q| (q, neutral_high))
            .collect::<Vec<_>>(),
    )
}

fn build_generic_priority_mapping<D, Coll>(
    dpa: D,
    initial: Coll,
) -> MooreMachine<D::Alphabet, D::EdgeColor>
where
    D: DPALike,
    Coll: StateCollection<D::StateIndex, D::EdgeColor>,
{
    let start = std::time::Instant::now();

    let neutral_high = dpa
        .edge_colors()
        .max()
        .expect("There needs to be at least one color");
    let neutral_low = dpa.edge_colors().min().unwrap();

    let mut cong = MooreMachine::new_for_alphabet(dpa.alphabet().clone());
    let mut states = vec![initial];
    let show_state = |state: &BTreeSet<(D::StateIndex, usize)>| {};

    let initial_idx: usize = cong.add_state(neutral_high);
    assert_eq!(initial_idx, 0);

    let mut sink = None;

    let mut iteration = 0;
    let mut queue = VecDeque::from_iter([initial_idx]);
    while let Some(source) = queue.pop_front() {
        iteration += 1;
        if iteration >= 300 {
            panic!("Too many iterations");
        }

        assert!(source < states.len(), "Wrong state index in queue");
        let state_with_color = states.get(source).cloned().unwrap();
        trace!(
            "Processing state {} with signature {}",
            owo_colors::OwoColorize::blue(&source.to_string()),
            state_with_color.show(),
        );

        'symbols: for sym in dpa.symbols() {
            // this updates a set of states
            let successor = Coll::from_pair_iter(state_with_color.pair_iter().map(|(q, c)| {
                let t = dpa
                    .transition(q, sym)
                    .expect("DPA must be deterministic and complete");
                (t.target(), std::cmp::min(c, t.color()))
            }));
            trace!("Computed successor on {}: {}", sym.show(), successor.show());
            let max_color = successor.pair_iter().map(|(_, c)| c).max().unwrap();

            if max_color == neutral_low {
                // this can be redirected to the sink state, if it exists
                if let Some(idx) = sink {
                    trace!(
                        "Adding edge to existing sink {idx}! {source}:{} --{}|{max_color}--> {idx}{}",
                        state_with_color.show(),
                        sym.show(),
                        successor.show()
                    );
                    cong.add_edge(source, dpa.make_expression(sym), idx, max_color);
                    continue 'symbols;
                }

                // we create the sink state
                let idx = cong.add_state(max_color);
                let s = Coll::sink();
                assert!(!states.contains(&s));
                states.push(s);

                trace!("Creating sink state {idx} and adding transition {source}:{} --{}|{max_color}--> {idx}{}",
                    state_with_color.show(),
                    sym.show(),
                    successor.show()
                );
                cong.add_edge(source, dpa.make_expression(sym), idx, max_color);
                sink = Some(idx);
                continue 'symbols;
            }

            if let Some(idx) = states.iter().position(|s| s == &successor) {
                trace!(
                    "Knwon target {idx}. Adding transition {source}:{} --{}|{max_color}--> {idx}{}",
                    state_with_color.show(),
                    sym.show(),
                    successor.show()
                );
                cong.add_edge(source, dpa.make_expression(sym), idx, max_color);
                continue 'symbols;
            }

            let idx = cong.add_state(max_color);
            trace!(
                "Adding transition {source}:{} --{}|{max_color}--> {idx}{}",
                state_with_color.show(),
                sym.show(),
                successor.show()
            );
            cong.add_edge(source, dpa.make_expression(sym), idx, max_color);

            assert_eq!(idx, states.len(), "Wrong state index");
            states.push(successor);
            queue.push_back(idx);
        }
    }

    if let Some(idx) = sink {
        trace!("Completed sink {idx} with loops on all symbols, emitting colour {neutral_low}");
        for sym in dpa.symbols() {
            cong.add_edge(idx, dpa.make_expression(sym), idx, neutral_low);
        }
    }

    debug!(
        "Computation of priority mapping took {}μs",
        start.elapsed().as_micros()
    );
    cong
}

pub fn characterize_moore<M>(dfa: M) -> DFASample<M>
where
    M: Congruence,
    StateColor<M>: Color,
{
    todo!()
}

pub fn actively_exchanged_words_dfa<D: DFALike>(dfa: D) -> DFASample<D> {
    todo!()
}

type MealySample<D> =
    Sample<<D as TransitionSystem>::Alphabet, Vec<SymbolOf<D>>, <D as TransitionSystem>::EdgeColor>;

pub fn actively_exchanged_words_mealy<D>(mm: D) -> MealySample<D>
where
    D: Congruence,
    EdgeColor<D>: Color,
{
    let alphabet = mm.alphabet().clone();
    let oracle = MealyOracle::new(mm, None);
    // let mut lstar = LStar::logged(oracle, alphabet);
    // let learned = lstar.learn();
    // let mut sample = MealySample::new(alphabet);
    todo!()
}

#[cfg(test)]
mod tests {
    use std::time::Instant;

    use automata::{
        automaton::{DPALike, MealyLike, MooreLike, DPA},
        transition_system::{Deterministic, Dottable, NTS},
        TransitionSystem,
    };

    use crate::passive::{
        fwpm::FWPM,
        precise::build_precise_dpa_for,
        sample::characterize::{
            build_generic_priority_mapping, priority_mapping_set_backed,
            priority_mapping_vec_backed, right_congruence_by_omega_words,
        },
    };

    #[test_log::test]
    #[ignore]
    fn priority_mapping() {
        let mm = NTS::builder()
            .with_transitions([
                (0, 'a', 2, 1),
                (0, 'b', 1, 0),
                (1, 'a', 2, 1),
                (1, 'b', 1, 2),
                (2, 'a', 0, 0),
                (2, 'b', 1, 0),
            ])
            .into_dpa(0);

        let pmset = priority_mapping_set_backed(&mm, mm.state_indices());
        let pmvec = priority_mapping_vec_backed(&mm, mm.state_indices()).minimize();
        pmvec.display_rendered().unwrap();

        let fwpm = FWPM::new(
            mm.prefix_congruence().collect_right_congruence_bare(),
            [(0, pmset)].into_iter().collect(),
        );

        let precise = build_precise_dpa_for(fwpm);
        mm.display_rendered().unwrap();
    }

    #[test]
    #[ignore]
    fn priority_mapping_all_colors() {
        let dpa = NTS::builder()
            .with_transitions([
                (0, 'a', 0, 1),
                (0, 'b', 1, 0),
                (0, 'c', 1, 0),
                (1, 'a', 1, 1),
                (1, 'b', 0, 2),
                (1, 'c', 1, 1),
                (2, 'a', 1, 2),
                (2, 'b', 1, 2),
                (2, 'c', 0, 0),
            ])
            .into_dpa(0);
        let start = Instant::now();
        let mm = priority_mapping_vec_backed(&dpa, dpa.state_indices());
        println!(
            "Vec backed took {} microseconds",
            start.elapsed().as_micros()
        );
        let start = Instant::now();
        let mm = priority_mapping_set_backed(&dpa, dpa.state_indices());
        println!(
            "Set backed took {} microseconds",
            start.elapsed().as_micros()
        );
        mm.display_rendered();
        mm.push_colors_to_outgoing_edges()
            .minimize()
            .display_rendered()
            .unwrap();
    }

    fn simple_dpa() -> DPA {
        NTS::builder()
            .with_transitions([
                (0, 'a', 0, 0),
                (0, 'b', 1, 1),
                (1, 'a', 3, 1),
                (1, 'b', 1, 0),
            ])
            .into_dpa(0)
    }

    #[test_log::test]
    fn characterize_prefix_cong() {
        let dpa = simple_dpa();

        assert!(dpa
            .as_ref()
            .with_initial(0)
            .into_dpa()
            .witness_inequivalence(&dpa.as_ref().with_initial(1).into_dpa())
            .is_some());

        let cong = dpa.prefix_congruence();
        assert_eq!(cong.size(), 2);
        let sample = right_congruence_by_omega_words(&cong);
    }

    #[test_log::test]
    #[ignore]
    fn characterize_dpa() {
        let dpa = simple_dpa();
        let sample = super::dpa(dpa);
        println!("{:?}", sample);
    }
}
