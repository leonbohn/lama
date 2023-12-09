use std::{
    cell::{Cell, RefCell},
    collections::{BTreeSet, VecDeque},
    fmt::Debug,
    sync::Arc,
};

use automata::{prelude::*, ts::reachable::ReachableStateIndices, Set};
use bimap::BiMap;
use itertools::Itertools;
use tracing::{debug, trace};

use crate::{
    active::{oracle::MealyOracle, LStar},
    passive::fwpm::FWPM,
    priority_mapping::PriorityMapping,
};

use super::{FiniteSample, OmegaSample, Sample};
type DFASample<D> = FiniteSample<<D as TransitionSystem>::Alphabet, bool>;

fn build_priority_mapping<D: DPALike, I: IntoIterator<Item = D::StateIndex>>(
    dpa: D,
    initial: I,
) -> MooreMachine<D::Alphabet, D::EdgeColor, D::EdgeColor> {
    let print_state_set =
        |set: &BTreeSet<D::StateIndex>| format!("{{{}}}", set.iter().map(|q| q.show()).join(", "));

    let start = std::time::Instant::now();

    let mut cong = MooreMachine::new_for_alphabet(dpa.alphabet().clone());
    let mut bimap: BiMap<usize, BTreeSet<D::StateIndex>> = BiMap::default();

    let neutral_high = dpa
        .edge_colors()
        .max()
        .expect("There needs to be at least one color");
    let neutral_low = dpa.edge_colors().min().unwrap();

    let initial_idx = cong.add_state(neutral_high);
    assert_eq!(initial_idx, 0);
    let initial_state: BTreeSet<_> = initial.into_iter().collect();
    bimap.insert(initial_idx, initial_state);

    let mut iteration = 0;

    let mut queue = VecDeque::from_iter([initial_idx]);
    while let Some(source) = queue.pop_front() {
        iteration += 1;
        if iteration >= 300 {
            panic!("Too many iterations");
        }

        let state = bimap
            .get_by_left(&source)
            .expect("State must be present to have been added to the queue");
        let color = cong.state_color(source).expect("state is present");
        let mut to_add = vec![];

        'symbols: for sym in dpa.symbols() {
            // this updates a set of states
            let (succ, c) =
                state
                    .iter()
                    .fold((BTreeSet::default(), neutral_low), |(mut acc, c), q| {
                        let t = dpa
                            .transition(*q, sym)
                            .expect("DPA must be deterministic and complete");
                        acc.insert(t.target());
                        (acc, std::cmp::max(c, t.color()))
                    });
            let succ_color = std::cmp::min(color, c);
            trace!("Computed color {succ_color} as minimum of {color} and {c}");

            if let Some(idx) = bimap.get_by_right(&succ) {
                if cong.state_color(*idx).unwrap() == succ_color {
                    trace!(
                        "Adding transition {source}:{} --{}|{succ_color}--> {idx}{}",
                        print_state_set(state),
                        sym.show(),
                        print_state_set(&succ)
                    );
                    cong.add_edge(source, dpa.make_expression(sym), *idx, succ_color);
                    continue 'symbols;
                }
            }

            let idx = cong.add_state(succ_color);
            trace!(
                "Adding transition {source}:{} --{}|{succ_color}--> {idx}{}",
                print_state_set(state),
                sym.show(),
                print_state_set(&succ)
            );
            cong.add_edge(source, dpa.make_expression(sym), idx, succ_color);

            to_add.push((idx, succ));
            queue.push_back(idx);
        }

        for (idx, succ) in to_add {
            bimap.insert(idx, succ);
        }
    }

    debug!(
        "Computation of priority mapping took {}μs",
        start.elapsed().as_micros()
    );
    cong
}

pub fn characterize_dfa<D: DFALike>(dfa: D) -> DFASample<D> {
    todo!()
}

pub fn actively_exchanged_words_dfa<D: DFALike>(dfa: D) -> DFASample<D> {
    todo!()
}

type MealySample<D> =
    Sample<<D as TransitionSystem>::Alphabet, Vec<SymbolOf<D>>, <D as TransitionSystem>::EdgeColor>;

pub fn actively_exchanged_words_mealy<D: MealyLike + Deterministic>(mm: D) -> MealySample<D> {
    let alphabet = mm.alphabet().clone();
    let oracle = MealyOracle::new(mm, None);
    // let mut lstar = LStar::logged(oracle, alphabet);
    // let learned = lstar.learn();
    // let mut sample = MealySample::new(alphabet);
    todo!()
}

#[cfg(test)]
mod tests {
    use automata::{
        ts::{ToDot, NTS},
        TransitionSystem,
    };

    use crate::passive::sample::characterize::build_priority_mapping;

    #[test_log::test]
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

        let pm = build_priority_mapping(&mm, mm.state_indices());
        pm.display_rendered().unwrap();
    }
}
