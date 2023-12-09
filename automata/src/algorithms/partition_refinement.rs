use std::{
    collections::{BTreeSet, VecDeque},
    hash::Hash,
    time::Instant,
};

use itertools::Itertools;
use tracing::{debug, trace};

use crate::{
    automata::AsMealyMachine, prelude::*, ts::transition_system::IsTransition, Alphabet, Map,
    Partition, Set,
};

pub fn mealy_partition_refinement<M: MealyLike>(mm: M) -> AsMealyMachine<M> {
    let start = Instant::now();
    let mut queue: Vec<Set<_>> = vec![mm.state_indices().collect()];

    let mut partition = vec![mm.state_indices().collect()];

    while let Some(set) = queue.pop() {
        for sym in mm.symbols() {
            let mut splitter = Map::default();
            for q in mm.state_indices() {
                if let Some(t) = mm.transition(q, sym) {
                    if set.contains(&t.target()) {
                        splitter
                            .entry(t.color())
                            .or_insert(Set::default())
                            .insert(q);
                    }
                }
            }

            for (c, x) in splitter {
                let mut new_partition = vec![];
                for y in &partition {
                    if x.intersection(y).next().is_none() || y.difference(&x).next().is_none() {
                        new_partition.push(y.clone());
                        continue;
                    }
                    let int = x.intersection(y).cloned().collect::<Set<_>>();
                    let diff = y.difference(&x).cloned().collect::<Set<_>>();

                    if let Some(pos) = queue.iter().position(|o| o == y) {
                        queue.remove(pos);
                        queue.extend([int.clone(), diff.clone()]);
                    } else {
                        queue.push(if int.len() <= diff.len() {
                            int.clone()
                        } else {
                            diff.clone()
                        });
                    }
                    new_partition.extend([int, diff])
                }
                partition = new_partition
            }
        }
    }

    debug!(
        "Partition refinement execution took {} microseconds",
        start.elapsed().as_micros()
    );

    trace!(
        "Building quotient with partition {{{}}}",
        partition
            .iter()
            .map(|set| format!("{{{}}}", set.iter().map(|c| c.to_string()).join(", ")))
            .join(", ")
    );

    let start = Instant::now();

    let out = mm
        .quotient(partition.into())
        .map_edge_colors(|c| {
            // assert!(c.iter().all_equal());
            c[0].clone()
        })
        .erase_state_colors()
        .collect_mealy();
    debug!(
        "Collecting into Mealy machine took {} microseconds",
        start.elapsed().as_micros()
    );
    out
}

pub fn partition_refinement<D: DFALike>(dfa: D) -> Partition<D::StateIndex> {
    let accepting = dfa.accepting_states().collect::<Set<_>>();
    let rejecting = dfa.rejecting_states().collect::<Set<_>>();
    let mut p: Vec<_> = [rejecting, accepting]
        .into_iter()
        .filter(|o| !o.is_empty())
        .collect();
    let mut w = p.clone();

    while let Some(a) = w.pop() {
        for sym in dfa.alphabet().universe() {
            let x = dfa
                .state_indices()
                .filter(|q| {
                    dfa.transition(*q, sym)
                        .map(|t| a.contains(&t.target()))
                        .unwrap_or(false)
                })
                .collect::<Set<_>>();

            let mut new_p = vec![];
            for y in &p {
                if x.intersection(y).next().is_none() || y.difference(&x).next().is_none() {
                    new_p.push(y.clone());
                    continue;
                }
                let int = x.intersection(y).cloned().collect::<Set<_>>();
                let diff = y.difference(&x).cloned().collect::<Set<_>>();

                if let Some(pos) = w.iter().position(|o| o == y) {
                    w.remove(pos);
                    w.extend([int.clone(), diff.clone()]);
                } else {
                    w.push(if int.len() <= diff.len() {
                        int.clone()
                    } else {
                        diff.clone()
                    });
                }

                new_p.extend([int, diff]);
            }
            p = new_p;
        }
    }
    Partition(p)
}

#[cfg(test)]
mod tests {
    use crate::{alphabet::Fixed, prelude::*, tests::wiki_dfa, Partition};

    use super::partition_refinement;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn partition_refinement_wiki() {
        let dfa = wiki_dfa();

        let p = partition_refinement(&dfa);
        assert_eq!(p, Partition::new([vec![0, 1], vec![5], vec![2, 3, 4]]))
    }

    #[test_log::test]
    fn partition_refinement_mealy() {
        let mm = NTS::builder()
            .with_transitions([
                (0, 'a', 0, 1),
                (0, 'b', 1, 0),
                (1, 'a', 0, 0),
                (1, 'b', 1, 0),
            ])
            .into_dpa(0);
        let minimized = mm.minimized();
        assert_eq!(minimized.size(), 1)
    }
}
