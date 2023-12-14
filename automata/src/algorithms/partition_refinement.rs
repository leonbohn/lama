use std::{
    collections::{BTreeSet, VecDeque},
    hash::Hash,
    time::Instant,
};

use itertools::Itertools;
use tracing::{debug, info, trace};

use crate::{
    automaton::{AsMealyMachine, AsMooreMachine},
    prelude::*,
    ts::transition_system::IsTransition,
    Alphabet, Map, Partition, Set,
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

    info!(
        "Mealy partition refinement execution took {} microseconds",
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
    info!(
        "Collecting into Mealy machine took {} microseconds",
        start.elapsed().as_micros()
    );
    out
}

pub fn moore_partition_refinement<D: MooreLike>(mm: D) -> AsMooreMachine<D> {
    let start = Instant::now();

    let mut presplit: Map<_, _> = Map::default();
    for (q, c) in mm.state_indices_with_color() {
        presplit.entry(c).or_insert(Set::default()).insert(q);
    }
    let mut partition: Vec<_> = presplit.into_values().collect();
    let mut queue = partition.clone();

    while let Some(a) = queue.pop() {
        for sym in mm.symbols() {
            let x = mm
                .state_indices()
                .filter(|q| {
                    mm.transition(*q, sym)
                        .map(|t| a.contains(&t.target()))
                        .unwrap_or(false)
                })
                .collect::<Set<_>>();

            let mut new_p = vec![];
            for y in &partition {
                if x.intersection(y).next().is_none() || y.difference(&x).next().is_none() {
                    new_p.push(y.clone());
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

                new_p.extend([int, diff]);
            }
            partition = new_p;
        }
    }

    debug!(
        "Moore partition refinement execution took {} microseconds",
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
        .map_state_colors(|c| {
            // assert!(c.iter().all_equal());
            c[0].clone()
        })
        // TODO: Should we not get rid of the edge colors entirely?
        .map_edge_colors(|c| c[0].clone())
        .collect_moore();
    debug!(
        "Collecting into Moore machine took {} microseconds",
        start.elapsed().as_micros()
    );
    out
}

#[cfg(test)]
mod tests {
    use crate::{alphabet::Fixed, prelude::*, tests::wiki_dfa, Partition};

    use super::moore_partition_refinement;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test_log::test]
    fn partition_refinement_moore() {
        let dfa = wiki_dfa();

        let p = moore_partition_refinement(&dfa);
        assert_eq!(p.size(), 3);
        assert!(p.moore_bisimilar(dfa));
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
        let minimized = mm.into_mealy().minimize();
        assert_eq!(minimized.size(), 1)
    }
}
