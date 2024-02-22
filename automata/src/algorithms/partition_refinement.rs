//! Partition refinement algorithms for determinsitic finite automata. This module implements the
//! Hopcroft and Moore algorithms for minimizing deterministic finite automata. There are two main
//! variants of the algorithm, one for Moore machines and one for Mealy machines. They differ only
//! on the way they handle the output of the automaton. Specifically, for Moore machines, we consider
//! the output of the state, whereas for Mealy machines, we consider the output of the transition.
//! It is necessary to have two distinct algorithms (so with different names) as there might be
//! transition systems which have outputs on both the states and the transitions.
use std::{
    collections::{BTreeSet, VecDeque},
    hash::Hash,
    time::Instant,
};

use itertools::Itertools;
use tracing::{debug, info, trace};

use crate::{prelude::*, ts::transition_system::IsEdge, Alphabet, Map, Partition, Set};

/// Computes the maximal bisimulation of the given [`MealyLike`] deterministic machine. The returned
/// partition is a [`Partition`] of the state indices, where any states in the same class of the
/// returned partition are pairwise bisimilar. This means for any *non-empty* input, they produce
/// the same sequence of outputs.
pub fn mealy_greatest_bisimulation<D>(mm: D) -> Partition<D::StateIndex>
where
    D: Deterministic,
    EdgeColor<D>: Color,
{
    let start = Instant::now();
    let mut queue: Vec<BTreeSet<_>> = vec![mm.state_indices().collect()];

    let mut partition: Vec<BTreeSet<_>> = vec![mm.state_indices().collect()];

    while let Some(set) = queue.pop() {
        for sym in mm.symbols() {
            let mut splitter = Map::default();
            for q in mm.state_indices() {
                if let Some(t) = mm.transition(q, sym) {
                    if set.contains(&t.target()) {
                        splitter
                            .entry(t.color())
                            .or_insert(BTreeSet::default())
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
                    let int = x.intersection(y).cloned().collect::<BTreeSet<_>>();
                    let diff = y.difference(&x).cloned().collect::<BTreeSet<_>>();

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
        "computing greatest bisimulation for Mealy Machine took {} microseconds",
        start.elapsed().as_micros()
    );
    partition.into()
}

/// Partition refinement algorithm for deterministic finite automata that have outputs on the edges.
/// Runs in O(n log n) time, where n is the number of states of the automaton and returns the unique
/// minimal automaton that is bisimilar to the input.
pub fn mealy_partition_refinement<D>(mm: D) -> MealyMachine<D::Alphabet, EdgeColor<D>>
where
    D: Congruence,
    EdgeColor<D>: Color,
{
    let partition = mealy_greatest_bisimulation(&mm);
    trace!(
        "Building quotient with partition {{{}}}",
        partition
            .iter()
            .map(|set| format!("{{{}}}", set.iter().map(|c| c.to_string()).join(", ")))
            .join(", ")
    );

    let start = Instant::now();

    let out = mm
        .quotient(partition)
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

/// Computes the maximal bisimulation for a given Moore machine. This is mainly used for
/// executing the partition refinement algorithm for Moore machines, see [`moore_partition_refinement`].
///
/// Two states of a mealy machine are considered to be bisimilar if and only if they have the same
/// output on all words. This gives a [`Partition`] of the state indices, where any states in the
/// same class of the returned partition are pairwise bisimilar.
pub fn moore_greatest_bisimulation<D>(mm: D) -> Partition<D::StateIndex>
where
    D: Deterministic,
    StateColor<D>: Color,
{
    let start = Instant::now();

    let mut presplit: Map<_, _> = Map::default();
    for (q, c) in mm.state_indices_with_color() {
        presplit.entry(c).or_insert(BTreeSet::default()).insert(q);
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
                .collect::<BTreeSet<_>>();

            let mut new_p = vec![];
            for y in &partition {
                if x.intersection(y).next().is_none() || y.difference(&x).next().is_none() {
                    new_p.push(y.clone());
                    continue;
                }
                let int = x.intersection(y).cloned().collect::<BTreeSet<_>>();
                let diff = y.difference(&x).cloned().collect::<BTreeSet<_>>();

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
        "computed greatest bisimulation for Moore machine in {} microseconds",
        start.elapsed().as_micros()
    );
    partition.into()
}

/// Partition refinement algorithm for deterministic finite automata that have outputs on the states.
/// Runs in O(n log n) time, where n is the number of states of the automaton and returns the unique
/// minimal automaton that is bisimilar to the input. This method computes the maximal bisimulation
/// by using [`moore_greatest_bisimulation`] and then uses the partition to compute the quotient
/// automaton.
pub fn moore_partition_refinement<D>(mm: D) -> MooreMachine<D::Alphabet, D::StateColor>
where
    D: Congruence,
    StateColor<D>: Color,
{
    let partition = moore_greatest_bisimulation(&mm);
    trace!(
        "Building quotient with partition {{{}}}",
        partition
            .iter()
            .map(|set| format!("{{{}}}", set.iter().map(|c| c.to_string()).join(", ")))
            .join(", ")
    );

    let start = Instant::now();

    let out = mm
        .quotient(partition)
        .map_state_colors(|c| {
            // assert!(c.iter().all_equal());
            c[0].clone()
        })
        // TODO: Should we not get rid of the edge colors entirely?
        .erase_edge_colors()
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
