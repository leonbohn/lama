use std::{
    collections::{BTreeSet, VecDeque},
    hash::Hash,
    marker::PhantomData,
};

use itertools::Itertools;
use tracing::{info, trace};

use super::{acceptor::OmegaSemantics, Automaton};
use crate::{
    prelude::*,
    ts::{
        connected_components::Scc,
        operations::{MapEdgeColor, MapStateColor},
        CollectDTS, Quotient, Shrinkable,
    },
    HasParity, Partition, Set,
};

/// Represents a parity condition which accepts if and only if the least color that
/// is seen infinitely often is even.
#[derive(Clone, Debug, Default, Copy, Hash, Eq, PartialEq)]
pub struct MinEven;

impl<Q> OmegaSemantics<Q, usize> for MinEven {
    type Output = bool;
    fn omega_semantic<R>(&self, run: R) -> Self::Output
    where
        R: OmegaRun<StateColor = Q, EdgeColor = usize>,
    {
        run.infinity_edge_colors()
            .and_then(|colors| colors.min())
            .map(|x| x.is_even())
            .unwrap_or(false)
    }
}

#[derive(Clone, Debug, Default, Copy, Hash, Eq, PartialEq)]
pub struct MaxEven;
#[derive(Clone, Debug, Default, Copy, Hash, Eq, PartialEq)]
pub struct MinOdd;
#[derive(Clone, Debug, Default, Copy, Hash, Eq, PartialEq)]
pub struct MaxOdd;

/// A deterministic parity automaton (DPA). It uses a [`DTS`]
/// as its transition system and a `usize` as its edge color.
/// The acceptance condition is given by the type `Sem`, which
/// defaults to [`MinEven`], meaning the automaton accepts
/// if the least color that appears infinitely often during
/// a run is even.
pub type DPA<A = CharAlphabet, Sem = MinEven> =
    Automaton<Initialized<DTS<A, Void, usize>>, Sem, true>;
/// Helper trait for converting a given transition system into a [`DPA`]
/// with the given semantics.
pub type IntoDPA<T, Sem = MinEven> = Automaton<T, Sem, true>;

/// Trait that should be implemented by every object that can be viewed as a [`crate::DPA`].
pub trait DPALike: Congruence<EdgeColor = usize> {
    /// Consumes `self` and returns a [`DPA`] from the transition system underlying `self`.
    fn into_dpa(self) -> IntoDPA<Self> {
        Automaton::from_parts(self, MinEven)
    }

    /// Uses a reference to `self` for creating a [`DPA`] from the underlying transition system.
    fn borrow_dpa(&self) -> IntoDPA<&Self> {
        self.into_dpa()
    }

    /// Consumes `self` and returns a [`DPA`] from the transition system underlying `self`. Note
    /// that this restricts to reachable states.
    fn collect_dpa(self) -> IntoDPA<Initialized<CollectDTS<Self>>> {
        self.trim_collect().into_dpa()
    }
}

impl<Ts> DPALike for Ts where Ts: Congruence<EdgeColor = usize> {}

impl<D: DPALike> IntoDPA<D> {
    /// Gives a witness for the fact that the language accepted by `self` is not empty. This is
    /// done by finding an accepting cycle in the underlying transition system.
    pub fn give_word(&self) -> Option<ReducedOmegaWord<SymbolOf<Self>>> {
        todo!()
    }

    /// Builds the complement of `self`, i.e. the DPA that accepts the complement of the language
    /// accepted by `self`. This is a cheap operation as it only requires to increment all edge
    /// colors by one.
    pub fn complement(self) -> IntoDPA<impl DPALike<Alphabet = D::Alphabet>> {
        self.map_edge_colors(|c| c + 1).collect_dpa()
    }

    /// Gives a witness for the fact that `left` and `right` are not language-equivalent. This is
    /// done by finding a separating word, i.e. a word that is accepted from one of the two states
    /// but not by the other.
    pub fn separate<X, Y>(&self, left: X, right: Y) -> Option<ReducedOmegaWord<SymbolOf<Self>>>
    where
        X: Indexes<Self>,
        Y: Indexes<Self>,
    {
        let p = left.to_index(self)?;
        let q = right.to_index(self)?;

        if p == q {
            return None;
        }

        self.with_initial(p)
            .into_dpa()
            .witness_inequivalence(&self.with_initial(q).into_dpa())
    }

    /// Computes a [`Partition`] of the state indices of `self` such that any two states in the
    /// same class of the partition are language-equivalent. This is done iteratively, by considering each
    /// state and finding a state that is language-equivalent to it. If no such state exists, a new
    /// class is created.
    pub fn prefix_partition(&self) -> Partition<D::StateIndex> {
        fn print<X: Show>(part: &[BTreeSet<X>]) -> String {
            format!(
                "{{{}}}",
                part.iter()
                    .map(|class| format!("[{}]", class.iter().map(|x| x.show()).join(", ")))
                    .join(", ")
            )
        }
        let mut it = self.reachable_state_indices();
        let fst = it.next();
        assert_eq!(fst, Some(self.initial()));

        let mut partition = vec![BTreeSet::from_iter([self.initial()])];
        let mut queue: VecDeque<_> = it.collect();
        let expected_size = queue.len() + 1;

        'outer: while let Some(q) = queue.pop_front() {
            trace!(
                "considering state {}, current partition: {}",
                q.show(),
                print(&partition)
            );
            for i in 0..partition.len() {
                let p = partition[i]
                    .first()
                    .expect("Class of partition must be non-empty");
                if self
                    .as_ref()
                    .with_initial(*p)
                    .into_dpa()
                    .language_equivalent(&self.as_ref().with_initial(q).into_dpa())
                {
                    trace!(
                        "it is language equivalent to {}, adding it to the equivalence class",
                        p.show()
                    );
                    partition.get_mut(i).unwrap().insert(q);
                    continue 'outer;
                }
            }
            trace!("not equivalent to any known states, creating a new equivalence class");
            partition.push(BTreeSet::from_iter([q]));
        }
        debug_assert_eq!(
            partition.iter().fold(0, |acc, x| acc + x.len()),
            expected_size,
            "size mismatch!"
        );
        partition.into()
    }

    /// Builds the quotient of `self` with respect to the prefix partition. This will result in the prefix
    /// congruence that underlies the language accepted by `self`.
    pub fn prefix_congruence(&self) -> Quotient<&Self> {
        self.quotient(self.prefix_partition())
    }

    /// Attempts to find an omega-word that witnesses the given `color`, meaning the least color that
    /// appears infinitely often during the run of the returned word is equal to `color`. If no such
    /// word exists, `None` is returned.
    pub fn witness_color(&self, color: D::EdgeColor) -> Option<ReducedOmegaWord<SymbolOf<Self>>> {
        let restrict = self.edge_color_restricted(color, usize::MAX);
        let sccs = restrict.sccs();
        for scc in sccs.iter() {
            if scc.is_transient() {
                continue;
            }
            if scc.interior_edge_colors().contains(&color) {
                let (q, rep) = scc
                    .minimal_representative()
                    .as_ref()
                    .expect("We know this is reachable");
                let cycle = scc
                    .maximal_loop_from(*rep)
                    .expect("This thing is non-transient");
                return Some(ReducedOmegaWord::ultimately_periodic(q, cycle));
            }
        }
        None
    }

    /// Attempts to find an omega-word `w` such that the least color seen infinitely often
    /// during the run of `self` on `w` is equal to `k` and the least color seen infinitely often
    /// during the run of `other` on `w` is equal to `l`. If no such word exists, `None` is returned.
    /// Main use of this is to witness the fact that `self` and `other` are not language-equivalent.
    pub fn witness_colors<O: DPALike<Alphabet = D::Alphabet>>(
        &self,
        k: usize,
        other: &IntoDPA<O>,
        l: usize,
    ) -> Option<ReducedOmegaWord<SymbolOf<Self>>> {
        trace!("attempting to witness colors {k} and {l}");
        let t1 = self.edge_color_restricted(k, usize::MAX);
        let t2 = other.edge_color_restricted(l, usize::MAX);
        let prod = t1.ts_product(t2);
        let sccs = prod.sccs();
        for scc in sccs.iter() {
            if scc.is_transient() {
                continue;
            }
            let (a, b) = scc
                .interior_edge_colors()
                .iter()
                .min()
                .expect("we know this is not transient");
            if *a == k && *b == l {
                let Some((mr, spoke)) = scc.minimal_representative() else {
                    continue;
                };
                let cycle = scc
                    .maximal_loop_from(*spoke)
                    .expect("This thing is non-transient");
                return Some(ReducedOmegaWord::ultimately_periodic(mr, cycle));
            }
        }
        None
    }

    /// Returns an iterator over all colors that appear on edges of `self`.
    pub fn colors(&self) -> impl Iterator<Item = D::EdgeColor> + '_ {
        MealyLike::color_range(self)
    }

    /// Attempts to find an omega-word that witnesses the fact that `self` and `other` are not
    /// language-equivalent. If no such word exists, `None` is returned. Internally, this uses
    /// [`Self::witness_not_subset_of`] in both directions.
    pub fn witness_inequivalence<O: DPALike<Alphabet = D::Alphabet>>(
        &self,
        other: &IntoDPA<O>,
    ) -> Option<ReducedOmegaWord<SymbolOf<D>>> {
        self.witness_not_subset_of(other)
            .or(other.witness_not_subset_of(self))
    }

    /// Returns true if `self` is language-equivalent to `other`, i.e. if and only if the Two
    /// DPAs accept the same language.
    pub fn language_equivalent<O: DPALike<Alphabet = D::Alphabet>>(
        &self,
        other: &IntoDPA<O>,
    ) -> bool {
        self.witness_inequivalence(other).is_none()
    }

    /// Returns true if and only if `self` is included in `other`, i.e. if and only if the language
    /// accepted by `self` is a subset of the language accepted by `other`.
    pub fn included_in<O: DPALike<Alphabet = D::Alphabet>>(&self, other: &IntoDPA<O>) -> bool {
        self.witness_not_subset_of(other).is_none()
    }

    /// Returns true if and only if `self` includes `other`, i.e. if and only if the language
    /// accepted by `self` is a superset of the language accepted by `other`.
    pub fn includes<O: DPALike<Alphabet = D::Alphabet>>(&self, other: &IntoDPA<O>) -> bool {
        other.witness_not_subset_of(self).is_none()
    }

    /// Attempts to find an omega-word that witnesses the fact that `self` is not included in `other`.
    /// If no such word exists, `None` is returned.
    pub fn witness_not_subset_of<O: DPALike<Alphabet = D::Alphabet>>(
        &self,
        other: &IntoDPA<O>,
    ) -> Option<ReducedOmegaWord<SymbolOf<D>>> {
        for i in self.colors().filter(|x| x.is_even()) {
            for j in other.colors().filter(|x| x.is_odd()) {
                if let Some(cex) = self.as_ref().witness_colors(i, other, j) {
                    trace!(
                        "found counterexample {:?}, witnessing colors {i} and {j}",
                        cex
                    );
                    return Some(cex);
                } else {
                    trace!("colors {i} and {j} are not witnessed by any word");
                }
            }
        }
        None
    }

    /// Produces a DPA that is language-equivalent to `self` but has the minimal number of different colors. This
    /// done by a procedure which in essence was first introduced by Carton and Maceiras in their paper
    /// "Computing the rabin index of a finite automaton". The procedure that this implementation actually uses
    /// is outlined by Schewe and Ehlers in [Natural Colors of Infinite Words](https://arxiv.org/pdf/2207.11000.pdf)
    /// in Section 4.1, Definition 2.
    pub fn normalized(&self) -> IntoDPA<impl DPALike<Alphabet = D::Alphabet>>
    where
        EdgeColor<Self>: Eq + Hash + Clone + Ord,
        StateColor<Self>: Eq + Hash + Clone + Ord,
    {
        let start = std::time::Instant::now();

        let mut ts: Initialized<HashTs<_, D::StateColor, D::EdgeColor>> = self.collect_pointed().0;
        let out = ts.clone();

        let mut recoloring = Vec::new();
        let mut remove_states = Vec::new();
        let mut remove_edges = Vec::new();

        let mut priority = 0;
        'outer: loop {
            for (source, expression) in remove_edges.drain(..) {
                ts.remove_edge(source, &expression).is_some();
            }
            for state in remove_states.drain(..) {
                ts.remove_state(state).is_some();
            }

            if ts.size() == 0 {
                trace!("no states left, terminating");
                break 'outer;
            }

            let dag = ts.tarjan_dag();

            'inner: for scc in dag.iter() {
                trace!("inner priority {priority} | scc {:?}", scc);
                if scc.is_transient() {
                    trace!("scc is transient");
                    for state in scc.iter() {
                        for edge in ts.edges_from(*state).unwrap() {
                            trace!(
                                "recolouring and removing {} --{}|{}--> {} with priority {}",
                                state,
                                edge.expression().show(),
                                edge.color().show(),
                                edge.target(),
                                priority
                            );
                            recoloring.push(((*state, edge.expression().clone()), priority));
                            remove_edges.push((edge.source(), edge.expression().clone()));
                        }
                        remove_states.push(*state);
                    }
                    continue 'inner;
                }
                let minimal_interior_edge_color = scc
                    .interior_edge_colors()
                    .iter()
                    .min()
                    .expect("We know this is not transient");

                if priority % 2 != minimal_interior_edge_color % 2 {
                    trace!("minimal interior priority: {minimal_interior_edge_color}, skipping");
                    continue 'inner;
                }

                trace!(
                    "minimal interior priority: {minimal_interior_edge_color}, recoloring edges"
                );
                for (q, a, c, p) in scc
                    .interior_edges()
                    .iter()
                    .filter(|(q, a, c, p)| c == minimal_interior_edge_color)
                {
                    trace!(
                        "recolouring and removing {} --{}|{}--> {} with priority {}",
                        q,
                        a.show(),
                        c.show(),
                        p,
                        priority
                    );
                    recoloring.push(((*q, a.clone()), priority));
                    remove_edges.push((*q, a.clone()));
                }
            }

            if remove_edges.is_empty() {
                priority += 1;
            }
        }

        let ret = out
            .map_edge_colors_full(|q, e, _, _| {
                let Some(c) = recoloring
                    .iter()
                    .find(|((p, f), _)| *p == q && f == e)
                    .map(|(_, c)| *c)
                else {
                    panic!("Could not find recoloring for edge ({}, {:?})", q, e);
                };
                c
            })
            .collect_dpa();

        info!("normalizing DPA took {} μs", start.elapsed().as_micros());

        debug_assert!(self.language_equivalent(&ret));
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::DPA;
    use crate::{prelude::*, TransitionSystem, Void};

    #[test_log::test]
    fn normalize_dpa() {
        let mut dpa = NTS::builder()
            .default_color(Void)
            .with_transitions([
                (0, 'a', 2, 0),
                (0, 'b', 1, 1),
                (1, 'a', 0, 0),
                (1, 'b', 1, 1),
            ])
            .collect()
            .into_deterministic()
            .with_initial(0)
            .collect_dpa();
        let normalized = dpa.normalized();
        assert!(normalized.language_equivalent(&dpa));

        for (input, expected) in [("a", 0), ("b", 0), ("ba", 0), ("bb", 1)] {
            assert_eq!(normalized.try_mealy_map(input), Some(expected))
        }
        let n = example_dpa().normalized();
    }

    fn example_dpa() -> DPA {
        NTS::builder()
            .default_color(Void)
            .with_transitions([
                (0, 'a', 0, 0),
                (0, 'b', 1, 1),
                (0, 'c', 2, 2),
                (1, 'a', 3, 2),
                (1, 'b', 4, 2),
                (1, 'c', 7, 1),
                (2, 'a', 2, 0),
                (2, 'b', 5, 0),
                (2, 'c', 6, 0),
            ])
            .into_dpa(0)
    }

    #[test]
    fn dpa_priority_restriction() {
        let dpa = example_dpa();
        assert_eq!(dpa.edges_from(0).unwrap().count(), 3);
        let d05 = dpa.as_ref().edge_color_restricted(0, 5);
        let d13 = dpa.as_ref().edge_color_restricted(1, 3);
        assert_eq!(d05.edges_from(2).unwrap().count(), 2);
        assert_eq!(d13.edges_from(1).unwrap().count(), 1);
        assert_eq!(d13.edges_from(2).unwrap().count(), 1);
        assert_eq!(d13.edges_from(0).unwrap().count(), 2);
    }

    #[test_log::test]
    fn dpa_equivalences() {
        let good = [
            NTS::builder()
                .default_color(())
                .with_transitions([
                    (0, 'a', 0, 1),
                    (0, 'b', 1, 0),
                    (1, 'a', 1, 1),
                    (1, 'b', 0, 0),
                ])
                .deterministic()
                .with_initial(0)
                .collect_dpa(),
            NTS::builder()
                .default_color(())
                .with_transitions([
                    (0, 'a', 5, 1),
                    (0, 'b', 7, 0),
                    (1, 'a', 3, 1),
                    (1, 'b', 2, 2),
                    (2, 'a', 3, 0),
                    (2, 'b', 5, 2),
                ])
                .deterministic()
                .with_initial(0)
                .collect_dpa(),
        ];
        let bad = [
            NTS::builder()
                .default_color(())
                .with_transitions([(0, 'a', 1, 0), (0, 'b', 0, 0)])
                .deterministic()
                .with_initial(0)
                .collect_dpa(),
            NTS::builder()
                .default_color(())
                .with_transitions([(0, 'a', 1, 0), (0, 'b', 2, 0)])
                .deterministic()
                .with_initial(0)
                .collect_dpa(),
            NTS::builder()
                .default_color(())
                .with_transitions([
                    (0, 'a', 4, 1),
                    (0, 'b', 1, 0),
                    (1, 'a', 5, 0),
                    (1, 'b', 3, 1),
                ])
                .deterministic()
                .with_initial(0)
                .collect_dpa(),
        ];

        let l = &good[0];
        let r = &bad[2];
        let prod = l.ts_product(r);
        let sccs = prod.sccs();
        assert!(!good[0].language_equivalent(&bad[2]));

        for g in &good {
            for b in &bad {
                println!("GUT");
                assert!(!g.language_equivalent(b));
            }
        }
    }

    #[test_log::test]
    fn dpa_run() {
        let dpa = NTS::builder()
            .with_transitions([
                (0, 'a', 1, 1),
                (0, 'b', 1, 0),
                (0, 'c', 1, 0),
                (1, 'a', 0, 0),
                (1, 'b', 1, 0),
                (1, 'c', 1, 0),
            ])
            .default_color(Void)
            .into_dpa(0);
        assert!(!dpa.accepts(upw!("cabaca")))
    }

    #[test]
    fn dpa_inclusion() {
        let univ = NTS::builder()
            .default_color(())
            .with_transitions([(0, 'a', 0, 0), (0, 'b', 2, 0)])
            .deterministic()
            .with_initial(0)
            .collect_dpa();
        let aomega = NTS::builder()
            .default_color(())
            .with_transitions([(0, 'a', 0, 0), (0, 'b', 1, 0)])
            .deterministic()
            .with_initial(0)
            .collect_dpa();
        assert!(univ.includes(&aomega));
        assert!(!univ.included_in(&aomega));
    }

    #[test_log::test]
    fn dpa_equivalence_clases() {
        let dpa = NTS::builder()
            .with_transitions([
                (0, 'a', 0, 1),
                (0, 'b', 1, 0),
                (1, 'a', 2, 0),
                (1, 'b', 0, 1),
            ])
            .into_dpa(0);
        let a = (&dpa).with_initial(1).into_dpa();
        assert!(!dpa.language_equivalent(&a));

        let cong = dpa.prefix_congruence().collect_right_congruence_bare();
        assert_eq!(cong.size(), 2);
        assert_eq!(cong.initial(), cong.reached_state_index("aa").unwrap());
        assert!(cong.congruent("", "aa"));
        assert!(cong.congruent("ab", "baaba"));

        let dpa = NTS::builder()
            .with_transitions([
                (0, 'a', 0, 0),
                (0, 'b', 0, 1),
                (1, 'a', 0, 0),
                (1, 'b', 0, 0),
            ])
            .into_dpa(0);
        let cong = dpa.prefix_congruence();
        assert_eq!(cong.size(), 1);
    }
}
