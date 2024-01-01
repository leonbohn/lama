use std::{collections::VecDeque, marker::PhantomData};

use crate::{
    prelude::*,
    ts::{
        connected_components::Scc,
        operations::{MapEdgeColor, MapStateColor},
        IntoInitialBTS, Quotient,
    },
    Parity, Partition,
};

use super::acceptor::OmegaWordAcceptor;

impl_mealy_automaton!(DPA, usize);

/// Trait that should be implemented by every object that can be viewed as a [`crate::DPA`].
pub trait DPALike: Deterministic<EdgeColor = usize> + Pointed {
    /// Consumes `self` and returns a [`DPA`] from the transition system underlying `self`.
    fn into_dpa(self) -> IntoDPA<Self> {
        DPA::from(self)
    }

    /// Uses a reference to `self` for creating a [`DPA`] from the underlying transition system.
    fn as_dpa(&self) -> IntoDPA<&Self> {
        DPA::from(self)
    }

    fn collect_dpa(
        self,
    ) -> IntoDPA<WithInitial<DTS<Self::Alphabet, Self::StateColor, Self::EdgeColor>>> {
        DPA::from(self.trim_collect())
    }
}

impl<Ts> DPALike for Ts where Ts: Deterministic<EdgeColor = usize> + Pointed {}

impl<Ts: DPALike<EdgeColor = usize>> OmegaWordAcceptor<SymbolOf<Ts>>
    for DPA<Ts::Alphabet, Ts::StateColor, Ts>
{
    fn accepts_omega<W: OmegaWord<SymbolOf<Ts>>>(&self, word: W) -> bool {
        self.recurrent_edge_colors(word)
            .map(|set| set.into_iter().min().unwrap_or(1) % 2 == 0)
            .unwrap_or(false)
    }
}

impl<Ts: DPALike> AsRef<IntoDPA<Ts>> for IntoDPA<Ts> {
    fn as_ref(&self) -> &IntoDPA<Ts> {
        self
    }
}

impl<D: DPALike> IntoDPA<D> {
    pub fn give_word(&self) -> Option<Reduced<SymbolOf<Self>>> {
        todo!()
    }

    pub fn complement(self) -> DPA<D::Alphabet, D::StateColor> {
        self.map_edge_colors(|c| c + 1).collect_dpa()
    }

    pub fn separate<X, Y>(&self, left: X, right: Y) -> Option<Reduced<SymbolOf<Self>>>
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

    pub fn prefix_congruence(&self) -> Quotient<&Self> {
        let mut it = self.reachable_state_indices();
        let fst = it.next();
        assert_eq!(fst, Some(self.initial()));
        let mut partition = vec![vec![fst.unwrap()]];
        let mut queue: VecDeque<_> = it.collect();
        let expected_size = queue.len() + 1;

        'outer: while let Some(q) = queue.pop_front() {
            for i in 0..partition.len() {
                let p = partition[i]
                    .first()
                    .expect("Class of partition must be non-empty");
                if self
                    .as_ref()
                    .with_initial(p)
                    .as_dpa()
                    .language_equivalent(&self.as_ref().with_initial(q).as_dpa())
                {
                    partition.get_mut(i).unwrap().push(q);
                    continue 'outer;
                }
            }
            partition.push(vec![q]);
        }
        debug_assert_eq!(
            partition.iter().fold(0, |acc, x| acc + x.len()),
            expected_size,
            "size mismatch!"
        );

        self.quotient(Partition::new(partition))
    }

    pub fn witness_color(&self, color: D::EdgeColor) -> Option<Reduced<SymbolOf<Self>>> {
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
                    .maximal_loop_from(*q)
                    .expect("This thing is non-transient");
                return Some(Reduced::ultimately_periodic(rep, cycle));
            }
        }
        None
    }

    pub fn witness_colors<O: DPALike<Alphabet = D::Alphabet>>(
        &self,
        k: usize,
        other: &IntoDPA<O>,
        l: usize,
    ) -> Option<Reduced<SymbolOf<Self>>> {
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
                    .maximal_loop_from(*mr)
                    .expect("This thing is non-transient");
                return Some(Reduced::ultimately_periodic(spoke, cycle));
            }
        }
        None
    }

    pub fn colors(&self) -> impl Iterator<Item = D::EdgeColor> + '_ {
        MealyLike::color_range(self)
    }

    pub fn witness_inequivalence<O: DPALike<Alphabet = D::Alphabet>>(
        &self,
        other: &IntoDPA<O>,
    ) -> Option<Reduced<SymbolOf<D>>> {
        self.witness_not_subset_of(other)
            .or(other.witness_not_subset_of(self))
    }

    pub fn language_equivalent<O: DPALike<Alphabet = D::Alphabet>>(
        &self,
        other: &IntoDPA<O>,
    ) -> bool {
        self.witness_inequivalence(other).is_none()
    }

    pub fn included_in<O: DPALike<Alphabet = D::Alphabet>>(&self, other: &IntoDPA<O>) -> bool {
        self.witness_not_subset_of(other).is_none()
    }

    pub fn includes<O: DPALike<Alphabet = D::Alphabet>>(&self, other: &IntoDPA<O>) -> bool {
        other.witness_not_subset_of(self).is_none()
    }

    pub fn witness_not_subset_of<O: DPALike<Alphabet = D::Alphabet>>(
        &self,
        other: &IntoDPA<O>,
    ) -> Option<Reduced<SymbolOf<D>>> {
        for i in self.colors().filter(|x| x.is_even()) {
            for j in other.colors().filter(|x| x.is_odd()) {
                if let Some(cex) = self.as_ref().witness_colors(i, &other, j) {
                    return Some(cex);
                }
            }
        }
        None
    }

    pub fn normalize(&mut self) {
        let mut ts = self.collect_with_initial();

        let mut recoloring = Vec::new();
        let mut remove_states = Vec::new();
        let mut remove_edges = Vec::new();

        let mut priority = 0;
        'outer: loop {
            let dag = ts.tarjan_dag();
            for edge in dag.transient_edges() {
                recoloring.push(((edge.source(), edge.expression()), priority));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{prelude::*, TransitionSystem};
    #[cfg(test)]
    pub use pretty_assertions::{assert_eq, assert_ne};

    use super::DPA;

    fn example_dpa() -> DPA {
        NTS::builder()
            .default_color(())
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
            .collect()
            .into_deterministic()
            .with_initial(0)
            .collect_dpa()
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

    #[test]
    fn dpa_equivalence() {
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
        for g in &good {
            for b in &bad {
                assert!(!g.language_equivalent(b));
            }
        }
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

    #[test]
    fn dpa_equivalence_clases() {
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
        let dpa = NTS::builder()
            .with_transitions([
                (0, 'a', 0, 1),
                (0, 'b', 1, 0),
                (1, 'a', 2, 0),
                (1, 'b', 0, 1),
            ])
            .into_dpa(0);
        let cong = dpa.prefix_congruence().collect_right_congruence();
        assert_eq!(cong.size(), 2);
        assert_eq!(cong.initial(), cong.reached("aa").unwrap());
        assert!(cong.congruent("", "aa"));
        assert!(cong.congruent("ab", "baaba"));
    }
}
