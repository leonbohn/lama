use std::{collections::VecDeque, marker::PhantomData};

use crate::{
    prelude::*,
    ts::{
        operations::{MapEdgeColor, MapStateColor},
        IntoInitialBTS,
    },
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

    fn collect_dpa(self) -> IntoDPA<IntoInitialBTS<Self>> {
        DPA::from(self.trim_collect())
    }

    fn prefix_congruence(&self) -> RightCongruence<Self::Alphabet> {
        let mut states: VecDeque<_> = self.state_indices().collect();

        while let Some(q) = states.pop_front() {}
        todo!()
    }

    fn witness_colors<O: DPALike<Alphabet = Self::Alphabet>>(
        self,
        k: usize,
        other: IntoDPA<O>,
        l: usize,
    ) -> Option<Reduced<SymbolOf<Self>>> {
        todo!()
    }
}

impl<Ts> DPALike for Ts where Ts: Deterministic<EdgeColor = usize> + Pointed {}

impl<Ts: DPALike<EdgeColor = usize>> OmegaWordAcceptor<SymbolOf<Ts>>
    for DPA<Ts::Alphabet, Ts::StateColor, Ts>
{
    fn accepts_omega<W: OmegaWord<SymbolOf<Ts>>>(&self, word: W) -> bool {
        self.infinity_set(word)
            .map(|set| set.into_iter().min().unwrap_or(1) % 2 == 0)
            .unwrap_or(false)
    }
}

impl<Ts: DPALike> AsRef<IntoDPA<Ts>> for IntoDPA<Ts> {
    fn as_ref(&self) -> &IntoDPA<Ts> {
        self
    }
}

impl<Ts: DPALike> DPA<Ts::Alphabet, Ts::StateColor, Ts> {
    pub fn give_word(&self) -> Option<Reduced<SymbolOf<Self>>> {
        todo!()
    }

    pub fn complement(self) -> DPA<Ts::Alphabet, Ts::StateColor> {
        self.map_edge_colors(|c| c + 1).collect_dpa()
    }

    pub fn intersection<O: DPALike<Alphabet = Ts::Alphabet>>(
        self,
        other: IntoDPA<O>,
    ) -> DPA<Ts::Alphabet, (Ts::StateColor, O::StateColor)> {
        self.ts
            .ts_product(other.ts)
            .map_edge_colors(|(c, d)| std::cmp::min(c, d))
            .collect_dpa()
    }

    pub fn equivalent<O: DPALike<Alphabet = Ts::Alphabet>>(self, other: IntoDPA<O>) -> bool {
        self.intersection(other.complement()).give_word().is_none()
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
            .extend([
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
    fn simple_dpa_equivalence() {
        let good = [
            NTS::builder()
                .default_color(())
                .extend([
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
                .extend([
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
                .extend([(0, 'a', 1, 0), (0, 'b', 0, 0)])
                .deterministic()
                .with_initial(0)
                .collect_dpa(),
            NTS::builder()
                .default_color(())
                .extend([(0, 'a', 1, 0), (0, 'b', 2, 0)])
                .deterministic()
                .with_initial(0)
                .collect_dpa(),
            NTS::builder()
                .default_color(())
                .extend([
                    (0, 'a', 4, 1),
                    (0, 'b', 1, 0),
                    (1, 'a', 5, 0),
                    (1, 'b', 3, 1),
                ])
                .deterministic()
                .with_initial(0)
                .collect_dpa(),
        ];
    }
}
