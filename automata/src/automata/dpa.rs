use std::collections::VecDeque;

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
