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

    pub fn priority_restricted(&self, min: usize, max: usize) -> IntoDPA<PriorityRestricted<&Ts>> {
        PriorityRestricted::new(&self.ts, min, max).into_dpa()
    }
}

#[derive(Clone, Debug)]
pub struct PriorityRestricted<D> {
    ts: D,
    min: usize,
    max: usize,
}

impl<D: TransitionSystem<EdgeColor = usize> + Pointed> Pointed for PriorityRestricted<D> {
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

impl<D: TransitionSystem<EdgeColor = usize>> HasAlphabet for PriorityRestricted<D> {
    type Alphabet = D::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts().alphabet()
    }
}

impl<D: TransitionSystem<EdgeColor = usize>> TransitionSystem for PriorityRestricted<D> {
    type StateIndex = D::StateIndex;

    type StateColor = D::StateColor;

    type EdgeColor = D::EdgeColor;

    type TransitionRef<'this> = D::TransitionRef<'this>
    where
        Self: 'this;

    type EdgesFromIter<'this> = PriorityRestrictedEdgesFrom<'this, D>    where
        Self: 'this;

    type StateIndices<'this> = D::StateIndices<'this>
    where
        Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts().state_indices()
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        let min = self.min;
        let max = self.max;
        Some(PriorityRestrictedEdgesFrom {
            min,
            max,
            _phantom: PhantomData,
            it: self.ts().edges_from(state.to_index(self)?)?,
        })
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        self.ts().state_color(state)
    }
}

impl<D: PredecessorIterable<EdgeColor = usize>> PredecessorIterable for PriorityRestricted<D> {
    type PreTransitionRef<'this> = D::PreTransitionRef<'this>
    where
        Self: 'this;

    type EdgesToIter<'this> = PriorityRestrictedEdgesTo<'this, D>
    where
        Self: 'this;

    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        todo!()
    }
}

impl<D: DPALike> Deterministic for PriorityRestricted<D> {
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        self.ts()
            .transition(state.to_index(self)?, symbol)
            .and_then(|t| {
                if t.color() <= self.max && self.min <= t.color() {
                    Some(t)
                } else {
                    None
                }
            })
    }
}

pub struct PriorityRestrictedEdgesFrom<'a, D: TransitionSystem> {
    _phantom: PhantomData<&'a D>,
    it: D::EdgesFromIter<'a>,
    min: usize,
    max: usize,
}

impl<'a, D: TransitionSystem<EdgeColor = usize>> Iterator for PriorityRestrictedEdgesFrom<'a, D> {
    type Item = D::TransitionRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it
            .find(|t| t.color() <= self.max && self.min <= t.color())
    }
}

pub struct PriorityRestrictedEdgesTo<'a, D: PredecessorIterable> {
    _phantom: PhantomData<&'a D>,
    it: D::EdgesToIter<'a>,
    min: usize,
    max: usize,
}

impl<'a, D: PredecessorIterable<EdgeColor = usize>> Iterator for PriorityRestrictedEdgesTo<'a, D> {
    type Item = D::PreTransitionRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it
            .find(|t| t.color() <= self.max && self.min <= t.color())
    }
}

impl<D: TransitionSystem<EdgeColor = usize>> PriorityRestricted<D> {
    pub fn ts(&self) -> &D {
        &self.ts
    }
    pub fn new(ts: D, min: usize, max: usize) -> Self {
        Self { ts, min, max }
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
        let d05 = dpa.priority_restricted(0, 5);
        let d13 = dpa.priority_restricted(1, 3);
        assert_eq!(d05.edges_from(2).unwrap().count(), 2);
        assert_eq!(d13.edges_from(1).unwrap().count(), 1);
        assert_eq!(d13.edges_from(2).unwrap().count(), 1);
        assert_eq!(d13.edges_from(0).unwrap().count(), 2);
    }
}
