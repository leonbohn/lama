use crate::prelude::*;

use super::acceptor::OmegaWordAcceptor;

impl_mealy_automaton!(DPA, usize);

/// Trait that should be implemented by every object that can be viewed as a [`crate::DPA`].
pub trait DPALike: TransitionSystem<EdgeColor = usize> + Pointed {
    /// Consumes `self` and returns a [`DPA`] from the transition system underlying `self`.
    fn into_dpa(self) -> IntoDPA<Self> {
        DPA::from(self)
    }

    /// Uses a reference to `self` for creating a [`DPA`] from the underlying transition system.
    fn as_dpa(&self) -> IntoDPA<&Self> {
        DPA::from(self)
    }
}

impl<Ts> DPALike for Ts where Ts: TransitionSystem<EdgeColor = usize> + Pointed {}

impl<A: Alphabet> OmegaWordAcceptor<A::Symbol> for DPA<A> {
    fn accepts_omega<W: OmegaWord<A::Symbol>>(&self, word: W) -> bool {
        self.infinity_set(word)
            .map(|set| set.into_iter().min().unwrap_or(1) % 2 == 0)
            .unwrap_or(false)
    }
}
