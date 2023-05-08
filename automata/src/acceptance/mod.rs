mod acceptor;
pub use acceptor::Acceptor;

mod omega;
pub use omega::{BuchiCondition, OmegaCondition, ParityCondition, ToOmega};
mod reachability;

pub use reachability::{ReachabilityCondition, SafetyAcceptance};

use crate::{
    run::{Induces, InitialRun},
    words::{IsFinite, IsInfinite},
    BuchiAcceptance, FiniteKind, InfiniteKind, ParityAcceptance, Pointed, ReachabilityAcceptance,
    Set, StateIndex, Symbol, Transformer, TransitionSystem, Word, DBA, DFA, DPA,
};

/// Abstracts the finiteness of the type `X`.
pub trait Finite {
    /// The type of an Iterator over the the finite universe of `X`.
    type Elements: Iterator<Item = Self>;
    /// The type of the finite universe of `X`.
    fn universe() -> Self::Elements;

    /// The size of the finite universe of `X`.
    fn size() -> u32 {
        Self::universe().count() as u32
    }
}

impl Finite for u32 {
    type Elements = std::ops::RangeInclusive<u32>;

    fn universe() -> Self::Elements {
        0..=(u32::MAX)
    }
}

/// Can verify for a given induced object whether it satisfies the condition or not.
pub trait AcceptanceCondition {
    /// The type of the induced object, depends on [`Self::Kind`].
    type Induced;

    /// Returns whether the given induced object satisfies the acceptance condition.
    fn is_accepting(&self, induced: &Self::Induced) -> bool;
}

pub trait Acceptance<I> {
    fn is_accepting(&self, induced: &I) -> bool;
}

impl<Q: StateIndex> Acceptance<Q> for ReachabilityAcceptance<Q> {
    fn is_accepting(&self, induced: &Q) -> bool {
        self.apply(induced)
    }
}

impl<Q: StateIndex, S: Symbol> Acceptance<Set<(Q, S)>> for BuchiAcceptance<Q, S> {
    fn is_accepting(&self, induced: &Set<(Q, S)>) -> bool {
        induced.iter().any(|q| self.apply(q))
    }
}

impl<Q: StateIndex, S: Symbol> Acceptance<Set<(Q, S)>> for ParityAcceptance<Q, S> {
    fn is_accepting(&self, induced: &Set<(Q, S)>) -> bool {
        induced
            .iter()
            .map(|q| self.apply(q))
            .min()
            .map(|x| x % 2 == 0)
            .unwrap_or(false)
    }
}
