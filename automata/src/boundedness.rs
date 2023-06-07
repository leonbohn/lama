use crate::{
    ts::{Path, StateOf, TransitionOf},
    Set, Successor,
};

/// Encapsulates the kind of a sequence, differentiating between finite and infinite ones.
pub trait Boundedness {
    type Induces<TS: Successor>: Eq;

    /// Returns whether the object is finite or not.
    fn is_finite() -> bool;

    fn induces_from_path<TS: Successor>(path: Path<TS::Q, TS::Sigma>) -> Self::Induces<TS>;
}

#[derive(Clone, PartialEq, Eq, std::hash::Hash, std::fmt::Debug)]
/// A marker type which indicates that a sequence finite.
pub struct FiniteKind();

#[derive(Clone, PartialEq, Eq, std::hash::Hash, std::fmt::Debug)]
/// A marker type which indicates that a sequence is infinite.
pub struct InfiniteKind();

impl Boundedness for FiniteKind {
    type Induces<TS: Successor> = StateOf<TS>;

    fn induces_from_path<TS: Successor>(path: Path<TS::Q, TS::Sigma>) -> Self::Induces<TS> {
        path.reached().clone()
    }

    fn is_finite() -> bool {
        true
    }
}

impl Boundedness for InfiniteKind {
    type Induces<TS: Successor> = Set<TransitionOf<TS>>;

    fn induces_from_path<TS: Successor>(path: Path<TS::Q, TS::Sigma>) -> Self::Induces<TS> {
        path.infinity_set()
    }

    fn is_finite() -> bool {
        false
    }
}
