use crate::words::FiniteWord;

/// Encapsulates the kind of a sequence, differentiating between finite and infinite ones.
pub trait Boundedness {
    fn is_finite() -> bool;
}

#[derive(Clone, PartialEq, Eq, std::hash::Hash, std::fmt::Debug)]
/// A marker type which indicates that a sequence finite.
pub struct FiniteKind();

#[derive(Clone, PartialEq, Eq, std::hash::Hash, std::fmt::Debug)]
/// A marker type which indicates that a sequence is infinite.
pub struct InfiniteKind();

impl Boundedness for FiniteKind {
    fn is_finite() -> bool {
        true
    }
}

impl Boundedness for InfiniteKind {
    fn is_finite() -> bool {
        false
    }
}

impl<S> Boundedness for FiniteWord<S> {
    fn is_finite() -> bool {
        true
    }
}
