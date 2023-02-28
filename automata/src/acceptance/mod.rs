use crate::{Boundedness, FiniteKind, InfiniteKind, Set};

mod omega;
mod reachability;

pub use reachability::{ReachabilityAcceptance, SafetyAcceptance};

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

/// Holds the priority (i.e. label) of either a transition or of a state.
/// The type C is the label, which by default is a u32.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Priority(pub u32);

pub trait Parity {
    fn parity(&self) -> bool;
}

impl Parity for Priority {
    fn parity(&self) -> bool {
        self.0 % 2 == 0
    }
}

pub trait PriorityMapping {
    type X;

    fn priority(&self, of: &Self::X) -> Priority;

    fn universe(&self) -> Set<Priority>;

    fn complexity(&self) -> u32;
}

/// Can verify for a given induced object whether it satisfies the condition or not.
/// The type `K` is the type of the input, which is either finite or infinite.
pub trait AcceptanceCondition {
    /// The type of the induced object, depends on [`Self::Kind`].
    type Induced;

    /// Returns whether the given induced object satisfies the acceptance condition.
    fn is_accepting(&self, induced: &Self::Induced) -> bool;
}

impl<PM: PriorityMapping> AcceptanceCondition for PM {
    type Induced = PM::X;

    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        self.priority(induced).parity()
    }
}
