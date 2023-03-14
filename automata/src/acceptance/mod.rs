use crate::{Boundedness, Set};

mod omega;
pub use omega::{BuchiCondition, ParityCondition};
mod reachability;

pub use reachability::{ReachabilityCondition, SafetyAcceptance};

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
#[derive(Debug, Clone, Eq, Ord, PartialOrd, PartialEq, Hash)]
pub struct Priority(pub u32);

/// A trait for types that induce a parity, which is either true or false.
pub trait Parity {
    /// Returns true if the parity is even, false if it is odd.
    fn parity(&self) -> bool;
}

impl Parity for Priority {
    fn parity(&self) -> bool {
        self.0 % 2 == 0
    }
}

/// A mapping from a type `X` to a [`Priority`].
pub trait PriorityMapping {
    /// The domain of the mapping.
    type X;

    /// Obtains the priority of the given element.
    fn priority(&self, of: &Self::X) -> Priority;

    /// Returns the set of all priorities.
    fn universe(&self) -> Set<Priority>;

    /// Returns the number of distinct priorities.
    fn complexity(&self) -> u32;
}

/// Can verify for a given induced object whether it satisfies the condition or not.
pub trait AcceptanceCondition {
    /// The type of the induced object, depends on [`Self::Kind`].
    type Induced;

    /// Returns whether the given induced object satisfies the acceptance condition.
    fn is_accepting(&self, induced: &Self::Induced) -> bool;
}
