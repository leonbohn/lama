use crate::{
    congruence::CongruenceTrigger, Combined, Deterministic, Mapping, RightCongruence, Set,
};

mod transducer;

pub use transducer::Transducer;

/// Type alias to get the output type of a transducer.
pub type OutputOf<T> = <T as Transducer>::Output;

/// Holds the priority (i.e. label) of either a transition or of a state.
/// The type C is the label, which by default is a u32.
#[derive(Debug, Clone, Eq, Ord, PartialOrd, PartialEq, Hash)]
pub struct Priority(pub u32);

impl Priority {
    /// Creates a new priority from the given number.
    pub fn new(number: u32) -> Self {
        Self(number)
    }

    /// Returns the number representing the priority.
    pub fn number(&self) -> u32 {
        self.0
    }
}

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
    type Domain;

    /// Obtains the priority of the given element.
    fn priority(&self, of: &Self::Domain) -> Priority;

    /// Returns the set of all priorities.
    fn universe(&self) -> Set<Priority>;

    /// Returns the number of distinct priorities.
    fn complexity(&self) -> u32;
}

/// A mutable mapping from a type `X` to a [`Priority`].
pub trait MutableMapping: PriorityMapping {
    /// Sets the priority of the given element to the given priority.
    fn set_priority(&mut self, of: &Self::Domain, to: Priority) -> Option<Priority>;
}

pub type MealyCongruence<I, O> = Combined<RightCongruence<I>, Mapping<CongruenceTrigger<I>, O>>;
