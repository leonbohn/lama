use crate::{congruence::CongruenceTrigger, Combined, Map, RightCongruence};

mod mapping;
mod transducer;

pub use mapping::{Mapping, MutableMapping, PriorityMapping};
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

    pub fn as_usize(&self) -> usize {
        self.0 as usize
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

pub type MealyCongruence<I, O> = Combined<RightCongruence<I>, Map<CongruenceTrigger<I>, O>>;
