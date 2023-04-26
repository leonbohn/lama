use crate::{congruence::CongruenceTrigger, Combined, Map, RightCongruence};

mod mapping;
mod with_output;

pub use mapping::{
    Assignment, AssignmentReference, IntoAssigments, Mapping, MutableTransformer, Transformer,
};
pub use with_output::TransitionOutput;

use self::with_output::HasOutput;

/// Type alias to get the output type of a transducer.
pub type OutputOf<T> = <T as HasOutput>::Gamma;

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

    /// Returns the priority as a usize.
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

impl Parity for usize {
    fn parity(&self) -> bool {
        self % 2 == 0
    }
}

impl Parity for bool {
    fn parity(&self) -> bool {
        *self
    }
}

/// A right congruence relation, which has been combined with a mapping from transitions to outputs.
///
/// This encapsulates a Mealy Machine, which is defined over the transition structure of the right congruence.
pub type MealyCongruence<I, O> = Combined<RightCongruence<I>, Map<CongruenceTrigger<I>, O>>;
