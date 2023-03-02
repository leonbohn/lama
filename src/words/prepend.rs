use crate::Symbol;

use super::{FiniteWord, PeriodicWord, UltimatelyPeriodicWord, Word};

/// Encapsulates the ability to prepend something to a [`Word`].
pub trait Prepend<W: Word>: Word {
    /// The type of the resulting word.
    type Output: Word<S = Self::S>;

    /// Prepends `other` to `self`, producing an object of type `Self::Output`.
    fn prepend(&self, other: &W) -> Self::Output;
}

impl<S: Symbol> Prepend<FiniteWord<S>> for FiniteWord<S> {
    type Output = FiniteWord<S>;

    fn prepend(&self, other: &FiniteWord<S>) -> Self::Output {
        other
            .symbols
            .iter()
            .chain(self.symbols.iter())
            .cloned()
            .collect()
    }
}

impl<S: Symbol> Prepend<FiniteWord<S>> for PeriodicWord<S> {
    type Output = UltimatelyPeriodicWord<S>;

    fn prepend(&self, other: &FiniteWord<S>) -> Self::Output {
        (other.clone(), self.clone()).into()
    }
}

impl<S: Symbol> Prepend<FiniteWord<S>> for UltimatelyPeriodicWord<S> {
    type Output = UltimatelyPeriodicWord<S>;

    fn prepend(&self, other: &FiniteWord<S>) -> Self::Output {
        (self.0.prepend(other), self.1.clone()).into()
    }
}
