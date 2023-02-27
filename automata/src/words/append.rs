use crate::Alphabet;

use super::{FiniteWord, PeriodicWord, UltimatelyPeriodicWord, Word};

/// Encapsulates the ability to append some [`Word`] to a `Word`.
pub trait Append<W: Word>: Word {
    /// The type of the resulting word.
    type Output: Word<S = Self::S>;

    /// Appends `other` to `self`, producing an object of type `Self::Output`.
    fn append(&self, other: &W) -> Self::Output;
}

impl<S: Alphabet> Append<FiniteWord<S>> for FiniteWord<S> {
    type Output = FiniteWord<S>;

    fn append(&self, other: &FiniteWord<S>) -> Self::Output {
        self.symbols
            .iter()
            .chain(other.symbols.iter())
            .cloned()
            .collect()
    }
}

impl<S: Alphabet> Append<PeriodicWord<S>> for FiniteWord<S> {
    type Output = UltimatelyPeriodicWord<S>;

    fn append(&self, other: &PeriodicWord<S>) -> Self::Output {
        (self.clone(), other.clone()).into()
    }
}

impl<S: Alphabet> Append<UltimatelyPeriodicWord<S>> for FiniteWord<S> {
    type Output = UltimatelyPeriodicWord<S>;

    fn append(&self, other: &UltimatelyPeriodicWord<S>) -> Self::Output {
        (self.append(&other.0), other.1.clone()).into()
    }
}
