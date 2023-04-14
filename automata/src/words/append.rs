use crate::Symbol;

use super::{PeriodicWord, Str, UltimatelyPeriodicWord, Word};

/// Encapsulates the ability to append some [`Word`] to another.
pub trait Append<W: Word>: Word {
    /// The type of the resulting word.
    type Output: Word<S = Self::S>;

    /// Appends `other` to `self`, producing an object of type `Self::Output`.
    fn append(&self, other: &W) -> Self::Output;
}

impl<S: Symbol> Append<Str<S>> for Str<S> {
    type Output = Str<S>;

    fn append(&self, other: &Str<S>) -> Self::Output {
        self.symbols
            .iter()
            .chain(other.symbols.iter())
            .cloned()
            .collect()
    }
}

impl<S: Symbol> Append<PeriodicWord<S>> for Str<S> {
    type Output = UltimatelyPeriodicWord<S>;

    fn append(&self, other: &PeriodicWord<S>) -> Self::Output {
        (self.clone(), other.clone()).into()
    }
}

impl<S: Symbol> Append<UltimatelyPeriodicWord<S>> for Str<S> {
    type Output = UltimatelyPeriodicWord<S>;

    fn append(&self, other: &UltimatelyPeriodicWord<S>) -> Self::Output {
        (self.append(&other.0), other.1.clone()).into()
    }
}
