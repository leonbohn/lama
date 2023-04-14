use crate::Symbol;

use super::{PeriodicWord, Str, UltimatelyPeriodicWord, Word};

/// Encapsulates the ability to prepend something to a [`Word`].
pub trait Prepend<W: Word>: Word {
    /// The type of the resulting word.
    type Output: Word<S = Self::S>;

    /// Prepends `other` to `self`, producing an object of type `Self::Output`.
    fn prepend(&self, other: &W) -> Self::Output;
}

impl<S: Symbol> Prepend<Str<S>> for Str<S> {
    type Output = Str<S>;

    fn prepend(&self, other: &Str<S>) -> Self::Output {
        other
            .symbols
            .iter()
            .chain(self.symbols.iter())
            .cloned()
            .collect()
    }
}

impl<S: Symbol> Prepend<Str<S>> for PeriodicWord<S> {
    type Output = UltimatelyPeriodicWord<S>;

    fn prepend(&self, other: &Str<S>) -> Self::Output {
        (other.clone(), self.clone()).into()
    }
}

impl<S: Symbol> Prepend<Str<S>> for UltimatelyPeriodicWord<S> {
    type Output = UltimatelyPeriodicWord<S>;

    fn prepend(&self, other: &Str<S>) -> Self::Output {
        (self.0.prepend(other), self.1.clone()).into()
    }
}
