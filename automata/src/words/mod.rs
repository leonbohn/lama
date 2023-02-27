use crate::{Boundedness, FiniteKind, InfiniteKind};
mod finite;
mod infinite;
mod subword;

pub use finite::FiniteWord;
pub use infinite::{PeriodicWord, UltimatelyPeriodicWord};
pub use subword::Subword;

/// Abstracts a word over some given alphabet. The type parameter `S` is the alphabet, and `Kind` is a marker type which indicates whether the word is finite or infinite.
pub trait Word {
    /// The type of the symbols making up the word.
    type S: Clone;
    /// The kind of the word, either [`FiniteKind`] or [`InfiniteKind`].
    type Kind: Boundedness;

    /// Returns the symbol at the given index, or `None` if the index is out of bounds.
    fn nth(&self, index: usize) -> Option<Self::S>;
}

/// A trait which allows iterating over the symbols of a word. For an infinite word, this is an infinite iterator.
pub trait SymbolIterable: Word {
    /// The iterator type.
    type Iter: Iterator<Item = Self::S>;

    /// Returns an iterator over the symbols of the word.
    fn iter(&self) -> Self::Iter;
}

/// Helper type representing finite words over the alphabet `S`.
pub type InSigmaStar<S> = dyn Word<S = S, Kind = FiniteKind>;
/// Helper type representing infinite words over the alphabet `S`.
pub type InSigmaOmega<S> = dyn Word<S = S, Kind = InfiniteKind>;

impl Word for String {
    type S = char;

    type Kind = FiniteKind;

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.chars().nth(index)
    }
}

impl Word for &str {
    type S = char;

    type Kind = FiniteKind;

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.chars().nth(index)
    }
}

impl<S: Clone> Word for Vec<S> {
    type S = S;

    type Kind = FiniteKind;

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.get(index).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn symbol_iterability() {
        let word = FiniteWord::<usize>::from(vec![1, 3, 3, 7]);
        let mut iter = word.iter();
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(7));
        assert_eq!(iter.next(), None);

        let word =
            UltimatelyPeriodicWord(FiniteWord::empty(), PeriodicWord::from(vec![1, 3, 3, 7]));
        let mut iter = word.iter();
        assert_eq!(iter.next(), Some(1usize));
    }
}
