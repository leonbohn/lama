use std::fmt::Debug;

use crate::{Boundedness, FiniteKind, Symbol};
mod append;
pub use append::Append;

mod prepend;
use impl_tools::autoimpl;
pub use prepend::Prepend;

mod finite;
mod infinite;
mod subword;

pub use finite::Str;
pub use infinite::{PeriodicWord, UltimatelyPeriodicWord};
pub use subword::Subword;

/// Abstracts a word over some given alphabet. The type parameter `S` is the alphabet, and `Kind` is a marker type which indicates whether the word is finite or infinite.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait Word: Debug + Eq + std::hash::Hash {
    /// The type of the symbols making up the word.
    type S: Symbol;

    /// Indicates whether the word is finite or infinite.
    type Kind: Boundedness;

    /// Returns the symbol at the given index, or `None` if the index is out of bounds.
    fn nth(&self, index: usize) -> Option<Self::S>;
}

/// A trait which indicates that a word is finite.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait IsFinite: Word {
    /// Returns the length of the word.
    fn length(&self) -> usize;
}

/// Marker trait for infinite words, assumes the implementor is finitely representable as an ultimately periodic word, i.e. a base word followed by an infinitely looping non-empty word.
pub trait IsInfinite: Word {
    /// Returns the length of the base word.
    fn base_length(&self) -> usize;
    /// Returns the length of the recurring word.
    fn recur_length(&self) -> usize;
}

impl<F: IsInfinite> IsInfinite for &F {
    fn base_length(&self) -> usize {
        IsInfinite::base_length(*self)
    }

    fn recur_length(&self) -> usize {
        IsInfinite::recur_length(*self)
    }
}

/// A trait which allows iterating over the symbols of a word. For an infinite word, this is an infinite iterator.
pub trait SymbolIterable: Word {
    /// The iterator type.
    type Iter: Iterator<Item = Self::S>;

    /// Returns an iterator over the symbols of the word.
    fn iter(&self) -> Self::Iter;
}

impl Word for String {
    type Kind = FiniteKind;
    type S = char;

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.chars().nth(index)
    }
}

impl IsFinite for String {
    fn length(&self) -> usize {
        self.len()
    }
}

impl Word for &str {
    type Kind = FiniteKind;
    type S = char;

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.chars().nth(index)
    }
}

impl IsFinite for &str {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<S: Symbol> IsFinite for Vec<S> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<S: Symbol> Word for Vec<S> {
    type Kind = FiniteKind;
    type S = S;

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.get(index).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn symbol_iterability() {
        let word = Str::<usize>::from(vec![1, 3, 3, 7]);
        let mut iter = word.iter();
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(7));
        assert_eq!(iter.next(), None);

        let word = UltimatelyPeriodicWord(Str::empty(), PeriodicWord::from(vec![1, 3, 3, 7]));
        let mut iter = word.iter();
        assert_eq!(iter.next(), Some(1usize));
    }
}
