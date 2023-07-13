use std::{
    borrow::Cow,
    fmt::{Debug, Display},
};

use crate::{
    alphabet::{Alphabet, Symbol},
    length::{HasLength, RawPosition},
    FiniteLength, InfiniteLength, Length,
};
use itertools::Itertools;

mod subword;
pub use subword::{Prefix, Suffix};

/// Encapsulates the raw representation of a [`Word`], which is essentially just a sequence of [`Symbol`]s
/// which can be accessed by a [`RawPosition`] and whose length can be queried.
pub trait Rawpresentation: ToOwned<Owned = Self> {
    /// The type of symbol that is stored in the raw representation.
    type Symbol: Symbol;

    /// Returns the symbol at the given [`RawPosition`], if it exists.
    fn raw_get(&self, position: RawPosition) -> Option<Self::Symbol>;

    /// Returns the length of the raw representation.
    fn raw_length(&self) -> usize;

    /// Creates an infinite word from `self`, which loops back to the given `position`. Assumes that the
    /// given position is actually a valid position in the word.
    ///
    /// # Example
    /// If `self` is the word `a b c`, then `self.loop_back_to(1)` is the infinite word `abc bc bc ...`.
    /// If `self` is `abba` and we run `self.loop_back_to(2)`, then we get `abba ba ba ba ba...`.
    fn loop_back_to(&self, position: RawPosition) -> RawWithLength<Self, InfiniteLength>
    where
        Self: Sized,
    {
        let loop_index = position.position();
        assert!(loop_index < self.raw_length(), "Loop index out of bounds!");

        let length = InfiniteLength::new(self.raw_length(), loop_index);
        RawWithLength::new(self.to_owned(), length)
    }
}

impl<S: Symbol> Rawpresentation for Vec<S> {
    type Symbol = S;
    fn raw_get(&self, position: RawPosition) -> Option<Self::Symbol> {
        self.get(position.position()).cloned()
    }
    fn raw_length(&self) -> usize {
        self.len()
    }
}

impl<'a> Rawpresentation for &'a str {
    type Symbol = char;
    fn raw_get(&self, position: RawPosition) -> Option<Self::Symbol> {
        self.chars().nth(position.position())
    }
    fn raw_length(&self) -> usize {
        self.len()
    }
}

impl<'a, S: Symbol> Rawpresentation for Cow<'a, [S]> {
    type Symbol = S;
    fn raw_get(&self, position: RawPosition) -> Option<Self::Symbol> {
        self.get(position.position()).cloned()
    }
    fn raw_length(&self) -> usize {
        self.len()
    }
}

/// A word is a sequence of symbols which can be accessed positionally. This trait tries to fully abstract
/// away whether a word is finite or infinite, by relying on raw positions.
pub trait Word: HasLength {
    /// The type of raw representation that is used to store the symbols of this word.
    type Raw: Rawpresentation;
    /// The type of symbol that is stored in this word.
    type Symbol: Symbol;

    /// Accesses the symbol at the given position, if it exists.
    fn get(&self, position: usize) -> Option<Self::Symbol>;

    /// Returns a reference to the raw representation of `self`.
    fn rawpresentation(&self) -> &Self::Raw;

    /// Constructs a [`Prefix`] object, which is a finite prefix of `self` that has the given `length`.
    fn prefix(&self, length: usize) -> Prefix<'_, Self>
    where
        Self: Sized,
    {
        Prefix::new(self, length)
    }

    /// Creates a [`Suffix`] object, which is the suffix of `self` that starts at the given `offset`.
    fn suffix(&self, offset: usize) -> subword::Suffix<'_, Self>
    where
        Self: Sized,
    {
        subword::Suffix::new(self, offset)
    }

    /// Returns an iterator over the symbols making up `self`.
    fn symbols(&self) -> RawpresentationIter<'_, Self::Raw, Self::Length> {
        RawpresentationIter::new(self.rawpresentation(), self.length(), 0)
    }
}

/// Stores the actual representation of a [`Word`] as well as [`Length`], which determines the way
/// that the raw representation is accessed.
#[derive(Clone, PartialEq)]
pub struct RawWithLength<R, L> {
    raw: R,
    length: L,
}

impl<R, L> RawWithLength<R, L> {
    /// Create a new
    pub fn new(raw: R, length: L) -> Self
    where
        R: Rawpresentation,
        L: Length,
    {
        Self { raw, length }
    }
}

impl<R, L: Length> HasLength for RawWithLength<R, L> {
    type Length = L;

    fn length(&self) -> Self::Length {
        self.length
    }
}

impl<R, L> Word for RawWithLength<R, L>
where
    R: Rawpresentation,
    L: Length,
{
    type Raw = R;
    type Symbol = R::Symbol;

    fn get(&self, position: usize) -> Option<R::Symbol> {
        let raw_position = self.length.calculate_raw_position(position)?;
        self.raw.raw_get(raw_position)
    }
    fn rawpresentation(&self) -> &Self::Raw {
        &self.raw
    }
}

impl<R: Debug, L: Length> Debug for RawWithLength<R, L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}, {})", self.raw, self.length)
    }
}

impl<R, L> Display for RawWithLength<R, L>
where
    R: Rawpresentation,
    R::Symbol: Display,
    L: Length,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = self
            .length
            .raw_positions()
            .map(|p| self.raw.raw_get(p).unwrap())
            .join("");
        write!(f, "(\"{}\", {})", repr, self.length)
    }
}

impl HasLength for &str {
    type Length = FiniteLength;

    fn length(&self) -> Self::Length {
        FiniteLength::new(self.len())
    }
}

impl Word for &str {
    type Raw = Self;

    type Symbol = char;

    fn get(&self, position: usize) -> Option<Self::Symbol> {
        self.chars().nth(position)
    }

    fn rawpresentation(&self) -> &Self::Raw {
        self
    }
}

/// An iterator over the raw representation of a word. It stores a reference to the [`Rawpresentation`],
/// the [`Length`] of the word and the current position.
#[derive(Debug, Clone, PartialEq)]
pub struct RawpresentationIter<'a, R, L> {
    raw: &'a R,
    length: L,
    position: usize,
}

impl<'a, R, L> Iterator for RawpresentationIter<'a, R, L>
where
    R: Rawpresentation,
    L: Length,
{
    type Item = R::Symbol;

    fn next(&mut self) -> Option<Self::Item> {
        let symbol = self
            .raw
            .raw_get(self.length.calculate_raw_position(self.position)?)?;
        self.position += 1;
        Some(symbol)
    }
}

impl<'a, R, L> RawpresentationIter<'a, R, L>
where
    R: Rawpresentation,
    L: Length,
{
    /// Creates a new iterator over the raw representation of a word.
    pub fn new(raw: &'a R, length: L, position: usize) -> Self {
        Self {
            raw,
            length,
            position,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        length::RawPosition,
        word::{RawWithLength, Rawpresentation, Word},
        FiniteLength,
    };

    #[test]
    fn raw_representation() {
        let raw = vec!['a', 'b', 'a', 'b'];
        assert_eq!(raw.raw_get(RawPosition::new(0)), Some('a'));

        let word = RawWithLength::new(raw, FiniteLength::new(4));
        assert_eq!(word.get(1), Some('b'));
        assert_eq!(word.get(4), None);

        let infinite = word.rawpresentation().loop_back_to(RawPosition::new(2));
    }
}
