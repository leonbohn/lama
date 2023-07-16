use std::{
    borrow::Cow,
    fmt::{Debug, Display},
};

use crate::{
    alphabet::{Alphabet, Symbol},
    length::{HasLength, RawPosition},
    FiniteLength, InfiniteLength, Length,
};
use impl_tools::autoimpl;
use itertools::Itertools;

mod subword;
pub use subword::{Prefix, Suffix};

mod concat;
pub use concat::Concat;

/// Encapsulates the raw representation of a [`Word`], which is essentially just a sequence of [`Symbol`]s
/// which can be accessed by a [`RawPosition`] and whose length can be queried.
pub trait Rawpresentation {
    /// The type of symbol that is stored in the raw representation.
    type Symbol: Symbol;

    /// Returns the symbol at the given [`RawPosition`], if it exists.
    fn raw_get(&self, position: RawPosition) -> Option<Self::Symbol>;

    /// Returns the length of the raw representation.
    fn raw_length(&self) -> usize;

    /// Constructs a vector of all symbols in the raw representation.
    fn raw_symbols(&self) -> Vec<Self::Symbol> {
        (0..self.raw_length())
            .map(|i| self.raw_get(RawPosition::new(i)).unwrap())
            .collect()
    }

    /// Creates an infinite word from `self`, which loops back to the given `position`. Assumes that the
    /// given position is actually a valid position in the word.
    ///
    /// # Example
    /// If `self` is the word `a b c`, then `self.loop_back_to(1)` is the infinite word `abc bc bc ...`.
    /// If `self` is `abba` and we run `self.loop_back_to(2)`, then we get `abba ba ba ba ba...`.
    fn loop_back_to(
        &self,
        position: RawPosition,
    ) -> RawWithLength<Vec<Self::Symbol>, InfiniteLength>
    where
        Self: Sized,
    {
        let loop_index = position.position();
        assert!(loop_index < self.raw_length(), "Loop index out of bounds!");

        let length = InfiniteLength::new(self.raw_length(), loop_index);
        RawWithLength::new(self.raw_symbols(), length)
    }
}

impl<S: Symbol> Rawpresentation for Vec<S> {
    type Symbol = S;
    fn raw_get(&self, position: RawPosition) -> Option<Self::Symbol> {
        self.nth(position.position())
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
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait Word: HasLength {
    /// The type of raw representation that is used to store the symbols of this word.
    type Raw: Rawpresentation<Symbol = Self::Symbol>;
    /// The type of symbol that is stored in this word.
    type Symbol: Symbol;

    /// Accesses the symbol at the given position, if it exists.
    fn nth(&self, position: usize) -> Option<Self::Symbol>;

    /// Returns a reference to the raw representation of `self`.
    fn rawpresentation(&self) -> &Self::Raw;

    /// Constructs a [`Prefix`] object, which is a finite prefix of `self` that has the given `length`.
    fn prefix(&self, length: usize) -> Prefix<'_, Self>
    where
        Self: Sized,
    {
        Prefix::new(self, length)
    }

    fn concat<W: Word<Symbol = Self::Symbol>>(self, other: W) -> Concat<Self, W>
    where
        Self: Sized + HasLength<Length = FiniteLength>,
    {
        Concat::new(self, other)
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
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RawWithLength<R, L> {
    raw: R,
    length: L,
}

impl<R, L> RawWithLength<R, L> {
    /// Create a new `RawWithLength` object from the given raw representation and length.
    pub fn new(raw: R, length: L) -> Self
    where
        R: Rawpresentation,
        L: Length,
    {
        Self { raw, length }
    }

    /// Does the same as [`RawWithLength::new`], but reverses the arguments.
    pub fn new_reverse_args(length: L, raw: R) -> Self
    where
        R: Rawpresentation,
        L: Length,
    {
        Self { raw, length }
    }

    pub fn raw_as_vec(self) -> Vec<R::Symbol>
    where
        R: Rawpresentation,
        L: Length,
        R::Symbol: Clone,
    {
        self.raw.raw_symbols()
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

    fn nth(&self, position: usize) -> Option<R::Symbol> {
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

    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        self.chars().nth(position)
    }

    fn rawpresentation(&self) -> &Self::Raw {
        self
    }
}

impl<S: Symbol> HasLength for Vec<S> {
    type Length = FiniteLength;
    fn length(&self) -> Self::Length {
        FiniteLength::new(self.len())
    }
}

impl<S: Symbol> Word for Vec<S> {
    type Raw = Self;

    type Symbol = S;

    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        self.get(position).copied()
    }

    fn rawpresentation(&self) -> &Self::Raw {
        self
    }
}

impl<S: Symbol> HasLength for &[S] {
    type Length = FiniteLength;
    fn length(&self) -> Self::Length {
        FiniteLength::new(self.len())
    }
}
impl<S: Symbol> Rawpresentation for &[S] {
    type Symbol = S;

    fn raw_get(&self, position: RawPosition) -> Option<Self::Symbol> {
        self.nth(position.position())
    }

    fn raw_length(&self) -> usize {
        self.len()
    }
}
impl<S: Symbol> Word for &[S] {
    type Raw = Self;
    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        self.get(position).copied()
    }

    type Symbol = S;

    #[doc = " Returns a reference to the raw representation of `self`."]
    fn rawpresentation(&self) -> &Self::Raw {
        self
    }
}

#[derive(Debug, Clone, Eq, Copy, PartialEq, Hash)]
pub struct Letter<S: Symbol>(pub S);
impl<S: Symbol> Rawpresentation for Letter<S> {
    type Symbol = S;
    fn raw_get(&self, position: RawPosition) -> Option<Self::Symbol> {
        if position.position() == 0 {
            Some(self.0)
        } else {
            None
        }
    }
    fn raw_length(&self) -> usize {
        1
    }
}
impl<S: Symbol> Word for Letter<S> {
    type Raw = Self;
    type Symbol = S;
    fn rawpresentation(&self) -> &Self::Raw {
        self
    }
    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        if position == 0 {
            Some(self.0)
        } else {
            None
        }
    }
}
impl<S: Symbol> HasLength for Letter<S> {
    type Length = FiniteLength;
    fn length(&self) -> Self::Length {
        FiniteLength::new(1)
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

/// This macro can be used to create a [`RawWithLength`] object from some representation, it is mainly interesting
/// for quickly constructing infinite words without having to go through the [`RawWithLength`] struct.
///
/// There are essentially three distinct variants of using this macro:
/// - `upw!(repr, loopindex index)` creates a word with the given representation and the given loopindex.
/// - `upw!(base, recur)` creates an ultimately word with the representation of `base` followed by the representation of `recur`.
/// - `upw!(recur)` creates a periodic word that is the repetition of `recur`.
#[macro_export]
macro_rules! upw {
    ($repr:expr, loopindex $index:expr) => {
        $crate::word::RawWithLength::new_reverse_args(
            $crate::InfiniteLength::new($repr.len(), $index),
            $repr,
        )
    };
    ($recur:expr) => {
        upw!($recur.chars().collect::<Vec<_>>(), loopindex 0)
    };
    ($base:expr, $recur:expr) => {
        upw!($base.chars()
            .chain($recur.chars())
            .collect::<Vec<_>>(), loopindex $base.len())
    };
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{
        length::RawPosition,
        word::{RawWithLength, Rawpresentation, Word},
        FiniteLength,
    };

    #[test]
    fn macro_upw() {
        let w = upw!("ab", loopindex 1);
        let ww = upw!("a", "b");
        assert!(w.symbols().zip(ww.symbols()).take(20).all(|(a, b)| a == b));
    }

    #[test]
    fn raw_representation() {
        let raw = vec!['a', 'b', 'a', 'b'];
        assert_eq!(raw.raw_get(RawPosition::new(0)), Some('a'));

        let word = RawWithLength::new(raw, FiniteLength::new(4));
        assert_eq!(word.nth(1), Some('b'));
        assert_eq!(word.nth(4), None);

        let infinite = word.rawpresentation().loop_back_to(RawPosition::new(2));
    }
}
