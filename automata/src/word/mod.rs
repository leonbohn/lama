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

    fn raw_last(&self) -> Option<Self::Symbol> {
        self.raw_get(RawPosition::new(self.raw_length() - 1))
    }

    /// Constructs a vector of all symbols in the raw representation.
    fn to_vec(&self) -> Vec<Self::Symbol> {
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
    fn loop_back_to(&self, position: RawPosition) -> OmegaWord<Vec<Self::Symbol>, InfiniteLength>
    where
        Self: Sized,
    {
        let loop_index = position.position();
        assert!(loop_index < self.raw_length(), "Loop index out of bounds!");

        let length = InfiniteLength::new(self.raw_length(), loop_index);
        OmegaWord::new(self.to_vec(), length)
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
    const FINITE: bool;

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

    fn first(&self) -> Option<Self::Symbol>
    where
        Self: Sized,
    {
        self.nth(0)
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

    fn to_omega_word(&self) -> OmegaWord<Vec<Self::Symbol>, InfiniteLength> {
        todo!()
    }

    /// Returns an iterator over the symbols making up `self`.
    fn symbols(&self) -> RawpresentationIter<'_, Self::Raw, Self::Length> {
        RawpresentationIter::new(self.rawpresentation(), self.length(), 0)
    }
}

/// Stores the actual representation of a [`Word`] as well as [`Length`], which determines the way
/// that the raw representation is accessed.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct OmegaWord<R, L> {
    raw: R,
    length: L,
}

impl<S: Symbol> OmegaWord<Vec<S>, InfiniteLength> {
    pub fn from_parts(base: Vec<S>, recur: Vec<S>) -> Self {
        let length = InfiniteLength::new(base.len() + recur.len(), base.len());
        OmegaWord::new(base.into_iter().chain(recur).collect(), length)
    }
}

impl<R> OmegaWord<R, InfiniteLength> {
    pub fn initial_segment(&self) -> Vec<R::Symbol>
    where
        R: Rawpresentation,
        R::Symbol: Clone,
    {
        self.raw.to_vec()[..self.length().loop_index()].to_vec()
    }
    pub fn repeating_segment(&self) -> Vec<R::Symbol>
    where
        R: Rawpresentation,
        R::Symbol: Clone,
    {
        self.raw.to_vec()[self.length().loop_index()..].to_vec()
    }
}

impl<R, L> OmegaWord<R, L> {
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
        self.raw.to_vec()
    }
}

impl<R, L: Length> HasLength for OmegaWord<R, L> {
    type Length = L;

    fn length(&self) -> Self::Length {
        self.length
    }
}

impl<R, L> Word for OmegaWord<R, L>
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

    const FINITE: bool = L::FINITE;
}

impl<R: Rawpresentation> Debug for OmegaWord<R, FiniteLength> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            self.raw
                .to_vec()
                .iter()
                .map(|s| format!("{:?}", s))
                .join("")
        ))
    }
}

impl<R: Rawpresentation> Debug for OmegaWord<R, InfiniteLength> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}({})⁰",
            self.initial_segment()
                .iter()
                .map(|s| format!("{:?}", s))
                .join(""),
            self.repeating_segment()
                .iter()
                .map(|s| format!("{:?}", s))
                .join("")
        ))
    }
}

impl<R, L> Display for OmegaWord<R, L>
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

    const FINITE: bool = true;
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

    const FINITE: bool = true;
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

    const FINITE: bool = true;
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

    const FINITE: bool = true;
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

/// This macro can be used to create a [`OmegaWord`] object from some representation, it is mainly interesting
/// for quickly constructing infinite words without having to go through the [`OmegaWord`] struct.
///
/// There are essentially three distinct variants of using this macro:
/// - `upw!(repr, loopindex index)` creates a word with the given representation and the given loopindex.
/// - `upw!(base, recur)` creates an ultimately word with the representation of `base` followed by the representation of `recur`.
/// - `upw!(recur)` creates a periodic word that is the repetition of `recur`.
#[macro_export]
macro_rules! upw {
    ($repr:expr, loopindex $index:expr) => {
        $crate::word::OmegaWord::new_reverse_args(
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
        word::{OmegaWord, Rawpresentation, Word},
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

        let word = OmegaWord::new(raw, FiniteLength::new(4));
        assert_eq!(word.nth(1), Some('b'));
        assert_eq!(word.nth(4), None);

        let infinite = word.rawpresentation().loop_back_to(RawPosition::new(2));
    }
}
