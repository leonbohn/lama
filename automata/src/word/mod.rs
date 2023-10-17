use std::fmt::{Debug, Display};

use crate::{alphabet::Symbol, length::HasLength, FiniteLength, InfiniteLength, Length};
use impl_tools::autoimpl;
use itertools::Itertools;

mod subword;
pub use subword::{Offset, Prefix};

mod concat;
pub use concat::Concat;

#[macro_use]
mod normalized;
pub use normalized::{Normalized, NormalizedParseError, NormalizedPeriodic};

/// Allows conversion into a vector of symbols.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait RawSymbols<S> {
    /// Converts self into a vector of symbols.
    fn to_vec(&self) -> Vec<S>;
}

impl RawSymbols<char> for &str {
    fn to_vec(&self) -> Vec<char> {
        self.chars().collect()
    }
}

impl RawSymbols<char> for str {
    fn to_vec(&self) -> Vec<char> {
        self.chars().collect()
    }
}
impl RawSymbols<char> for char {
    fn to_vec(&self) -> Vec<char> {
        vec![*self]
    }
}
impl<S: Symbol> RawSymbols<S> for Vec<S> {
    fn to_vec(&self) -> Vec<S> {
        self.clone()
    }
}

impl<S: Symbol> RawSymbols<S> for [S] {
    fn to_vec(&self) -> Vec<S> {
        self.to_vec()
    }
}

/// A word is a sequence of symbols which can be accessed positionally. This trait tries to fully abstract
/// away whether a word is finite or infinite, by relying on raw positions.
#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait Word: HasLength {
    /// The type of symbol that is stored in this word.
    type Symbol: Symbol;

    /// Accesses the symbol at the given position, if it exists.
    fn nth(&self, position: usize) -> Option<Self::Symbol>;

    /// Constructs a [`Prefix`] object, which is a finite prefix of `self` that has the given `length`.
    fn prefix(&self, length: usize) -> Prefix<'_, Self>
    where
        Self: Sized,
    {
        Prefix::new(self, length)
    }

    /// Returns the first symbol of `self`, if it exists.
    fn first(&self) -> Option<Self::Symbol>
    where
        Self: Sized,
    {
        self.nth(0)
    }

    /// Constructs a [`NormalizedPeriodic`] object, which is the normalized periodic word that is
    /// represented by iterating `self` over and over again.
    fn omega_power(&self) -> NormalizedPeriodic<Self::Symbol> {
        NormalizedPeriodic::new(self.raw_to_vec())
    }

    /// Constructs a [`Concat`] object, which is the concatenation of `self` and `other`.
    fn append<W: Word<Symbol = Self::Symbol>>(self, other: W) -> Concat<Self, W>
    where
        Self: Sized + HasLength<Length = FiniteLength>,
    {
        Concat::new(self, other)
    }

    /// Normalizes `self` and returns the result.
    fn normalized(&self) -> Normalized<Self::Symbol, Self::Length> {
        Normalized::new(self.raw_to_vec(), self.length())
    }

    /// Removes the first symbol of `self` and returns it together with the remaining suffix.
    fn pop_first(&self) -> (Self::Symbol, subword::Offset<'_, Self>)
    where
        Self: Sized + HasLength<Length = InfiniteLength>,
    {
        let first = self.first().unwrap();
        (first, self.offset(1))
    }

    /// Creates an [`subword::Offset`] object, which is the suffix of `self` that starts at the given `offset`.
    fn offset(&self, offset: usize) -> subword::Offset<'_, Self>
    where
        Self: Sized,
    {
        subword::Offset::new(self, offset)
    }

    /// Returns the raw representation of `self` as a vector of symbols.
    fn finite_to_vec(&self) -> Vec<Self::Symbol>
    where
        Self: HasLength<Length = FiniteLength>,
    {
        self.raw_to_vec()
    }

    /// Returns an iterator over the symbols making up `self`.
    fn raw_to_vec(&self) -> Vec<Self::Symbol> {
        self.length()
            .raw_positions()
            .map(|p| self.nth(p.position()).unwrap())
            .collect()
    }
}

/// Stores the actual representation of a [`Word`] as well as [`Length`], which determines the way
/// that the raw representation is accessed.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct OmegaWord<S, L> {
    raw: Vec<S>,
    length: L,
}

impl<S: Symbol> OmegaWord<S, InfiniteLength> {
    /// Creates a new ultimately periodic word from the given base and repeating segment.
    pub fn from_parts(base: Vec<S>, recur: Vec<S>) -> Self {
        let length = InfiniteLength::new(base.len() + recur.len(), base.len());
        OmegaWord::new(base.into_iter().chain(recur).collect_vec(), length)
    }
}

impl<S> OmegaWord<S, InfiniteLength> {
    /// Returns the initial segment of the word, i.e. the part that is not repeating.
    pub fn initial_segment(&self) -> Vec<S>
    where
        S: Clone,
    {
        self.raw[..self.length().loop_index()].to_vec()
    }

    /// Returns the repeating segment of the word, i.e. the part that is repeating.
    pub fn repeating_segment(&self) -> Vec<S>
    where
        S: Clone,
    {
        self.raw[self.length().loop_index()..].to_vec()
    }
}

impl<S, L> OmegaWord<S, L> {
    /// Create a new `RawWithLength` object from the given raw representation and length.
    pub fn new<R: RawSymbols<S>>(raw: R, length: L) -> Self
    where
        L: Length,
    {
        Self {
            raw: raw.to_vec(),
            length,
        }
    }

    /// Does the same as [`Self::new()`], but reverses the arguments.
    pub fn new_reverse_args<R: RawSymbols<S>>(length: L, raw: R) -> Self
    where
        L: Length,
    {
        Self::new(raw, length)
    }

    /// Turns `self` into a vector of symbols.
    pub fn to_vec(self) -> Vec<S>
    where
        L: Length,
        S: Clone,
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
    L: Length,
    R: Symbol,
{
    type Symbol = R;

    fn nth(&self, position: usize) -> Option<R> {
        let raw_position = self.length.calculate_raw_position(position)?;
        self.raw.get(raw_position.position()).cloned()
    }
}

impl<R: Symbol> Debug for OmegaWord<R, FiniteLength> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            self.raw.iter().map(|s| format!("{:?}", s)).join("")
        ))
    }
}

impl<S: Symbol> Debug for OmegaWord<S, InfiniteLength> {
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
    R: Symbol + Display,
    L: Length,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = self
            .length
            .raw_positions()
            .map(|p| self.raw.get(p.position()).unwrap())
            .join("");
        write!(f, "(\"{}\", {})", repr, self.length)
    }
}

impl HasLength for str {
    type Length = FiniteLength;

    fn length(&self) -> Self::Length {
        FiniteLength::new(self.len())
    }
}

impl Word for str {
    type Symbol = char;

    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        self.chars().nth(position)
    }
}

impl<S: Symbol> HasLength for Vec<S> {
    type Length = FiniteLength;
    fn length(&self) -> Self::Length {
        FiniteLength::new(self.len())
    }
}

impl<S: Symbol> Word for Vec<S> {
    type Symbol = S;

    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        self.get(position).copied()
    }
}

impl<S: Symbol> HasLength for &[S] {
    type Length = FiniteLength;
    fn length(&self) -> Self::Length {
        FiniteLength::new(self.len())
    }
}
impl<S: Symbol> Word for &[S] {
    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        self.get(position).copied()
    }

    type Symbol = S;
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

    use crate::word::Word;

    #[test]
    fn macro_upw() {
        let w = upw!("ab", loopindex 1);
        let ww = upw!("a", "b");
        assert_eq!(w.prefix(6).finite_to_vec(), ww.prefix(6).finite_to_vec());
    }
}
