use std::fmt::{Debug, Display};
use std::hash::Hash;

/// Abstracts the concept of length, allowing us to work with finite and infinite words in a
/// somewhat similar fashion.
// TODO: DEPRECATE
pub trait Length: Eq + Ord + Hash + Debug + Display + Copy {
    /// Whether the length is finite or not.
    // FIXME: remove or improve
    const FINITE: bool;
    /// The type of iterator over the set of raw positions.
    type RawPositions: Iterator<Item = RawPosition>;

    /// Heavily used in the computation of a run but most probably deprecated. For
    /// finite words, this method always either returns `None` (if the given `position` is out
    /// of bounds) or `Some(position)`. More interestingly, if the length is infinite, this
    /// method takes the reset point/loop index of the input into account.
    ///
    /// # Examples
    /// If we have an ultimately periodic word w = uv, then the loop index of w is l = |u|, which
    /// is the point to which we reset after going through the loop once. Thus for i < |uv|,
    /// `calculate_raw_position` returns `Some(i)`. Now if `i >= |uv|`, then the method returns
    /// `Some(j)`, where $ j = i + ((i - |u|) \bmod |v|) $. In other words whenever i exceeds or
    /// equals |uv|, we reset the position to the loop index l = |u|, i.e. the first position
    /// after the finite prefix.
    fn calculate_raw_position<P: Into<usize>>(&self, position: P) -> Option<RawPosition>;
    /// Returns `true` if `position` is the last position (wrt. the length defined by `self`).
    /// This method will always return `false` for infinite words and `true` for objects of
    /// [`FiniteLength`] n if `position` is larger or equal to n.
    fn is_end<P: Into<usize>>(&self, position: P) -> bool {
        self.calculate_raw_position(position.into()).is_none()
    }

    /// Remove `offset` from the front.
    fn subtract_front(&self, offset: usize) -> Self;

    /// Add `additional` to the front.
    fn add_front(&self, additional: usize) -> Self;

    /// Returns an iterator over the raw positions.
    fn raw_positions(&self) -> Self::RawPositions;

    /// Returns the last position, if it exists.
    fn last_position(&self) -> Option<usize>;

    /// Returns true if and only if the length is finite.
    fn is_finite() -> bool;

    /// Returns true if and only if the length is infinite.
    fn is_infinite() -> bool {
        !Self::is_finite()
    }
}

/// A position in a word is simply an index into the sequence of symbols (an `usize`). On the other hand
/// a raw position is an index into the sequence of symbols, which takes the reset point/loop index of
/// the input into account. This is only relevant for infinite words. We use raw positions to allow
/// computation of runs for both finite and infinite words in a similar fashion.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, Hash, PartialOrd)]
pub struct RawPosition(usize);

impl RawPosition {
    /// Creates a new [`RawPosition`] object from a given `usize`.
    pub fn new(position: usize) -> Self {
        Self(position)
    }

    /// Obtain the underlying `usize` value.
    pub fn position(&self) -> usize {
        self.0
    }
}

/// Wrapper for things/words that have a finite length, which is simply an `usize`.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct FiniteLength(pub usize);

impl FiniteLength {
    /// Create a new [`FiniteLength`] object.
    pub fn new(length: usize) -> Self {
        Self(length)
    }

    /// Obtain the underlying `usize` value.
    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl std::ops::Add<FiniteLength> for FiniteLength {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl std::ops::Add<InfiniteLength> for FiniteLength {
    type Output = InfiniteLength;

    fn add(self, rhs: InfiniteLength) -> Self::Output {
        InfiniteLength(self.0 + rhs.0, self.0 + rhs.1)
    }
}

impl Length for FiniteLength {
    const FINITE: bool = true;
    type RawPositions = std::iter::Map<std::ops::Range<usize>, fn(usize) -> RawPosition>;

    fn raw_positions(&self) -> Self::RawPositions {
        (0..self.0).map(RawPosition::new)
    }

    fn is_finite() -> bool {
        true
    }
    fn calculate_raw_position<P: Into<usize>>(&self, position: P) -> Option<RawPosition> {
        let position = position.into();
        if position >= self.0 {
            None
        } else {
            Some(RawPosition::new(position))
        }
    }

    fn last_position(&self) -> Option<usize> {
        Some(self.0.saturating_sub(1))
    }

    fn subtract_front(&self, offset: usize) -> Self {
        Self(self.0.saturating_sub(offset))
    }

    fn add_front(&self, additional: usize) -> Self {
        Self(self.0 + additional)
    }
}

impl Display for FiniteLength {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for FiniteLength {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Finite({})", self.0)
    }
}

/// Wrapper type for things of infinite length. The first field is the length of
/// the raw representation, while the second field is the loop index.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct InfiniteLength(pub usize, pub usize);

impl InfiniteLength {
    /// Create a new [`InfiniteLength`] object.
    pub fn new(raw_length: usize, loop_index: usize) -> Self {
        Self(raw_length, loop_index)
    }

    /// Returns the length of the looping part of the word.
    pub fn loop_length(&self) -> usize {
        self.0 - self.1
    }

    /// Returns an iterator over the positions that belong to the looping part.
    pub fn loop_positions(&self) -> impl Iterator<Item = RawPosition> {
        (self.loop_index()..self.0).map(RawPosition::new)
    }

    /// Returns the loop index, which is the position that the looping part resets to.
    /// If you imagine an infinite, ultimately periodic word $ w = uv^\omega $, then the
    /// loop index is simply $ |u| $.
    pub fn loop_index(&self) -> usize {
        self.1
    }

    /// Set the loop index to a new value.
    pub fn set_loop_index(&mut self, loop_index: usize) {
        self.1 = loop_index;
    }

    /// Get the raw index of the last position.
    pub fn last_raw_position(&self) -> usize {
        self.0 - 1
    }

    /// Returns true if the word is periodic, or equivalently the loop index is equal to zero.
    pub fn is_periodic(&self) -> bool {
        self.loop_index() == 0
    }

    /// Returns true if the word is ultimately periodic, but not periodic.
    pub fn is_ultimately_periodic(&self) -> bool {
        self.loop_index() > 0
    }

    /// Auxiliary method which considers `raw` as a word with [`Length`] self and extracts
    /// the prefix part. For a periodic word, this prefix is empty, while for an ultimately
    /// periodic word $ uv^\omega $, it corresponds to $ u $.
    pub fn prefix_of<'a, S>(&self, raw: &'a [S]) -> &'a [S] {
        &raw[..self.1]
    }

    /// Operates similarly to [`Self::prefix_of`], but returns the looping part instead.
    pub fn suffix_of<'a, S>(&self, raw: &'a [S]) -> &'a [S] {
        &raw[self.1..self.0]
    }
}

impl Length for InfiniteLength {
    type RawPositions = std::iter::Map<std::ops::Range<usize>, fn(usize) -> RawPosition>;

    fn raw_positions(&self) -> Self::RawPositions {
        (0..self.0).map(RawPosition::new)
    }

    fn is_finite() -> bool {
        false
    }

    fn last_position(&self) -> Option<usize> {
        None
    }

    fn calculate_raw_position<P: Into<usize>>(&self, position: P) -> Option<RawPosition> {
        let position = position.into();
        if position >= self.0 {
            Some(RawPosition::new(
                self.1 + (position - self.0) % self.loop_length(),
            ))
        } else {
            Some(RawPosition::new(position))
        }
    }

    const FINITE: bool = false;

    fn subtract_front(&self, offset: usize) -> Self {
        if self.1 >= offset {
            Self(self.0 - offset, self.1 - offset)
        } else {
            Self(self.loop_length(), 0)
        }
    }

    fn add_front(&self, additional: usize) -> Self {
        Self(self.0 + additional, self.1 + additional)
    }
}

impl Display for InfiniteLength {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{},{}", self.0, self.1)
    }
}

impl Debug for InfiniteLength {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Infinite({}|{})", self.0, self.1)
    }
}

/// Implementors of this trait have a [`Length`] associated with them, for example words.
#[impl_tools::autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait HasLength {
    /// The type of [`Length`] that is associated with `self`.
    type Length: Length;
    /// Returns the [`Length`] of `self`.
    fn length(&self) -> Self::Length;

    /// Tries to convert a position into a raw position. Returns `None` if the position is out of bounds.
    fn to_raw_position<P: Into<usize>>(&self, position: P) -> Option<RawPosition> {
        self.length().calculate_raw_position(position)
    }

    /// Returns true if the associated [`Length`] is finite.
    fn is_finite(&self) -> bool {
        Self::Length::is_finite()
    }

    /// Returns true if the associated [`Length`] is infinite.
    fn is_infinite(&self) -> bool {
        !self.is_finite()
    }
}
