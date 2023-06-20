use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::Deref;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct RawPosition(usize);

impl RawPosition {
    pub fn new(position: usize) -> Self {
        Self(position)
    }

    pub fn position(&self) -> usize {
        self.0
    }
}

macro_rules! rawpos {
    ($position:expr) => {
        RawPosition::new($position)
    };
}

/// Abstracts the concept of length, allowing us to work with finite and infinite words in a
/// somewhat similar fashion.
pub trait Length: Eq + Ord + Hash + Debug + Display + Copy {
    /// Returns true if the length is finite.
    fn is_finite(&self) -> bool;
    /// Heavily used in the computation of a run (which is done by [`crate::run::Cane`]). For
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
}

/// Wrapper for things/words that have a finite length, which is simply an `usize`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct FiniteLength(pub usize);

impl FiniteLength {
    /// Create a new [`FiniteLength`] object.
    pub fn new(length: usize) -> Self {
        Self(length)
    }
}

impl Length for FiniteLength {
    fn is_finite(&self) -> bool {
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
}

impl Display for FiniteLength {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Wrapper type for things of infinite length. The first field is the length of
/// the raw representation, while the second field is the loop index.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
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
}

impl Length for InfiniteLength {
    fn is_finite(&self) -> bool {
        false
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
}

impl InfiniteLength {
    /// Returns the loop index, which is the position that the looping part resets to.
    /// If you imagine an infinite, ultimately periodic word $ w = uv^\omega $, then the
    /// loop index is simply $ |u| $.
    pub fn loop_index(&self) -> usize {
        self.1
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

    /// Operates similarly to [`prefix_of`], but returns the looping part instead.
    pub fn suffix_of<'a, S>(&self, raw: &'a [S]) -> &'a [S] {
        &raw[self.1..self.0]
    }
}
impl Display for InfiniteLength {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{},{}", self.0, self.1)
    }
}

/// Implementors of this trait have a [`Length`] associated with them, for example words.
pub trait HasLength {
    /// The type of [`Length`] that is associated with `self`.
    type Len: Length;
    /// Returns the [`Length`] of `self`.
    fn length(&self) -> Self::Len;

    fn to_raw_position<P: Into<usize>>(&self, position: P) -> Option<RawPosition> {
        self.length().calculate_raw_position(position)
    }
}

impl<HL: HasLength> HasLength for &HL {
    type Len = HL::Len;
    fn length(&self) -> Self::Len {
        HL::length(*self)
    }
}

impl<HL: HasLength> HasLength for &mut HL {
    type Len = HL::Len;
    fn length(&self) -> Self::Len {
        HL::length(*self)
    }
}
