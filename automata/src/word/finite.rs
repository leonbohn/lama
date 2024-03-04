use std::{collections::VecDeque, fmt::Write};

use impl_tools::autoimpl;
use itertools::Itertools;

use crate::{prelude::Symbol, Show};

use super::{omega::ReducedOmegaWord, Concat, LinearWord, PeriodicOmegaWord};

/// A finite word is a [`LinearWord`] that has a finite length.
pub trait FiniteWord<S>: LinearWord<S> {
    /// Type for an iterator over the symbols making up the word.
    type Symbols<'this>: Iterator<Item = S>
    where
        Self: 'this;

    /// Returns an iterator over the symbols of the word.
    fn symbols(&self) -> Self::Symbols<'_>;

    /// Appends the given [`LinearWord`] to the end of this word. Note, that the appended
    /// suffix may be finite or infinite.
    fn append<W: LinearWord<S>>(self, suffix: W) -> Concat<Self, W>
    where
        Self: Sized,
    {
        Concat(self, suffix)
    }

    /// Checks if the given word is equal to this word. Note, that this operation only makes sense
    /// when both words are finite.
    fn equals<W: FiniteWord<S>>(&self, other: W) -> bool
    where
        S: Eq,
    {
        self.len() == other.len() && self.symbols().zip(other.symbols()).all(|(a, b)| a == b)
    }

    /// Prepends the given `prefix` to the beginning of this word. This operation only works if
    /// the prefix is finite.
    fn prepend<W: FiniteWord<S>>(self, prefix: W) -> Concat<W, Self>
    where
        Self: Sized,
    {
        Concat(prefix, self)
    }

    /// Collects the symbols making up `self` into a vector.
    fn to_vec(&self) -> Vec<S> {
        self.symbols().collect()
    }

    /// Collects the symbols making up `self` into a [`VecDeque`].
    fn to_deque(&self) -> VecDeque<S> {
        VecDeque::from(self.to_vec())
    }

    /// Builds the [`PeriodicOmegaWord`] word that is the omega power of this word, i.e. if
    /// `self` is the word `u`, then the result is the word `u^ω` = `u u u u ...`.
    /// Panics if `self` is empty as the operation is not defined in that case.
    fn omega_power(&self) -> PeriodicOmegaWord<S>
    where
        S: Symbol,
    {
        assert!(
            !self.is_empty(),
            "Omega iteration of an empty word is undefined!"
        );
        PeriodicOmegaWord::new(self)
    }

    /// Gives the length of the word, i.e. the number of symbols.
    fn len(&self) -> usize {
        self.symbols().count()
    }

    /// Returns the `n`-th symbol of the word from the back.
    ///
    /// # Example
    /// ```
    /// use automata::prelude::*;
    /// let word = "abc";
    ///
    /// assert_eq!(word.nth_back(0), Some('c'));
    /// assert_eq!(word.nth_back(1), Some('b'));
    /// assert_eq!(word.nth_back(2), Some('a'));
    /// ```
    fn nth_back(&self, pos: usize) -> Option<S> {
        self.nth(self.len() - pos - 1)
    }

    /// Returns `true` if the word is empty, i.e. has no symbols.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Converts the word to a string. This is only possible if the the symbols implement
    /// the [`Show`] trait.
    fn as_string(&self) -> String
    where
        S: Show,
    {
        let out = self.symbols().map(|a| a.show()).join("");
        if out.is_empty() {
            "ε".into()
        } else {
            out
        }
    }
}

impl<S: Symbol, const N: usize> FiniteWord<S> for [S; N] {
    type Symbols<'this> = std::iter::Cloned<std::slice::Iter<'this, S>>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.iter().cloned()
    }
}

impl<S: Symbol, const N: usize> LinearWord<S> for [S; N] {
    fn nth(&self, position: usize) -> Option<S> {
        self.get(position).cloned()
    }
}

impl<S: Symbol, Fw: FiniteWord<S> + ?Sized> FiniteWord<S> for &Fw {
    type Symbols<'this> = Fw::Symbols<'this> where Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        (*self).symbols()
    }

    fn len(&self) -> usize {
        (*self).len()
    }
    fn to_vec(&self) -> Vec<S> {
        (*self).to_vec()
    }
}

impl<S: Symbol> LinearWord<S> for VecDeque<S> {
    fn nth(&self, position: usize) -> Option<S> {
        if position < self.len() {
            Some(self[position])
        } else {
            None
        }
    }
}

impl<S: Symbol> FiniteWord<S> for VecDeque<S> {
    type Symbols<'this> = std::iter::Cloned<std::collections::vec_deque::Iter<'this, S>>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.iter().cloned()
    }
}

impl LinearWord<char> for str {
    fn nth(&self, position: usize) -> Option<char> {
        self.chars().nth(position)
    }
}

impl FiniteWord<char> for str {
    type Symbols<'this> = std::str::Chars<'this>;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.chars()
    }

    fn len(&self) -> usize {
        self.len()
    }
    fn to_vec(&self) -> Vec<char> {
        self.chars().collect()
    }
}

impl LinearWord<char> for String {
    fn nth(&self, position: usize) -> Option<char> {
        self.chars().nth(position)
    }
}

impl FiniteWord<char> for String {
    fn to_vec(&self) -> Vec<char> {
        self.chars().collect()
    }

    fn len(&self) -> usize {
        self.len()
    }

    type Symbols<'this> = std::str::Chars<'this>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.chars()
    }
}

impl<S: Symbol> LinearWord<S> for Vec<S> {
    fn nth(&self, position: usize) -> Option<S> {
        if position < self.len() {
            Some(self[position])
        } else {
            None
        }
    }
}
impl<S: Symbol> FiniteWord<S> for Vec<S> {
    type Symbols<'this> = std::iter::Cloned<std::slice::Iter<'this, S>>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.iter().cloned()
    }

    fn to_vec(&self) -> Vec<S> {
        self.clone()
    }

    fn len(&self) -> usize {
        self.len()
    }
}

impl<S: Symbol> LinearWord<S> for [S] {
    fn nth(&self, position: usize) -> Option<S> {
        if position < self.len() {
            Some(self[position])
        } else {
            None
        }
    }
}
impl<S: Symbol> FiniteWord<S> for [S] {
    type Symbols<'this> = std::iter::Cloned<std::slice::Iter<'this, S>>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.iter().cloned()
    }

    fn to_vec(&self) -> Vec<S> {
        self.to_vec()
    }

    fn len(&self) -> usize {
        self.len()
    }
}
