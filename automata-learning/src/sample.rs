use std::hash::Hash;

use automata::{words::IsInfinite, FiniteKind, Set, Word};

/// Represents a finite sample, which is a pair of positive and negative instances.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Sample<W> {
    pub positive: Set<W>,
    pub negative: Set<W>,
}

impl<W: Eq + Hash> PartialEq for Sample<W> {
    fn eq(&self, other: &Self) -> bool {
        self.positive == other.positive && self.negative == other.negative
    }
}

impl<W: Eq + Hash> Eq for Sample<W> {}

impl<W: IsInfinite> Sample<W> {
    /// Returns the maximum length of the base prefix of any word in the sample.
    pub fn max_base_len(&self) -> usize {
        self.iter().map(|w| w.base_length()).max().unwrap_or(0)
    }

    /// Returns the maximum loop length of any word in the sample.
    pub fn max_recur_len(&self) -> usize {
        self.iter().map(|w| w.recur_length()).max().unwrap_or(0)
    }
}

impl<W: Eq + Hash> Sample<W> {
    /// Creates a new sample from the given data.
    pub fn from_parts(positive: Set<W>, negative: Set<W>) -> Self {
        Self { positive, negative }
    }

    /// Returns the number of elements in the sample.
    pub fn len(&self) -> usize {
        self.positive.len() + self.negative.len()
    }

    /// Checks if the sample is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Creates a new sample from two iterators.
    pub fn from_iters<I, J>(positive: I, negative: J) -> Self
    where
        I: IntoIterator<Item = W>,
        J: IntoIterator<Item = W>,
    {
        Self {
            positive: positive.into_iter().collect(),
            negative: negative.into_iter().collect(),
        }
    }

    /// Iterates over all elements in the sample.
    pub fn iter(&self) -> impl Iterator<Item = &W> {
        self.positive.iter().chain(self.negative.iter())
    }

    /// Iterates just over the positive instances.
    pub fn positive_iter(&self) -> impl Iterator<Item = &W> {
        self.positive.iter()
    }

    /// Iterates just over the negative instances.
    pub fn negative_iter(&self) -> impl Iterator<Item = &W> {
        self.negative.iter()
    }
}

impl<W: Word<Kind = FiniteKind>> Sample<W> {}
