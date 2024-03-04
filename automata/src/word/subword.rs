use std::marker::PhantomData;

use crate::{length::HasLength, prelude::Symbol, FiniteLength, Length};

use super::{omega::PeriodicOmegaWord, ConsumingInfixIterator, FiniteWord, LinearWord, OmegaWord};

/// A suffix of a [`LinearWord`] which skips a fixed number of symbols. If the underlying
/// word is infinite, the suffix is also infinite. If the underlying word is finite, the suffix
/// is also finite.
#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub struct Offset<'a, S, W: LinearWord<S>> {
    sequence: &'a W,
    offset: usize,
    _marker: std::marker::PhantomData<S>,
}

impl<'a, S, W: LinearWord<S>> Offset<'a, S, W> {
    /// Creates a new suffix, which skips the first `offset` symbols of the given sequence.
    pub fn new(sequence: &'a W, offset: usize) -> Self {
        Self {
            sequence,
            offset,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<'a, S: Symbol, W: LinearWord<S>> LinearWord<S> for Offset<'a, S, W> {
    fn nth(&self, position: usize) -> Option<S> {
        self.sequence.nth(self.offset + position)
    }
}

impl<'a, S: Symbol, W: FiniteWord<S>> FiniteWord<S> for Offset<'a, S, W> {
    type Symbols<'this> = std::iter::Skip<W::Symbols<'this>> where Self: 'this;

    fn to_vec(&self) -> Vec<S> {
        (self.offset..self.sequence.len())
            .map(|position| self.sequence.nth(position).unwrap())
            .collect()
    }

    fn len(&self) -> usize {
        self.sequence.len().saturating_sub(self.offset)
    }

    fn symbols(&self) -> Self::Symbols<'_> {
        self.sequence.symbols().skip(self.offset)
    }
}

#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub struct Rotated<W>(pub W, pub usize);

impl<S: Symbol, W: FiniteWord<S>> LinearWord<S> for Rotated<W> {
    fn nth(&self, position: usize) -> Option<S> {
        self.0.nth((position + self.1) % self.0.len())
    }
}

pub struct RotatedIter<'a, S, W> {
    rotated: &'a Rotated<W>,
    start: usize,
    position: usize,
    _pd: PhantomData<S>,
}

impl<'a, S, W> RotatedIter<'a, S, W> {
    pub fn new(rotated: &'a Rotated<W>, start: usize) -> Self {
        Self {
            rotated,
            start,
            position: 0,
            _pd: PhantomData,
        }
    }
}

impl<'a, S: Symbol, W: FiniteWord<S>> Iterator for RotatedIter<'a, S, W> {
    type Item = S;
    fn next(&mut self) -> Option<Self::Item> {
        if self.position < self.rotated.len() {
            let out = self
                .rotated
                .nth((self.start + self.position) % self.rotated.len());
            assert!(out.is_some());
            self.position += 1;
            out
        } else {
            None
        }
    }
}

impl<S: Symbol, W: FiniteWord<S>> FiniteWord<S> for Rotated<W> {
    type Symbols<'this> = RotatedIter<'this, S, W> where Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        RotatedIter::new(self, self.1)
    }

    fn to_vec(&self) -> Vec<S> {
        self.symbols().collect()
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'a, S: Symbol, W: OmegaWord<S>> OmegaWord<S> for Offset<'a, S, W> {
    type Spoke<'this> = Infix<'this, S, W>
    where
        Self: 'this;

    type Cycle<'this> = Infix<'this, S, W>
    where
        Self: 'this;

    fn spoke(&self) -> Self::Spoke<'_> {
        if self.offset < self.sequence.loop_index() {
            self.sequence
                .infix(self.offset, (self.sequence.loop_index() - self.offset))
        } else {
            self.sequence.infix(self.sequence.loop_index(), 0)
        }
    }

    fn cycle(&self) -> Self::Cycle<'_> {
        if self.offset < self.sequence.loop_index() {
            self.sequence
                .infix(self.sequence.loop_index(), self.sequence.cycle_length())
        } else {
            self.sequence.infix(
                (self.sequence.loop_index()
                    + (self.offset.saturating_sub(self.sequence.loop_index())
                        % self.sequence.cycle_length())),
                self.sequence.cycle_length(),
            )
        }
    }
}

/// Represents an infix of a [`LinearWord`]. This is a finite word, which is a subsequence of the
/// original word. It is specified by a starting position and a length, and stores a reference
/// to the underlying word.
#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub struct Infix<'a, S, W: LinearWord<S> + ?Sized> {
    sequence: &'a W,
    offset: usize,
    length: usize,
    _marker: std::marker::PhantomData<S>,
}

impl<'a, S, W: LinearWord<S> + ?Sized> Infix<'a, S, W> {
    /// Creates a new suffix, which skips the first `offset` symbols of the given sequence.
    pub fn new(sequence: &'a W, offset: usize, length: usize) -> Self {
        Self {
            sequence,
            offset,
            length,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<'a, S: Symbol, W: LinearWord<S>> LinearWord<S> for Infix<'a, S, W> {
    fn nth(&self, position: usize) -> Option<S> {
        if position < self.length {
            self.sequence.nth(self.offset + position)
        } else {
            None
        }
    }
}

impl<'a, S: Symbol, W: LinearWord<S>> FiniteWord<S> for Infix<'a, S, W> {
    type Symbols<'this> = ConsumingInfixIterator<'this, S, W>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        ConsumingInfixIterator::new(self.sequence, self.offset, self.offset + self.length)
    }

    fn to_vec(&self) -> Vec<S> {
        (self.offset..(self.offset + self.length))
            .map(|position| self.sequence.nth(position).unwrap())
            .collect()
    }

    fn len(&self) -> usize {
        self.length
    }
}

#[cfg(test)]
mod tests {

    use itertools::Itertools;

    use crate::{
        upw,
        word::{FiniteWord, LinearWord, OmegaWord, ReducedOmegaWord},
        FiniteLength,
    };

    #[test]
    fn finite_word_infix() {
        let fw = "abcde".to_string();
        assert_eq!(fw.infix(1, 3).to_vec(), vec!['b', 'c', 'd']);
        assert_eq!(fw.infix(1, 3).as_string(), "bcd".to_string());
    }

    #[test]
    fn subwords() {
        let word = ReducedOmegaWord::periodic("abab");
        let pref = word.prefix(2);
        assert_eq!(pref.symbols().collect_vec(), vec!['a', 'b']);

        let word = upw!("ab", "ac");
        assert_eq!(word.offset(3).prefix(4).to_vec(), vec!['c', 'a', 'c', 'a']);
        assert_eq!(
            word.offset(1)
                .offset(1)
                .offset(1)
                .offset(1)
                .offset(4)
                .prefix(2)
                .to_vec(),
            vec!['a', 'c']
        );
        let w = word.offset(3);
        assert!(w.spoke().is_empty());
        assert_eq!(w.cycle().to_vec(), vec!['c', 'a']);

        let offset_normalized = upw!("abba").offset(1).offset(20).normalized();
        assert!(offset_normalized.spoke().is_empty());
        assert_eq!(offset_normalized.cycle().to_vec(), vec!['b', 'b', 'a', 'a']);
    }
}
