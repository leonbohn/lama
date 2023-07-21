use crate::{length::HasLength, FiniteLength, Length};

use super::Word;

/// A suffix of a [`Sequence`] which skips the first `offset` symbols.
#[derive(Clone, PartialEq, Debug)]
pub struct Offset<'a, W: Word> {
    sequence: &'a W,
    offset: usize,
}

impl<'a, W: Word> Word for Offset<'a, W> {
    type Symbol = W::Symbol;

    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        self.sequence.nth(position + self.offset)
    }
}

impl<'a, S: Word> HasLength for Offset<'a, S> {
    type Length = S::Length;

    fn length(&self) -> Self::Length {
        self.sequence.length().subtract_front(self.offset)
    }
}

impl<'a, S: Word> Offset<'a, S> {
    /// Creates a new suffix, which skips the first `offset` symbols of the given sequence.
    pub fn new(sequence: &'a S, offset: usize) -> Self {
        Self { sequence, offset }
    }
}

/// A prefix of a [`Sequence`] which only contains the first `length` symbols.
#[derive(Clone, PartialEq, Debug)]
pub struct Prefix<'a, S: Word> {
    sequence: &'a S,
    length: usize,
}

impl<'a, S: Word> HasLength for Prefix<'a, S> {
    type Length = FiniteLength;

    fn length(&self) -> Self::Length {
        FiniteLength::new(self.length)
    }
}

impl<'a, S: Word> Word for Prefix<'a, S> {
    type Symbol = S::Symbol;

    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        if position < self.length {
            self.sequence.nth(position)
        } else {
            None
        }
    }
}

impl<'a, S: Word> Prefix<'a, S> {
    /// Creates a new prefix of the given length.
    pub fn new(sequence: &'a S, length: usize) -> Self {
        Self { sequence, length }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{
        word::{OmegaWord, Word},
        FiniteLength,
    };

    #[test]
    fn subwords() {
        let word = OmegaWord::new(vec!['a', 'b', 'a', 'b'], FiniteLength::new(4));
        let pref = word.prefix(2);
        assert_eq!(pref.raw_to_vec(), vec!['a', 'b']);
    }
}
