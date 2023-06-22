use crate::{length::HasLength, FiniteLength};

use super::Word;

/// A suffix of a [`Sequence`] which skips the first `offset` symbols.
#[derive(Clone, PartialEq, Debug)]
pub struct Suffix<'a, S: Word> {
    sequence: &'a S,
    offset: usize,
}

impl<'a, S: Word> HasLength for Suffix<'a, S> {
    type Length = S::Length;

    fn length(&self) -> Self::Length {
        self.sequence.length()
    }
}

impl<'a, S: Word> Word for Suffix<'a, S> {
    type Raw = S::Raw;
    type Symbol = S::Symbol;

    fn rawpresentation(&self) -> &Self::Raw {
        self.sequence.rawpresentation()
    }

    fn get(&self, position: usize) -> Option<Self::Symbol> {
        self.sequence.get(position + self.offset)
    }

    fn symbols(&self) -> super::RawpresentationIter<'_, Self::Raw, Self::Length> {
        super::RawpresentationIter::new(self.rawpresentation(), self.length(), self.offset)
    }
}

impl<'a, S: Word> Suffix<'a, S> {
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
    type Raw = S::Raw;
    type Symbol = S::Symbol;

    fn rawpresentation(&self) -> &Self::Raw {
        self.sequence.rawpresentation()
    }

    fn get(&self, position: usize) -> Option<Self::Symbol> {
        if position < self.length {
            self.sequence.get(position)
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
        word::{RawWithLength, Word},
        FiniteLength,
    };

    #[test]
    fn subwords() {
        let word = RawWithLength::new(vec!['a', 'b', 'a', 'b'], FiniteLength::new(4));
        let pref = word.prefix(2);
        assert_eq!(pref.symbols().collect_vec(), vec!['a', 'b']);
        assert_eq!(
            word.suffix(2).symbols().collect_vec(),
            word.prefix(2).symbols().collect_vec()
        );
    }
}
