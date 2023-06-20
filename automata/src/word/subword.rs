use super::Sequence;

/// A suffix of a [`Sequence`] which skips the first `offset` symbols.
#[derive(Clone, PartialEq, Debug)]
pub struct Suffix<'a, S: Sequence> {
    sequence: &'a S,
    offset: usize,
}

impl<'a, S: Sequence> Sequence for Suffix<'a, S> {
    type Raw = S::Raw;
    type Symbol = S::Symbol;

    fn rawpresentation(&self) -> &Self::Raw {
        self.sequence.rawpresentation()
    }

    fn get(&self, position: usize) -> Option<Self::Symbol> {
        self.sequence.get(position + self.offset)
    }
}

impl<'a, S: Sequence> Suffix<'a, S> {
    /// Creates a new suffix, which skips the first `offset` symbols of the given sequence.
    pub fn new(sequence: &'a S, offset: usize) -> Self {
        Self { sequence, offset }
    }
}

/// A prefix of a [`Sequence`] which only contains the first `length` symbols.
#[derive(Clone, PartialEq, Debug)]
pub struct Prefix<'a, S: Sequence> {
    sequence: &'a S,
    length: usize,
}

impl<'a, S: Sequence> Sequence for Prefix<'a, S> {
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

impl<'a, S: Sequence> Prefix<'a, S> {
    /// Creates a new prefix of the given length.
    pub fn new(sequence: &'a S, length: usize) -> Self {
        Self { sequence, length }
    }
}
