use std::ops::{Deref, DerefMut};

use crate::{
    alphabet::Symbol,
    length::{HasLength, RawPosition},
    Alphabet, Length,
};

use super::{
    subword::{Prefix, Suffix},
    Word,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position(usize);

impl Deref for Position {
    type Target = usize;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Position {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<usize> for Position {
    fn from(position: usize) -> Self {
        Self(position)
    }
}

impl From<&usize> for Position {
    fn from(position: &usize) -> Self {
        Self(*position)
    }
}

impl From<Position> for usize {
    fn from(value: Position) -> Self {
        value.0
    }
}

pub trait AsPosition {
    fn as_position(&self) -> Position;
}

impl AsPosition for usize {
    fn as_position(&self) -> Position {
        Position(*self)
    }
}

impl AsPosition for Position {
    fn as_position(&self) -> Position {
        *self
    }
}

impl AsPosition for RawPosition {
    fn as_position(&self) -> Position {
        (**self).into()
    }
}

pub trait Representable {
    type Alphabet: Alphabet;

    fn symbols(&self) -> RepresentableIter<'_, Self>
    where
        Self: std::marker::Sized,
    {
        RepresentableIter {
            representable: self,
            position: 0.into(),
        }
    }

    fn nth<P: AsPosition>(&self, position: P) -> Option<<Self::Alphabet as Alphabet>::Symbol>;
}

impl<A: Alphabet, L: Length> Representable for Word<A, L> {
    type Alphabet = A;
    fn nth<P: AsPosition>(&self, position: P) -> Option<<Self::Alphabet as Alphabet>::Symbol> {
        let position = self.to_raw_position(position.as_position())?;
        self.symbols.get(*position).copied()
    }
}

pub struct RepresentableIter<'a, R: Representable> {
    representable: &'a R,
    position: Position,
}

impl<'a, R: Representable> Iterator for RepresentableIter<'a, R> {
    type Item = <R::Alphabet as Alphabet>::Symbol;
    fn next(&mut self) -> Option<Self::Item> {
        let symbol = self.representable.nth(self.position)?;
        *self.position += 1;
        Some(symbol)
    }
}
