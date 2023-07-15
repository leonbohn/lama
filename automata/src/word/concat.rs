use crate::{
    length::{HasLength, RawPosition},
    FiniteLength, Length, Word,
};

use super::Rawpresentation;

#[derive(Debug, Clone)]
pub struct Concat<W, V>(W, V);

impl<W, V> Rawpresentation for Concat<W, V>
where
    W: Word<Length = FiniteLength>,
    W::Length: std::ops::Add<V::Length, Output = V::Length>,
    V: Word<Symbol = W::Symbol>,
{
    type Symbol = W::Symbol;

    fn raw_get(&self, position: crate::length::RawPosition) -> Option<Self::Symbol> {
        let pos = position.position();
        if pos >= self.0.rawpresentation().raw_length() {
            self.1.rawpresentation().raw_get(RawPosition::new(
                pos - self.0.rawpresentation().raw_length(),
            ))
        } else {
            self.0.rawpresentation().raw_get(position)
        }
    }

    fn raw_length(&self) -> usize {
        self.0.rawpresentation().raw_length() + self.1.rawpresentation().raw_length()
    }
}

impl<W, V> Word for Concat<W, V>
where
    W: Word<Length = FiniteLength>,
    W::Length: std::ops::Add<V::Length, Output = V::Length>,
    V: Word<Symbol = W::Symbol>,
{
    type Raw = Self;

    type Symbol = W::Symbol;

    fn get(&self, position: usize) -> Option<Self::Symbol> {
        todo!()
    }

    fn rawpresentation(&self) -> &Self::Raw {
        self
    }
}

impl<W, V> HasLength for Concat<W, V>
where
    W: HasLength,
    W::Length: std::ops::Add<V::Length, Output = V::Length>,
    V: HasLength,
{
    type Length = V::Length;

    fn length(&self) -> Self::Length {
        if <V::Length as Length>::is_infinite() {
            self.1.length()
        } else {
            self.0.length() + self.1.length()
        }
    }
}

impl<W: Word<Length = FiniteLength>, V: Word<Symbol = W::Symbol>> Concat<W, V> {
    pub fn new(prefix: W, suffix: V) -> Self {
        Self(prefix, suffix)
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{length::HasLength, Word};

    #[test]
    fn concatenations() {
        let prefix = "abc";
        let suffix = "def";
        let combined = prefix.concat(suffix);
        assert_eq!(combined.symbols().join(""), "abcdef");
    }
}
