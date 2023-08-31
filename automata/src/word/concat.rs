use crate::{
    length::{HasLength, RawPosition},
    FiniteLength, Length, Word,
};

#[derive(Debug, Clone)]
pub struct Concat<W, V>(W, V);

impl<W, V> Word for Concat<W, V>
where
    W: Word<Length = FiniteLength>,
    V: Word<Symbol = W::Symbol>,
{
    type Symbol = W::Symbol;

    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        if self.0.length().as_usize() > position {
            self.0.nth(position)
        } else {
            self.1.nth(position - self.0.length().as_usize())
        }
    }
}

impl<W, V> HasLength for Concat<W, V>
where
    W: HasLength<Length = FiniteLength>,
    V: HasLength,
{
    type Length = V::Length;

    fn length(&self) -> Self::Length {
        self.1.length().add_front(self.0.length().as_usize())
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
        let combined = prefix.append(suffix);
        assert_eq!(combined.finite_to_vec(), vec!['a', 'b', 'c', 'd', 'e', 'f']);
    }
}
