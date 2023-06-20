use crate::{
    alphabet::Symbol,
    length::{HasLength, RawPosition},
    Alphabet, FiniteLength, InfiniteLength, Length,
};

use self::subword::Prefix;

pub mod subword;

pub type FiniteWord<A> = Word<A, FiniteLength>;
pub type InfiniteWord<A> = Word<A, InfiniteLength>;

pub trait Rawpresentation: Clone {
    type Symbol: Symbol;
    fn raw_get(&self, position: RawPosition) -> Option<Self::Symbol>;
    fn raw_length(&self) -> usize;
}

impl<S: Symbol> Rawpresentation for Vec<S> {
    type Symbol = S;
    fn raw_get(&self, position: RawPosition) -> Option<Self::Symbol> {
        self.get(position.position()).cloned()
    }
    fn raw_length(&self) -> usize {
        self.len()
    }
}

pub trait Sequence {
    type Raw: Rawpresentation;
    type Symbol: Symbol;
    fn get(&self, position: usize) -> Option<Self::Symbol>;
    fn rawpresentation(&self) -> &Self::Raw;

    fn prefix(&self, length: usize) -> Prefix<'_, Self>
    where
        Self: Sized,
    {
        Prefix::new(self, length)
    }

    fn suffix(&self, offset: usize) -> subword::Suffix<'_, Self>
    where
        Self: Sized,
    {
        subword::Suffix::new(self, offset)
    }

    fn loop_back_to(&self, position: usize) -> Word<Self::Raw, InfiniteLength> {
        Word {
            raw: self.rawpresentation().clone(),
            length: InfiniteLength::new(self.rawpresentation().raw_length(), position),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Word<R, L> {
    raw: R,
    length: L,
}

impl<R, L> Word<R, L> {
    pub fn new(raw: R, length: L) -> Self
    where
        R: Rawpresentation,
        L: Length,
    {
        Self { raw, length }
    }
}

impl<R, L> Sequence for Word<R, L>
where
    R: Rawpresentation,
    L: Length,
{
    type Raw = R;
    type Symbol = R::Symbol;
    fn get(&self, position: usize) -> Option<R::Symbol> {
        let raw_position = self.length.calculate_raw_position(position)?;
        self.raw.raw_get(raw_position)
    }
    fn rawpresentation(&self) -> &Self::Raw {
        &self.raw
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        length::RawPosition,
        word::{Rawpresentation, Sequence, Word},
        FiniteLength,
    };

    #[test]
    fn raw_representation() {
        let raw = vec!['a', 'b', 'a', 'b'];
        assert_eq!(raw.raw_get(RawPosition::new(0)), Some('a'));

        let word = Word::new(raw, FiniteLength::new(4));
        assert_eq!(word.get(1), Some('b'));
        assert_eq!(word.get(4), None);

        let infinite = word.loop_back_to(3);
        assert_eq!(infinite.get(4), Some('b'));
    }
}
