use std::hash::Hash;

use automata::{Set, Word};

type SampleComponentIter<'s, S> = std::iter::Filter<
    <S as Sample>::SampleIter<'s>,
    fn(&(&<S as Sample>::SampleWord, bool)) -> bool,
>;

pub type SampleKind<S> = <<S as Sample>::SampleWord as Word>::Kind;
pub trait Sample {
    type SampleWord: Eq + Word;
    type SampleIter<'w>: Iterator<Item = (&'w Self::SampleWord, bool)>
    where
        Self: 'w,
        Self::SampleWord: 'w;

    fn iter(&self) -> Self::SampleIter<'_>;

    fn positive(&self) -> SampleComponentIter<'_, Self> {
        self.iter().filter(|(_, b)| *b)
    }

    fn negative(&self) -> SampleComponentIter<'_, Self> {
        self.iter().filter(|(_, b)| !*b)
    }

    fn is_positive(&self, word: &Self::SampleWord) -> Option<bool>;

    fn is_negative(&self, word: &Self::SampleWord) -> Option<bool> {
        self.is_positive(word).map(|b| !b)
    }
}

pub struct SampleWithout<'s, W, S> {
    sample: &'s S,
    masked: Set<W>,
}

pub struct SampleWithoutIter<'w, W, I> {
    iter: I,
    masked: &'w Set<W>,
}

impl<'w, W: Eq + Hash, I: Iterator<Item = (&'w W, bool)>> Iterator for SampleWithoutIter<'w, W, I> {
    type Item = (&'w W, bool);

    fn next(&mut self) -> Option<Self::Item> {
        for (word, classification) in self.iter.by_ref() {
            if !self.masked.contains(word) {
                return Some((word, classification));
            }
        }
        None
    }
}

impl<'s, W: Word + Eq + Hash, S: Sample<SampleWord = W>> Sample for SampleWithout<'s, W, S> {
    type SampleWord = W;

    type SampleIter<'w> = SampleWithoutIter<'w, W, S::SampleIter<'w>>
    where
        Self: 'w,
        Self::SampleWord: 'w;

    fn iter(&self) -> Self::SampleIter<'_> {
        SampleWithoutIter {
            iter: self.sample.iter(),
            masked: &self.masked,
        }
    }

    fn is_positive(&self, word: &Self::SampleWord) -> Option<bool> {
        if self.masked.contains(word) {
            None
        } else {
            self.sample.is_positive(word)
        }
    }
}

impl<W: Eq + Word> Sample for (Vec<W>, Vec<W>) {
    type SampleWord = W;
    type SampleIter<'w> = std::iter::Chain<
    std::iter::Zip<std::slice::Iter<'w, W>, std::iter::Repeat<bool>>, std::iter::Zip<std::slice::Iter<'w, W>, std::iter::Repeat<bool>>> where W: 'w;

    fn iter(&self) -> Self::SampleIter<'_> {
        self.0
            .iter()
            .zip(std::iter::repeat(true))
            .chain(self.1.iter().zip(std::iter::repeat(false)))
    }

    fn is_positive(&self, word: &W) -> Option<bool> {
        if self.0.contains(word) {
            Some(true)
        } else if self.1.contains(word) {
            Some(false)
        } else {
            None
        }
    }
}

impl<W: Eq + std::hash::Hash + Word> Sample for (Set<W>, Set<W>) {
    type SampleWord = W;
    type SampleIter<'w> = std::iter::Chain<
    std::iter::Zip<std::collections::hash_set::Iter<'w, W>, std::iter::Repeat<bool>>, std::iter::Zip<std::collections::hash_set::Iter<'w, W>, std::iter::Repeat<bool>>> where W: 'w;

    fn iter(&self) -> Self::SampleIter<'_> {
        self.0
            .iter()
            .zip(std::iter::repeat(true))
            .chain(self.1.iter().zip(std::iter::repeat(false)))
    }

    fn is_positive(&self, word: &W) -> Option<bool> {
        if self.0.contains(word) {
            Some(true)
        } else if self.1.contains(word) {
            Some(false)
        } else {
            None
        }
    }
}
