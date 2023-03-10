use crate::Symbol;

use super::{FiniteWord, IsFinite, PeriodicWord, SymbolIterable, UltimatelyPeriodicWord, Word};

/// A trait which allows accessing a finite prefix of a given length as well as a finite suffix of a word which is obtained by skipping a number of symbols from the start.
pub trait Subword: Word {
    /// The type of the suffix of the word.
    type SuffixType: Word<S = Self::S> + Subword;

    /// The type of the prefix of the word.
    type PrefixType: Word<S = Self::S> + Subword + IsFinite;

    /// Returns a prefix of length `length` of the word.
    fn prefix(&self, length: usize) -> Self::PrefixType;

    /// Returns a word which is the same kind as the original word, but with the first `number` symbols removed.
    fn skip(&self, number: usize) -> Self::SuffixType;
}

impl<S: Symbol> Subword for FiniteWord<S> {
    type SuffixType = Self;

    type PrefixType = Self;

    fn prefix(&self, length: usize) -> Self::PrefixType {
        self.symbols.iter().take(length).cloned().collect()
    }

    fn skip(&self, number: usize) -> Self {
        self.symbols.iter().skip(number).cloned().collect()
    }
}

impl<S: Symbol> Subword for PeriodicWord<S> {
    type PrefixType = FiniteWord<S>;
    type SuffixType = Self;

    fn prefix(&self, length: usize) -> FiniteWord<Self::S> {
        self.0.iter().cycle().take(length).collect()
    }

    fn skip(&self, number: usize) -> Self {
        let mut symbols = self.0.symbols.clone();
        symbols.rotate_left(number % self.0.symbols.len());
        Self(symbols.into())
    }
}

impl<S: Symbol> Subword for UltimatelyPeriodicWord<S> {
    type PrefixType = FiniteWord<S>;
    type SuffixType = UltimatelyPeriodicWord<S>;

    fn prefix(&self, length: usize) -> FiniteWord<Self::S> {
        let prefix_length = self.0.symbols.len();
        if length <= prefix_length {
            self.0.prefix(length)
        } else {
            let mut symbols = self.0.symbols.clone();
            symbols.extend(self.1.iter().take(length - prefix_length));
            symbols.into()
        }
    }

    fn skip(&self, number: usize) -> Self::SuffixType {
        let prefix_length = self.0.symbols.len();
        if number <= prefix_length {
            Self(self.0.skip(number), self.1.clone())
        } else {
            Self(FiniteWord::empty(), self.1.skip(number - prefix_length))
        }
    }
}

impl Subword for String {
    type PrefixType = String;
    type SuffixType = Self;

    fn prefix(&self, length: usize) -> Self::PrefixType {
        self.chars().take(length).collect()
    }

    fn skip(&self, number: usize) -> Self {
        self.chars().skip(number).collect()
    }
}

impl Subword for &str {
    type PrefixType = String;
    type SuffixType = String;

    fn prefix(&self, length: usize) -> Self::PrefixType {
        self.chars().take(length).collect()
    }

    fn skip(&self, number: usize) -> Self::SuffixType {
        self.chars().skip(number).collect()
    }
}

impl<S: Symbol> Subword for Vec<S> {
    type PrefixType = Vec<S>;
    type SuffixType = Vec<S>;

    fn prefix(&self, length: usize) -> Self::PrefixType {
        Vec::from(
            self.get(..(std::cmp::min(length, self.len())))
                .unwrap_or_default(),
        )
    }

    fn skip(&self, number: usize) -> Self::SuffixType {
        if number >= self.len() {
            Vec::new()
        } else {
            Vec::from(self.get(number..).unwrap())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn subword_impls() {
        let word = FiniteWord::from(vec![1, 3, 3, 7]);
        assert_eq!(vec![1, 3, 3, 7].prefix(2), vec![1, 3]);
        assert_eq!("1337".skip(2), "37".to_string());
        assert_eq!(word.prefix(10), FiniteWord::from(vec![1, 3, 3, 7]));
        assert_eq!(word.skip(10), FiniteWord::from(vec![]));
    }
}
