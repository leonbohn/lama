use std::fmt::Display;

use itertools::Itertools;

use super::{IsFinite, SymbolIterable, Word};
use crate::{FiniteKind, Symbol};

pub trait FiniteWord {
    type Symbol: Symbol;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents a 'usual' finite word consisting of a sequence of symbols.
pub struct Str<S> {
    /// The symbols making up the object.
    pub symbols: Vec<S>,
}

impl<S: Symbol> Display for Str<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_empty() {
            write!(f, "ε")
        } else {
            write!(
                f,
                "{}",
                self.symbols.iter().map(|chr| format!("{}", chr)).join("")
            )
        }
    }
}

impl<S: Symbol> PartialOrd for Str<S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<S: Symbol> Ord for Str<S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.symbols.len().cmp(&other.symbols.len()) {
            std::cmp::Ordering::Equal => self.symbols.iter().cmp(&other.symbols),
            ord => ord,
        }
    }
}

impl Str<char> {
    /// Creates an `Str` object from something that can be displayed.
    pub fn from_display<D: Display>(d: D) -> Self {
        Self {
            symbols: d.to_string().chars().collect(),
        }
    }
}

impl<S: Symbol> IsFinite for Str<S> {
    fn length(&self) -> usize {
        self.symbols.len()
    }
}

impl<S: Clone> FromIterator<S> for Str<S> {
    fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
        Self {
            symbols: iter.into_iter().collect(),
        }
    }
}

impl<S: Symbol> From<Vec<S>> for Str<S> {
    fn from(symbols: Vec<S>) -> Self {
        Str { symbols }
    }
}

impl From<&str> for Str<char> {
    fn from(s: &str) -> Self {
        Str {
            symbols: s.chars().collect(),
        }
    }
}

impl<C: Symbol> SymbolIterable for Str<C> {
    type Iter = std::vec::IntoIter<C>;

    fn iter(&self) -> Self::Iter {
        self.symbols.clone().into_iter()
    }
}

impl<S: Symbol> Word for Str<S> {
    type Kind = FiniteKind;
    type S = S;
    fn nth(&self, index: usize) -> Option<Self::S> {
        self.symbols.get(index).cloned()
    }
}

impl<S> Str<S> {
    pub fn alphabet(&self) -> impl Iterator<Item = &S>
    where
        S: Symbol,
    {
        self.symbols.iter().unique()
    }

    /// Returns the length of the word.
    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    /// Returns true iff the word is empty
    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    /// Creates an empty instance.
    pub fn empty() -> Self {
        Self { symbols: vec![] }
    }

    pub fn has_prefix(&self, prefix: &Self) -> bool
    where
        S: Eq,
    {
        self.symbols.starts_with(&prefix.symbols)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_llex_order() {
        let left = super::Str::from(vec!['a', 'b', 'c']);
        let right_same_length = super::Str::from(vec!['a', 'b', 'd']);
        let right_shorter = super::Str::from(vec!['a', 'b']);
        let right_longer = super::Str::from(vec!['a', 'b', 'c', 'd']);
        assert!(left < right_same_length);
        assert!(left > right_shorter);
        assert!(left < right_longer);
    }
}
