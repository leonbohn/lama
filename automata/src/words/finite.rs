use std::fmt::Display;

use super::{IsFinite, SymbolIterable, Word};
use crate::{FiniteKind, Symbol};

pub trait FiniteWord {
    type Symbol: Symbol;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents a 'usual' finite word consisting of a sequence of symbols.
pub struct Str<S> {
    pub(crate) symbols: Vec<S>,
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
}
