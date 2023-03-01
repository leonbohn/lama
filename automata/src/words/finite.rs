use super::{IsFinite, SymbolIterable, Word};
use crate::{FiniteKind, Symbol};

#[derive(Debug, Clone, PartialEq, Eq)]
/// Represents a 'usual' finite word consisting of a sequence of symbols.
pub struct FiniteWord<S> {
    pub(crate) symbols: Vec<S>,
}

impl<S: Symbol> IsFinite for FiniteWord<S> {
    fn length(&self) -> usize {
        self.symbols.len()
    }
}

impl<S: Clone> FromIterator<S> for FiniteWord<S> {
    fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
        Self {
            symbols: iter.into_iter().collect(),
        }
    }
}

impl<S: Symbol> From<Vec<S>> for FiniteWord<S> {
    fn from(symbols: Vec<S>) -> Self {
        FiniteWord { symbols }
    }
}

impl From<&str> for FiniteWord<char> {
    fn from(s: &str) -> Self {
        FiniteWord {
            symbols: s.chars().collect(),
        }
    }
}

impl<C: Symbol> SymbolIterable for FiniteWord<C> {
    type Iter = std::vec::IntoIter<C>;

    fn iter(&self) -> Self::Iter {
        self.symbols.clone().into_iter()
    }
}

impl<S: Symbol> Word for FiniteWord<S> {
    type S = S;
    type Kind = FiniteKind;

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.symbols.get(index).cloned()
    }
}

impl<S> FiniteWord<S> {
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
