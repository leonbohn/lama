use std::{
    borrow::Borrow,
    fmt::Display,
    ops::{Add, AddAssign},
};

use itertools::Itertools;

use super::{FiniteLength, HasLength, Length, SymbolIterable, Word, WordTransitions};
use crate::{
    congruence::CongruenceTransition,
    ts::{transitionsystem::States, HasInput, HasStates, IntoStates, IntoTransitions},
    Class, Set, Successor, Symbol,
};

pub trait FiniteWord {
    type Symbol: Symbol;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents a 'usual' finite word consisting of a sequence of symbols.
pub struct Str<S> {
    /// The symbols making up the object.
    pub symbols: Vec<S>,
}

impl<S> Str<S> {
    /// Gets the last symbol of the word.
    pub fn last(&self) -> Option<&S> {
        self.symbols.last()
    }

    /// Appends the given symbol to the end of the word.
    pub fn push_back<X: Borrow<S>>(&mut self, sym: X)
    where
        S: Clone,
    {
        self.symbols.push(sym.borrow().clone())
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
    pub fn epsilon() -> Self {
        Self { symbols: vec![] }
    }

    /// Returns true if and only if `prefix` is a prefix of `self`, meaning all symbols
    /// of `prefix` are contained in `self` in the same order.
    pub fn has_prefix<X: Borrow<S>, I: IntoIterator<Item = X>>(&self, prefix: I) -> bool
    where
        S: Eq,
    {
        prefix
            .into_iter()
            .enumerate()
            .all(|(i, sym)| self.symbols.get(i) == Some(sym.borrow()))
    }
}

impl<S: Symbol> PartialEq<Class<S>> for Str<S> {
    fn eq(&self, other: &Class<S>) -> bool {
        self.symbols == other.0.symbols
    }
}

impl PartialEq<String> for Str<char> {
    fn eq(&self, other: &String) -> bool {
        self.symbols.iter().zip(other.chars()).all(|(a, b)| a == &b)
    }
}

impl<S: Symbol> IntoIterator for Str<S> {
    type Item = S;
    type IntoIter = std::vec::IntoIter<S>;

    fn into_iter(self) -> Self::IntoIter {
        self.symbols.into_iter()
    }
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

impl<S: Clone> FromIterator<S> for Str<S> {
    fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
        Self {
            symbols: iter.into_iter().collect(),
        }
    }
}

impl<'a, S: Clone + 'a> FromIterator<&'a S> for Str<S> {
    fn from_iter<T: IntoIterator<Item = &'a S>>(iter: T) -> Self {
        Self {
            symbols: iter.into_iter().cloned().collect(),
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

impl<S: Symbol> Add<&S> for &Str<S> {
    type Output = Str<S>;

    fn add(self, rhs: &S) -> Self::Output {
        let mut v = self.symbols.clone();
        v.push(rhs.clone());
        Str { symbols: v }
    }
}

impl<S: Symbol> AddAssign<&Str<S>> for Str<S> {
    fn add_assign(&mut self, rhs: &Str<S>) {
        self.symbols.extend(rhs.symbols.iter().cloned())
    }
}

impl<S: Symbol> AddAssign<Str<S>> for Str<S> {
    fn add_assign(&mut self, rhs: Str<S>) {
        self.symbols.extend(rhs.symbols.into_iter())
    }
}

impl<S: Symbol> AddAssign<S> for Str<S> {
    fn add_assign(&mut self, rhs: S) {
        self.symbols.push(rhs)
    }
}

impl<S: Symbol> AddAssign<&S> for Str<S> {
    fn add_assign(&mut self, rhs: &S) {
        self.symbols.push(rhs.clone())
    }
}

impl<S: Symbol> HasLength for Str<S> {
    type Len = FiniteLength;
    fn length(&self) -> Self::Len {
        FiniteLength(self.symbols.len())
    }
}

impl<'a, S: Symbol> SymbolIterable for &'a Str<S> {
    type SymbolIter = std::vec::IntoIter<S>;

    fn symbol_iter(self) -> Self::SymbolIter {
        self.symbols.clone().into_iter()
    }
}

impl<S: Symbol> Word for Str<S> {
    type S = S;
    fn nth<I: Into<usize>>(&self, index: I) -> Option<Self::S> {
        self.symbols.get(index.into()).cloned()
    }

    fn alphabet(&self) -> Set<Self::S> {
        self.symbol_iter().collect()
    }
}

impl<S: Symbol> HasInput for Str<S> {
    type Sigma = S;

    type Input<'me> = itertools::Unique<std::slice::Iter<'me, S>>
    where Self:'me;

    fn input_alphabet(&self) -> Self::Input<'_> {
        self.symbols.iter().unique()
    }
}
impl<S: Symbol> HasStates for Str<S> {
    type Q = Class<S>;

    fn contains_state<X: Borrow<Self::Q>>(&self, state: X) -> bool {
        self.has_prefix(state.borrow().iter())
    }
}
impl<S: Symbol> Successor for Str<S> {
    fn successor<X: Borrow<Self::Q>, Y: Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q> {
        if self.has_prefix(from.borrow() + on.borrow()) {
            let mut new = from.borrow().clone();
            new.push_back(on.borrow());
            Some(new)
        } else {
            None
        }
    }
}

impl<'a, S: Symbol> IntoStates for &'a Str<S> {
    type StateRef = &'a Class<S>;

    type IntoStates = States<'a, Class<S>>;

    fn into_states(self) -> Self::IntoStates {
        todo!()
    }
}

impl<'a, S: Symbol> IntoTransitions for &'a Str<S> {
    type TransitionRef = CongruenceTransition<S>;

    type IntoTransitions = WordTransitions<Self>;

    fn into_transitions(self) -> Self::IntoTransitions {
        WordTransitions::new(self)
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
