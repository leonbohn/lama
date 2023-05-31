use std::fmt::Display;

use itertools::Itertools;

use super::{IsFinite, IsInfinite, Str, SymbolIterable, Word, WordTransitions};
use crate::{
    congruence::CongruenceTransition,
    ts::{transitionsystem::States, HasInput, HasStates, IntoStates, IntoTransitions},
    Class, InfiniteKind, RightCongruence, Set, Subword, Successor, Symbol, TransitionSystem,
};

pub trait InfiniteWord {
    type Symbol: Symbol;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A `PeriodicWord` essentially just loops a [`FiniteWord`] over and over again.
pub struct PeriodicWord<S>(pub(crate) Str<S>);

impl<S: Symbol> Display for PeriodicWord<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.0)
    }
}

impl<S: Symbol> IsInfinite for PeriodicWord<S> {
    fn base_length(&self) -> usize {
        0
    }

    fn recur_length(&self) -> usize {
        self.0.symbols.len()
    }
}

impl<S: Symbol> Word for PeriodicWord<S> {
    type Kind = InfiniteKind;
    type S = S;

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.0.nth(index % self.0.symbols.len())
    }

    fn alphabet(&self) -> Set<Self::S> {
        self.0.symbol_iter().collect()
    }
}

impl<S, I: Into<Str<S>>> From<I> for PeriodicWord<S> {
    fn from(finite: I) -> Self {
        Self(finite.into())
    }
}

impl<S> TryFrom<UltimatelyPeriodicWord<S>> for PeriodicWord<S> {
    type Error = ();

    fn try_from(upw: UltimatelyPeriodicWord<S>) -> Result<Self, Self::Error> {
        if upw.0.is_empty() {
            Ok(upw.1)
        } else {
            Err(())
        }
    }
}

impl<'a, S: Symbol> SymbolIterable for &'a PeriodicWord<S> {
    type SymbolIter = std::iter::Cycle<std::vec::IntoIter<S>>;

    fn symbol_iter(self) -> Self::SymbolIter {
        self.0.symbol_iter().cycle()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// In an `UltimatelyPeriodicWord`, the first part is a finite prefix, after which a periodic part follows. The prefix can be empty.
pub struct UltimatelyPeriodicWord<S>(pub(crate) Str<S>, pub(crate) PeriodicWord<S>);

impl<S: Symbol> Display for UltimatelyPeriodicWord<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.0, self.1)
    }
}

impl<S: Symbol> IsInfinite for UltimatelyPeriodicWord<S> {
    fn base_length(&self) -> usize {
        self.0.symbols.len()
    }

    fn recur_length(&self) -> usize {
        self.1 .0.symbols.len()
    }
}

impl<S: Symbol> Word for UltimatelyPeriodicWord<S> {
    type Kind = InfiniteKind;
    type S = S;

    fn nth(&self, index: usize) -> Option<Self::S> {
        let prefix_length = self.0.symbols.len();
        if index < prefix_length {
            self.0.nth(index)
        } else {
            self.1.nth(index - prefix_length)
        }
    }

    fn alphabet(&self) -> Set<Self::S> {
        self.0
            .alphabet()
            .union(&self.1.alphabet())
            .cloned()
            .collect()
    }
}

impl<S> From<PeriodicWord<S>> for UltimatelyPeriodicWord<S> {
    fn from(periodic: PeriodicWord<S>) -> Self {
        Self(Str::epsilon(), periodic)
    }
}

impl<S> From<(Str<S>, PeriodicWord<S>)> for UltimatelyPeriodicWord<S> {
    fn from((prefix, periodic): (Str<S>, PeriodicWord<S>)) -> Self {
        Self(prefix, periodic)
    }
}

impl<S> From<(Str<S>, Str<S>)> for UltimatelyPeriodicWord<S> {
    fn from((prefix, periodic): (Str<S>, Str<S>)) -> Self {
        Self(prefix, periodic.into())
    }
}

impl<S, I> From<I> for UltimatelyPeriodicWord<S>
where
    I: Into<Str<S>>,
{
    fn from(value: I) -> Self {
        Self(Str::epsilon(), value.into().into())
    }
}

impl<'a, S: Symbol> SymbolIterable for &'a UltimatelyPeriodicWord<S> {
    type SymbolIter =
        std::iter::Chain<std::vec::IntoIter<S>, std::iter::Cycle<std::vec::IntoIter<S>>>;

    fn symbol_iter(self) -> Self::SymbolIter {
        self.0.symbol_iter().chain(self.1.symbol_iter())
    }
}

impl<S: Symbol> UltimatelyPeriodicWord<S> {
    /// Returns a reference to the base part of the word.
    pub fn base(&self) -> &Str<S> {
        &self.0
    }

    /// Returns a reference to the looping part of the word.
    pub fn recur(&self) -> &PeriodicWord<S> {
        &self.1
    }

    /// Unrolls the word by one step, i.e. it appends the first symbol of the looping part
    /// to the base part and rotates the looping part by one.
    ///
    /// # Example
    /// If we consider a word like (aba,baa), then unrolling it once will result in
    /// the word (abab,aab).
    pub fn unroll_one(&mut self) {
        self.0.symbols.push(self.1 .0.symbols[0].clone());
        self.1 .0.symbols.rotate_left(1);
    }

    /// Returns true if and only if the word has the given prefix.
    pub fn has_prefix(&self, prefix: &Str<S>) -> bool {
        self.symbol_iter()
            .zip(prefix.symbol_iter())
            .all(|(x, y)| x == y)
    }

    pub fn into_ts(&self) -> RightCongruence<S> {
        let mut transitions = vec![];
        for i in (0..(self.base_length() + self.recur_length())) {
            transitions.push((self.prefix(i), self.nth(i).unwrap(), self.prefix(i + 1)))
        }
        transitions.push((
            self.prefix(self.base_length() + self.recur_length()),
            self.nth(self.base_length() + self.recur_length()).unwrap(),
            self.prefix(self.base_length() + 1),
        ));
        transitions.into_iter().collect()
    }
}
