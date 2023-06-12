use std::borrow::Cow;

use itertools::Itertools;
use tracing::trace;

use crate::{
    ts::{InputOf, Path, StateOf, TransitionOf, TransitionReference},
    PeriodicWord, Set, Str, Successor, Symbol, Trigger, UltimatelyPeriodicWord, Value, Word,
};

use super::{FiniteLength, HasLength, InfiniteLength, Length, SymbolIterable};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Repr<'a, S: Value, L: Length> {
    raw: Cow<'a, [S]>,
    length: L,
}

impl<'a, S: Value, L: Length> Repr<'a, S, L> {
    pub fn new<I: Into<Cow<'a, [S]>>>(raw_representation: I, mode: L) -> Self {
        let mut length = mode;
        Self {
            raw: length.normalize(raw_representation),
            length,
        }
    }
}

impl<'a, S: Symbol, L: Length> HasLength for Repr<'a, S, L> {
    type Len = L;
    fn length(&self) -> Self::Len {
        self.length
    }
}

impl<'a, S: Symbol, L: Length> Word for Repr<'a, S, L> {
    type S = S;

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.length
            .calculate_raw_position(index)
            .and_then(|raw_position| self.raw.get(raw_position).cloned())
    }

    fn alphabet(&self) -> crate::Set<Self::S> {
        self.raw.iter().cloned().collect()
    }

    fn as_repr(&self) -> Repr<'_, Self::S, Self::Len> {
        self.clone()
    }
}

impl<'a> From<&'a String> for Repr<'a, char, FiniteLength> {
    fn from(value: &'a String) -> Self {
        Repr::new(
            value.chars().collect::<Vec<char>>(),
            FiniteLength(value.len()),
        )
    }
}

impl<'a> From<&'a str> for Repr<'a, char, FiniteLength> {
    fn from(value: &'a str) -> Self {
        Repr::new(
            value.chars().collect::<Vec<char>>(),
            FiniteLength(value.len()),
        )
    }
}

impl<'a, S: Symbol> From<&'a Str<S>> for Repr<'a, S, FiniteLength> {
    fn from(value: &'a Str<S>) -> Self {
        Repr::new(&value.symbols, FiniteLength(value.len()))
    }
}

impl<'a, S: Symbol> From<Str<S>> for Repr<'a, S, FiniteLength> {
    fn from(value: Str<S>) -> Self {
        let len = value.len();
        Repr::new(value.symbols, FiniteLength(len))
    }
}

impl<'a, S: Symbol> From<PeriodicWord<S>> for Repr<'a, S, InfiniteLength> {
    fn from(value: PeriodicWord<S>) -> Self {
        let len = value.0.len();
        Repr::new(value.0.symbols, InfiniteLength(len, 0))
    }
}

impl<'a, S: Symbol> From<&'a PeriodicWord<S>> for Repr<'a, S, InfiniteLength> {
    fn from(value: &'a PeriodicWord<S>) -> Self {
        Repr::new(&value.0.symbols, InfiniteLength(value.0.len(), 0))
    }
}

impl<'a, S: Symbol> From<UltimatelyPeriodicWord<S>> for Repr<'a, S, InfiniteLength> {
    fn from(value: UltimatelyPeriodicWord<S>) -> Self {
        let split = value.0.len();
        let raw = value
            .0
            .symbols
            .into_iter()
            .chain(value.1 .0.symbols.into_iter())
            .collect_vec();
        Self {
            length: InfiniteLength(raw.len(), split),
            raw: raw.into(),
        }
    }
}

impl<'a, S: Symbol> From<&'a UltimatelyPeriodicWord<S>> for Repr<'a, S, InfiniteLength> {
    fn from(value: &'a UltimatelyPeriodicWord<S>) -> Self {
        let split = value.0.len();
        let raw = value
            .0
            .symbols
            .iter()
            .chain(value.1 .0.symbols.iter())
            .cloned()
            .collect_vec();
        let len = raw.len();
        Repr::new(raw, InfiniteLength(len, split))
    }
}

impl<'a, S: Symbol> From<Vec<S>> for Repr<'a, S, FiniteLength> {
    fn from(value: Vec<S>) -> Self {
        let length = FiniteLength(value.len());
        Repr::new(value, length)
    }
}

impl<'a, S: Symbol> From<&'a Vec<S>> for Repr<'a, S, FiniteLength> {
    fn from(value: &'a Vec<S>) -> Self {
        let length = FiniteLength(value.len());
        Repr::new(value, length)
    }
}

#[cfg(test)]
mod tests {
    use crate::{upw, Word};

    #[test]
    fn repr_upw() {
        println!("{:?}", upw!("abab").as_repr());
    }
}
