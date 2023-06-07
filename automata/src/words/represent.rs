use std::borrow::Cow;

use itertools::Itertools;
use tracing::trace;

use crate::{
    ts::{InputOf, Path, StateOf, TransitionOf, TransitionReference},
    PeriodicWord, Set, Str, Successor, Symbol, Trigger, UltimatelyPeriodicWord, Value, Word,
};

use super::{HasLength, InducesFromPath, InfiniteLength, Length, SymbolIterable};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Repr<'a, S: Value, L: Length> {
    raw: Cow<'a, [S]>,
    length: L,
}

pub trait Representable<S: Value>: HasLength {
    fn represent(&self) -> Repr<'_, S, Self::Len>;
}
impl<S: Value, R: Representable<S>> Representable<S> for &R {
    fn represent(&self) -> Repr<'_, S, Self::Len> {
        (*self).represent()
    }
}

impl<'a, S: Value, L: Length> Repr<'a, S, L> {
    pub fn new<I: Into<Cow<'a, [S]>>>(raw_representation: I, mode: L) -> Self {
        Self {
            raw: raw_representation.into(),
            length: mode,
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
}

impl<'a, S: Symbol> From<&'a Str<S>> for Repr<'a, S, usize> {
    fn from(value: &'a Str<S>) -> Self {
        Repr::new(&value.symbols, value.len())
    }
}

impl<'a, S: Symbol> From<Str<S>> for Repr<'a, S, usize> {
    fn from(value: Str<S>) -> Self {
        let len = value.len();
        Repr::new(value.symbols, len)
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
        Self {
            length: InfiniteLength(raw.len(), split),
            raw: raw.into(),
        }
    }
}
