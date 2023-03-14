use super::{IsInfinite, Str, SymbolIterable, Word};
use crate::{Boundedness, InfiniteKind, Symbol};

pub trait InfiniteWord {
    type Symbol: Symbol;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A `PeriodicWord` essentially just loops a [`FiniteWord`] over and over again.
pub struct PeriodicWord<S>(pub(crate) Str<S>);

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

impl<S: Symbol> SymbolIterable for PeriodicWord<S> {
    type Iter = std::iter::Cycle<std::vec::IntoIter<S>>;

    fn iter(&self) -> Self::Iter {
        self.0.iter().cycle()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// In an `UltimatelyPeriodicWord`, the first part is a finite prefix, after which a periodic part follows. The prefix can be empty.
pub struct UltimatelyPeriodicWord<S>(pub(crate) Str<S>, pub(crate) PeriodicWord<S>);

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
}

impl<S> From<PeriodicWord<S>> for UltimatelyPeriodicWord<S> {
    fn from(periodic: PeriodicWord<S>) -> Self {
        Self(Str::empty(), periodic)
    }
}

impl<S> From<(Str<S>, PeriodicWord<S>)> for UltimatelyPeriodicWord<S> {
    fn from((prefix, periodic): (Str<S>, PeriodicWord<S>)) -> Self {
        Self(prefix, periodic)
    }
}

impl<S: Symbol> SymbolIterable for UltimatelyPeriodicWord<S> {
    type Iter = std::iter::Chain<std::vec::IntoIter<S>, std::iter::Cycle<std::vec::IntoIter<S>>>;

    fn iter(&self) -> Self::Iter {
        self.0.iter().chain(self.1.iter())
    }
}
