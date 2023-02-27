use super::{FiniteWord, SymbolIterable, Word};
use crate::InfiniteKind;

#[derive(Debug, Clone, PartialEq, Eq)]
/// A `PeriodicWord` essentially just loops a `FiniteWord` over and over again.
pub struct PeriodicWord<S>(pub(crate) FiniteWord<S>);

impl<S: Clone> Word for PeriodicWord<S> {
    type S = S;

    type Kind = InfiniteKind;

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.0.nth(index % self.0.symbols.len())
    }
}

impl<S> From<FiniteWord<S>> for PeriodicWord<S> {
    fn from(finite: FiniteWord<S>) -> Self {
        Self(finite)
    }
}

impl<S: Clone> From<Vec<S>> for PeriodicWord<S> {
    fn from(symbols: Vec<S>) -> Self {
        Self(FiniteWord::from(symbols))
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

impl<S: Clone> SymbolIterable for PeriodicWord<S> {
    type Iter = std::iter::Cycle<std::vec::IntoIter<S>>;

    fn iter(&self) -> Self::Iter {
        self.0.iter().cycle()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// In an `UltimatelyPeriodicWord`, the first part is a finite prefix, after which a periodic part follows. The prefix can be empty.
pub struct UltimatelyPeriodicWord<S>(pub(crate) FiniteWord<S>, pub(crate) PeriodicWord<S>);

impl<S: Clone> Word for UltimatelyPeriodicWord<S> {
    type S = S;

    type Kind = InfiniteKind;

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
        Self(FiniteWord::empty(), periodic)
    }
}

impl<S> From<(FiniteWord<S>, PeriodicWord<S>)> for UltimatelyPeriodicWord<S> {
    fn from((prefix, periodic): (FiniteWord<S>, PeriodicWord<S>)) -> Self {
        Self(prefix, periodic)
    }
}

impl<S: Clone> SymbolIterable for UltimatelyPeriodicWord<S> {
    type Iter = std::iter::Chain<std::vec::IntoIter<S>, std::iter::Cycle<std::vec::IntoIter<S>>>;

    fn iter(&self) -> Self::Iter {
        self.0.iter().chain(self.1.iter())
    }
}

/// Creates an ultimately periodic word from a finite prefix and a periodic part.
pub fn upw<S, B: Into<FiniteWord<S>>, R: Into<FiniteWord<S>>>(
    base: B,
    rep: R,
) -> UltimatelyPeriodicWord<S> {
    (base.into(), PeriodicWord(rep.into())).into()
}

/// Creates a periodic word from an object that can be turned into a [`FiniteWord`].
pub fn pw<S, R: Into<FiniteWord<S>>>(rep: R) -> PeriodicWord<S> {
    rep.into().into()
}
