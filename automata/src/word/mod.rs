use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::{alphabet::Symbol, length::HasLength, FiniteLength, InfiniteLength, Length, Show};
use impl_tools::autoimpl;
use itertools::Itertools;

mod subword;
pub use subword::Offset;

mod concat;
pub use concat::Concat;

mod normalized;

mod finite;
pub use finite::FiniteWord;

mod omega;
pub use omega::{OmegaWord, Reduced, ReducedParseError};
use tracing::subscriber::SetGlobalDefaultError;

use self::subword::Infix;

#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait LinearWord<S>: Hash + Eq {
    fn nth(&self, position: usize) -> Option<S>;

    /// Returns the first symbol of `self`, if it exists.
    fn first(&self) -> Option<S>
    where
        Self: Sized,
    {
        self.nth(0)
    }

    fn infix(&self, offset: usize, length: usize) -> Infix<'_, S, Self>
    where
        Self: Sized,
    {
        Infix::new(self, offset, length)
    }

    /// Constructs a [`Prefix`] object, which is a finite prefix of `self` that has the given `length`.
    fn prefix(&self, length: usize) -> Infix<'_, S, Self>
    where
        Self: Sized,
    {
        Infix::new(self, 0, length)
    }

    /// Removes the first symbol of `self` and returns it together with the remaining suffix.
    fn pop_first(&self) -> (S, subword::Offset<'_, S, Self>)
    where
        Self: Sized + HasLength<Length = InfiniteLength>,
    {
        let first = self.first().unwrap();
        (first, self.offset(1))
    }

    /// Creates an [`subword::Offset`] object, which is the suffix of `self` that starts at the given `offset`.
    fn offset(&self, offset: usize) -> subword::Offset<'_, S, Self>
    where
        Self: Sized,
    {
        subword::Offset::new(self, offset)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ConsumingInfixIterator<'a, S: Symbol, W: LinearWord<S>> {
    word: &'a W,
    start: usize,
    end: usize,
    _marker: std::marker::PhantomData<S>,
}

impl<'a, S: Symbol, W: LinearWord<S>> LinearWord<S> for ConsumingInfixIterator<'a, S, W> {
    fn nth(&self, position: usize) -> Option<S> {
        todo!()
    }
}

impl<'a, S: Symbol, W: LinearWord<S>> Iterator for ConsumingInfixIterator<'a, S, W> {
    type Item = S;
    fn next(&mut self) -> Option<Self::Item> {
        if self.start < self.end {
            let out = self.word.nth(self.start);
            self.start += 1;
            out
        } else {
            None
        }
    }
}

impl<'a, S: Symbol, W: LinearWord<S>> ConsumingInfixIterator<'a, S, W> {
    pub fn new(word: &'a W, start: usize, end: usize) -> Self {
        Self {
            word,
            start,
            end,
            _marker: std::marker::PhantomData,
        }
    }
}

/// This macro can be used to create a [`OmegaWord`] object from some representation, it is mainly interesting
/// for quickly constructing infinite words without having to go through the [`OmegaWord`] struct.
///
/// There are essentially three distinct variants of using this macro:
/// - `upw!(repr, loopindex index)` creates a word with the given representation and the given loopindex.
/// - `upw!(base, recur)` creates an ultimately word with the representation of `base` followed by the representation of `recur`.
/// - `upw!(recur)` creates a periodic word that is the repetition of `recur`.
#[macro_export]
macro_rules! upw {
    ($recur:expr) => {
        $crate::word::Reduced::periodic($recur)
    };
    ($base:expr, $recur:expr) => {
        $crate::word::Reduced::ultimately_periodic($base, $recur)
    };
}

#[cfg(test)]
mod tests {

    #[test]
    fn macro_upw() {
        let w = upw!("a", "bbbb");
        let ww = upw!("ab", "b");
        // assert_eq!(w.prefix(6).finite_to_vec(), ww.prefix(6).finite_to_vec());
    }
}
