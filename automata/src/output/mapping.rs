use std::hash::Hash;

use itertools::Itertools;

use crate::{Map, Priority, Set, Symbol};

use super::with_output::HasOutput;

/// A mapping from a type `X` to a [`Priority`].
pub trait Mapping {
    /// The domain of the mapping.
    type Domain: Clone + Hash + Eq;

    /// The type of values that are produced.
    type Range: Clone + Hash + Eq;

    type RangeIter<'me>: Iterator<Item = &'me Self::Range>
    where
        Self: 'me;

    fn range_iter(&self) -> Self::RangeIter<'_>;

    fn range(&self) -> itertools::Unique<Self::RangeIter<'_>> {
        self.range_iter().unique()
    }

    /// Obtains the priority of the given element.
    fn get_value(&self, of: &Self::Domain) -> Self::Range;

    /// Returns the set of all priorities.
    fn as_set_range(&self) -> Set<Self::Range> {
        self.range().cloned().collect()
    }

    /// Returns the number of distinct priorities.
    fn complexity(&self) -> usize {
        self.as_set_range().len()
    }
}

/// A special kind of [`Mapping`] where the range is a [`Priority`].
pub trait PriorityMapping: Mapping<Range = Priority> {}
impl<M: Mapping<Range = Priority>> PriorityMapping for M {}

/// A mutable mapping from a type `X` to a [`Priority`].
pub trait MutableMapping: Mapping {
    /// Sets the priority of the given element to the given priority.
    fn set_value(&mut self, of: &Self::Domain, to: Self::Range) -> Option<Self::Range>;
}

impl<I, O> Mapping for Map<I, O>
where
    I: Clone + Hash + Eq,
    O: Clone + Hash + Eq,
{
    type Domain = I;

    type Range = O;

    type RangeIter<'me> = std::collections::hash_map::Values<'me, I, O>
    where
        Self: 'me;

    fn get_value(&self, _of: &Self::Domain) -> Self::Range {
        todo!()
    }
    fn range_iter(&self) -> Self::RangeIter<'_> {
        self.values()
    }
}

impl<I, O: Symbol> HasOutput for Map<I, O> {
    type Gamma = O;

    type Output<'me> = std::collections::hash_map::Values<'me, I, O> where Self:'me;

    fn output_alphabet_iter(&self) -> Self::Output<'_> {
        self.values()
    }
}

impl<I, O> MutableMapping for Map<I, O>
where
    I: Clone + Hash + Eq,
    O: Clone + Hash + Eq,
{
    fn set_value(&mut self, of: &Self::Domain, to: Self::Range) -> Option<Self::Range> {
        self.insert(of.clone(), to)
    }
}
