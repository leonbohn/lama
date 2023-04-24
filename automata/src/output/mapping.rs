use std::hash::Hash;

use crate::{Map, Priority, Set};

/// A mapping from a type `X` to a [`Priority`].
pub trait Mapping {
    /// The domain of the mapping.
    type Domain: Clone + Hash + Eq;

    /// The type of values that are produced.
    type Range: Clone + Hash + Eq;

    /// Obtains the priority of the given element.
    fn get_value(&self, of: &Self::Domain) -> Self::Range;

    /// Returns the set of all priorities.
    fn universe(&self) -> Set<Self::Range>;

    /// Returns the set of all elements that can be mapped.
    fn domain(&self) -> Set<Self::Domain>;

    /// Returns the number of distinct priorities.
    fn complexity(&self) -> usize {
        self.universe().len()
    }

    /// Applies the given function to map the range.
    fn map_range<X, F: Fn(Self::Range) -> X>(&mut self, f: F) -> Map<Self::Domain, X>
    where
        Self::Domain: Clone + Eq + Hash,
    {
        self.domain()
            .iter()
            .map(|x| (x.clone(), f(self.get_value(x))))
            .collect()
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

    fn get_value(&self, _of: &Self::Domain) -> Self::Range {
        todo!()
    }

    fn universe(&self) -> Set<Self::Range> {
        todo!()
    }

    fn domain(&self) -> Set<Self::Domain> {
        todo!()
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
