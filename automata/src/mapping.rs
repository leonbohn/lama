use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    hash::Hash,
};

/// A mapping associates elements of one type with elements of another type. Note, that we assume
/// mappings to be total, i.e. every input element is mapped to an output element.
pub trait Mapping<Input> {
    /// The type of the output elements.
    type Output;

    /// Applies the mapping to the given input and returns the output. Assumes the mapping to be total
    /// and may panic if this is not the case.
    fn apply<I: Borrow<Input>>(&self, input: I) -> Self::Output;
}

impl<In: PartialEq, Out: Clone> Mapping<In> for Vec<(In, Out)> {
    type Output = Out;

    fn apply<I: Borrow<In>>(&self, input: I) -> Self::Output {
        self.iter()
            .find(|(i, _)| i == input.borrow())
            .map(|(_, o)| o)
            .unwrap()
            .clone()
    }
}

impl<In: Eq + Hash, Out: Clone> Mapping<In> for HashMap<In, Out> {
    type Output = Out;

    fn apply<I: Borrow<In>>(&self, input: I) -> Self::Output {
        self.get(input.borrow()).unwrap().clone()
    }
}

impl<In: Eq + Hash> Mapping<In> for HashSet<In> {
    type Output = bool;

    fn apply<I: Borrow<In>>(&self, input: I) -> Self::Output {
        self.contains(input.borrow())
    }
}
