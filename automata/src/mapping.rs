use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    hash::Hash,
};

pub trait Mapping {
    type Input;
    type Output;

    fn apply<I: Borrow<Self::Input>>(&self, input: I) -> Self::Output;
}

impl<In: PartialEq, Out: Clone> Mapping for Vec<(In, Out)> {
    type Input = In;
    type Output = Out;

    fn apply<I: Borrow<Self::Input>>(&self, input: I) -> Self::Output {
        self.iter()
            .find(|(i, _)| i == input.borrow())
            .map(|(_, o)| o)
            .unwrap()
            .clone()
    }
}

impl<In: Eq + Hash, Out: Clone> Mapping for HashMap<In, Out> {
    type Input = In;
    type Output = Out;

    fn apply<I: Borrow<Self::Input>>(&self, input: I) -> Self::Output {
        self.get(input.borrow()).unwrap().clone()
    }
}

impl<In: Eq + Hash> Mapping for HashSet<In> {
    type Input = In;
    type Output = bool;

    fn apply<I: Borrow<Self::Input>>(&self, input: I) -> Self::Output {
        self.contains(input.borrow())
    }
}
