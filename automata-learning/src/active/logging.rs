use super::table::{LStarExample, LStarExampleFor, LStarTarget};
use automata::prelude::*;
use itertools::Itertools;
use std::fmt::Debug;

#[derive(Clone)]
pub enum LStarQuery<const FOR_MEALY: bool, T: LStarTarget<FOR_MEALY>> {
    Membership(LStarExample<T::Alphabet, T::Output>),
    Equivalence(T::Hypothesis, Option<LStarExample<T::Alphabet, T::Output>>),
}

impl<const FOR_MEALY: bool, T> std::fmt::Debug for LStarQuery<FOR_MEALY, T>
where
    T: LStarTarget<FOR_MEALY>,
    T::Hypothesis: Debug,
    T::Output: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Membership((w, c)) => write!(f, "Membership({:?}) = {:?}", w, c),
            Self::Equivalence(hyp, cex) => match cex {
                Some((w, c)) => write!(f, "Equivalence({:?}, {:?}) = {:?}", hyp, w, c),
                None => write!(f, "Equivalence({:?}) succeeded", hyp),
            },
        }
    }
}

impl<const FOR_MEALY: bool, T: LStarTarget<FOR_MEALY>> LStarQuery<FOR_MEALY, T> {
    pub fn is_equivalence(&self) -> bool {
        matches!(self, LStarQuery::Equivalence(_, _))
    }

    pub fn is_successful_equivalence(&self) -> bool {
        matches!(self, LStarQuery::Equivalence(_, None))
    }

    pub fn example(&self) -> Option<LStarExample<T::Alphabet, T::Output>> {
        match self {
            LStarQuery::Membership(ex) => Some(ex.clone()),
            LStarQuery::Equivalence(hyp, ex) => ex.clone(),
            _ => None,
        }
    }
}

pub trait LStarLogger<const FOR_MEALY: bool, T: LStarTarget<FOR_MEALY>> {
    fn log(&mut self, query: LStarQuery<FOR_MEALY, T>);
    fn create() -> Self;
}

impl<const FOR_MEALY: bool, T: LStarTarget<FOR_MEALY>> LStarLogger<FOR_MEALY, T> for () {
    fn log(&mut self, query: LStarQuery<FOR_MEALY, T>) {}

    fn create() -> Self {}
}

#[derive(Clone)]
pub struct LStarLogbook<const FOR_MEALY: bool, T: LStarTarget<FOR_MEALY>>(
    Vec<LStarQuery<FOR_MEALY, T>>,
);

impl<const FOR_MEALY: bool, T: LStarTarget<FOR_MEALY>> LStarLogbook<FOR_MEALY, T> {
    pub fn is_sane(&self) -> bool {
        match self
            .0
            .iter()
            .map(|e| match e {
                LStarQuery::Equivalence(_, None) => 1,
                _ => 0,
            })
            .reduce(|acc, x| acc + x)
            .unwrap_or(0)
        {
            0 => true,
            1 => self.0.last().unwrap().is_successful_equivalence(),
            _ => unreachable!("This should never be possible"),
        }
    }

    pub fn examples(&self) -> impl Iterator<Item = LStarExample<T::Alphabet, T::Output>> + '_ {
        assert!(self.is_sane());
        self.0.iter().filter_map(|e| e.example()).unique()
    }
}

impl<const FOR_MEALY: bool, T: LStarTarget<FOR_MEALY>> LStarLogger<FOR_MEALY, T>
    for LStarLogbook<FOR_MEALY, T>
{
    fn log(&mut self, query: LStarQuery<FOR_MEALY, T>) {
        assert!(self.is_sane());
        self.0.push(query);
    }

    fn create() -> Self {
        Self(vec![])
    }
}
