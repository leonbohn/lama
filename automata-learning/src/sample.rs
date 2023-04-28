use std::{collections::BTreeSet, hash::Hash};

use automata::{
    ts::{HasStates, IntoTransitions, Trivial},
    words::IsInfinite,
    Class, FiniteKind, RightCongruence, Set, Subword, Successor, Symbol, TransitionSystem,
    UltimatelyPeriodicWord, Word, DFA,
};
use itertools::Itertools;

/// Represents a finite sample, which is a pair of positive and negative instances.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Sample<W> {
    pub positive: Set<W>,
    pub negative: Set<W>,
}

impl<W: Eq + Hash> PartialEq for Sample<W> {
    fn eq(&self, other: &Self) -> bool {
        self.positive == other.positive && self.negative == other.negative
    }
}

impl<W: Eq + Hash> Eq for Sample<W> {}

impl<W: IsInfinite> Sample<W> {
    /// Returns the maximum length of the base prefix of any word in the sample.
    pub fn max_base_len(&self) -> usize {
        self.iter().map(|w| w.base_length()).max().unwrap_or(0)
    }

    /// Returns the maximum loop length of any word in the sample.
    pub fn max_recur_len(&self) -> usize {
        self.iter().map(|w| w.recur_length()).max().unwrap_or(0)
    }
}

impl<W: Eq + Hash> Sample<W> {
    /// Creates a new sample from the given data.
    pub fn from_parts(positive: Set<W>, negative: Set<W>) -> Self {
        Self { positive, negative }
    }

    /// Returns the number of elements in the sample.
    pub fn len(&self) -> usize {
        self.positive.len() + self.negative.len()
    }

    /// Checks if the sample is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Creates a new sample from two iterators.
    pub fn from_iters<I, J>(positive: I, negative: J) -> Self
    where
        I: IntoIterator<Item = W>,
        J: IntoIterator<Item = W>,
    {
        Self {
            positive: positive.into_iter().collect(),
            negative: negative.into_iter().collect(),
        }
    }

    /// Iterates over all elements in the sample.
    pub fn iter(&self) -> impl Iterator<Item = &W> {
        self.positive.iter().chain(self.negative.iter())
    }

    /// Iterates just over the positive instances.
    pub fn positive_iter(&self) -> impl Iterator<Item = &W> {
        self.positive.iter()
    }

    /// Iterates just over the negative instances.
    pub fn negative_iter(&self) -> impl Iterator<Item = &W> {
        self.negative.iter()
    }

    /// Iterates over all elements in the sample.
    pub fn annotated_iter(&self) -> impl Iterator<Item = (bool, &W)> {
        self.positive
            .iter()
            .map(|w| (true, w))
            .chain(self.negative.iter().map(|w| (false, w)))
    }
}

impl<W: Word<Kind = FiniteKind>> Sample<W> {}

fn build_prefix_acceptor<S: Symbol>(set: &Set<W>) -> DFA<Class<W::S>, W::S> {
    let mut ts = TransitionSystem::trivial();
    let mut words = set.iter().collect::<Vec<_>>();
}

pub struct Prefixes<'a, S: Symbol> {
    set: &'a Set<UltimatelyPeriodicWord<S>>,
}

impl<'a, S: Symbol> HasStates for Prefixes<'a, S> {
    type Q = Class<S>;

    type States<'me> = std::iter::Map<
        std::collections::btree_set::Iter<'me, UltimatelyPeriodicWord<S>>,
        fn(&'me UltimatelyPeriodicWord<S>) -> &'me Class<S>>
    where
        Self: 'me;

    fn states(&self) -> Self::States<'_> {
        todo!()
    }
}

impl<S: Symbol> Sample<UltimatelyPeriodicWord<S>> {
    pub fn build_separated(&self) -> Self {
        let mut pos = Set::new();
        let mut neg = Set::new();

        let mut queue = self.annotated_iter().collect_vec();
        while let Some((is_positive, word)) = queue.pop() {
            while queue.iter().any(|(_, w)| w.base() == word.base()) {
                word.unroll_one();
            }
            if is_positive {
                pos.insert(word.clone());
            } else {
                neg.insert(word.clone());
            }
        }

        Sample::from_parts(pos, neg)
    }
}

impl<S: Symbol> Sample<UltimatelyPeriodicWord<S>> {
    pub fn positive_prefixes(&self) -> DFA<Class<S>, S> {
        build_prefix_acceptor(&self.positive)
    }

    pub fn negative_prefixes(&self) -> DFA<Class<S>, S> {
        build_prefix_acceptor(&self.negative)
    }
}
