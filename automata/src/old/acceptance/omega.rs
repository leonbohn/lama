use std::{
    fmt::Display,
    hash::Hash,
    ops::{AddAssign, SubAssign},
};

use itertools::Itertools;

use crate::{
    output::{Parity, Priority, Transformer},
    Set, Value,
};

use super::AcceptanceCondition;

/// Represents an omega acceptance condition.
pub enum OmegaCondition<X> {
    /// This variant encapsulates a [`ParityCondition`].
    Parity(ParityCondition<X>),
    /// Encapsulates a [`BuchiCondition`].
    Buchi(BuchiCondition<X>),
}

impl<X: Value> AcceptanceCondition for OmegaCondition<X> {
    type Induced = Set<X>;

    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        match self {
            Self::Parity(parity) => parity.is_accepting(induced),
            Self::Buchi(buchi) => buchi.is_accepting(induced),
        }
    }
}

pub trait ToOmega {
    type X: Hash + Eq + Clone;
    fn to_omega(&self) -> OmegaCondition<Self::X>;
}

/// Represents a parity condition as a vector of sets which encode a Zielonka path. The sets form an inclusion chain, where the first set contains the states with the lowest priority, the second set contains the states with the second lowest priority, and so on. The last set contains the states with the highest priority.
/// Note that we are using min-even, meaning the first set contains the most significant even priority, the second set contains the second most significant even priority, and so on.
#[derive(Debug, Clone)]
pub struct ParityCondition<X>(pub Vec<Set<X>>);

impl<X: Value> ToOmega for ParityCondition<X> {
    type X = X;
    fn to_omega(&self) -> OmegaCondition<Self::X> {
        OmegaCondition::Parity(self.clone())
    }
}

impl<X: Value> AcceptanceCondition for ParityCondition<X> {
    type Induced = Set<X>;

    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        if let Some(minimum) = induced.iter().map(|x| self.apply(x)).min() {
            minimum.parity()
        } else {
            false
        }
    }
}

impl<X: Value> ParityCondition<X> {
    /// Creates a new `ParityCondition` from the given vector of sets.
    pub fn new(parity: Vec<Set<X>>) -> Self {
        Self(parity)
    }

    /// Returns the number of priorities in the condition.
    pub fn priorities(&self) -> usize {
        self.0.len()
    }
}

impl<X: Value> Default for ParityCondition<X> {
    fn default() -> Self {
        Self::new(vec![Set::new()])
    }
}

pub struct ParityConditionRangeIter<'a> {
    priorities: Vec<&'a Priority>,
    pos: usize,
}

impl<'a> Iterator for ParityConditionRangeIter<'a> {
    type Item = &'a Priority;

    fn next(&mut self) -> Option<Self::Item> {
        self.priorities.get(self.pos).map(|p| {
            self.pos += 1;
            *p
        })
    }
}

impl<X> Transformer for ParityCondition<X>
where
    X: Value,
{
    type Range = usize;
    type Domain = X;

    fn apply<R: std::borrow::Borrow<X>>(&self, input: R) -> usize {
        self.0
            .iter()
            .enumerate()
            .find_map(|(i, set)| {
                if set.contains(input.borrow()) {
                    Some(i)
                } else {
                    None
                }
            })
            .unwrap_or(self.0.len())
    }
}

/// Represents a Buchi condition which marks a set of transitions as accepting.
#[derive(Debug, Clone)]
pub struct BuchiCondition<X>(pub Set<X>);

impl<X: Hash + Eq + Clone> ToOmega for BuchiCondition<X> {
    type X = X;
    fn to_omega(&self) -> OmegaCondition<Self::X> {
        OmegaCondition::Buchi(self.clone())
    }
}

impl<X: Eq + Hash> AcceptanceCondition for BuchiCondition<X> {
    type Induced = Set<X>;

    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        induced.iter().any(|x| self.0.contains(x))
    }
}

impl<X: Eq + Hash> BuchiCondition<X> {
    /// Creates a new `BuchiCondition` from the given set.
    pub fn new(buchi: Set<X>) -> Self {
        Self(buchi)
    }

    /// Marks the given transition as accepting.
    pub fn set_accepting(&mut self, x: X) {
        self.0.insert(x);
    }

    /// Marks the given transition as non-accepting.
    pub fn unset_accepting(&mut self, x: &X) {
        self.0.remove(x);
    }
}

impl<X: Eq + Hash, I: Into<X>> AddAssign<I> for BuchiCondition<X> {
    fn add_assign(&mut self, rhs: I) {
        self.set_accepting(rhs.into())
    }
}

impl<X: Eq + Hash> SubAssign<X> for BuchiCondition<X> {
    fn sub_assign(&mut self, rhs: X) {
        self.unset_accepting(&rhs)
    }
}

pub struct BuchiConditionRangeIter<'a>(&'a std::ops::Range<usize>);

impl<X> Transformer for BuchiCondition<X>
where
    X: Value,
{
    type Range = bool;
    type Domain = X;

    fn apply<R: std::borrow::Borrow<X>>(&self, input: R) -> bool {
        self.0.contains(input.borrow())
    }
}

impl<X: Hash + Eq> Default for BuchiCondition<X> {
    fn default() -> Self {
        Self(Set::new())
    }
}

impl<X, Y> Display for BuchiCondition<(X, Y)>
where
    X: Display,
    Y: Display,
    (X, Y): Ord,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Buchi({})",
            self.0
                .iter()
                .sorted()
                .map(|(x, y)| format!("({}, {})", x, y))
                .join(", ")
        )
    }
}

impl<X, Y> Display for ParityCondition<(X, Y)>
where
    X: Display,
    Y: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Parity({})",
            self.0
                .iter()
                .enumerate()
                .map(|(i, set)| {
                    format!(
                        "[{i} => {}]",
                        set.iter()
                            .map(|(x, y)| format!("({}, {})", x, y))
                            .join(", ")
                    )
                })
                .join(" ")
        )
    }
}