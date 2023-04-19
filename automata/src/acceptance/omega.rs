use std::{
    fmt::Display,
    hash::Hash,
    ops::{AddAssign, SubAssign},
};

use itertools::Itertools;

use crate::{
    output::{Mapping, Parity, Priority, PriorityMapping},
    Set,
};

use super::{AcceptanceCondition, Finite};

/// Represents an omega acceptance condition.
pub enum OmegaCondition<X> {
    /// This variant encapsulates a [`ParityCondition`].
    Parity(ParityCondition<X>),
    /// Encapsulates a [`BuchiCondition`].
    Buchi(BuchiCondition<X>),
}

impl<X: Clone + Hash + Eq> AcceptanceCondition for OmegaCondition<X> {
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

impl<X: Hash + Eq + Clone> ToOmega for ParityCondition<X> {
    type X = X;
    fn to_omega(&self) -> OmegaCondition<Self::X> {
        OmegaCondition::Parity(self.clone())
    }
}

impl<X: Eq + Hash + Clone> AcceptanceCondition for ParityCondition<X> {
    type Induced = Set<X>;

    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        if let Some(minimum) = induced.iter().map(|x| self.get_value(x)).min() {
            minimum.parity()
        } else {
            false
        }
    }
}

impl<X: Eq + Hash> ParityCondition<X> {
    /// Creates a new `ParityCondition` from the given vector of sets.
    pub fn new(parity: Vec<Set<X>>) -> Self {
        Self(parity)
    }

    /// Returns the number of priorities in the condition.
    pub fn priorities(&self) -> usize {
        self.0.len()
    }
}

impl<X: Eq + Hash + Finite> ParityCondition<X> {
    /// Creates a new `ParityCondition` from the given mapping. *EXPENSIVE*
    pub fn from_mapping<C: PriorityMapping<Domain = X>>(condition: C) -> Self {
        let mut out = (0..condition.complexity())
            .map(|_| Set::new())
            .collect::<Vec<_>>();
        for x in X::universe() {
            out[condition.get_value(&x).as_usize()].insert(x);
        }

        Self(out)
    }
}

impl<X: Eq + Hash> Default for ParityCondition<X> {
    fn default() -> Self {
        Self::new(vec![Set::new()])
    }
}

pub struct ParityConditionDomainIter<'a, X>(&'a ParityCondition<X>);

impl<X> Mapping for ParityCondition<X>
where
    X: Clone + Eq + std::hash::Hash,
{
    type Domain = X;

    type Range = Priority;

    fn get_value(&self, of: &Self::Domain) -> Priority {
        for (i, set) in self.0.iter().enumerate() {
            if set.contains(of) {
                return Priority(i as u32);
            }
        }
        Priority(self.complexity() as u32)
    }

    fn universe(&self) -> Set<Priority> {
        (0..self.0.len()).map(|i| Priority(i as u32)).collect()
    }

    fn domain(&self) -> Set<Self::Domain> {
        self.0.iter().flatten().cloned().collect()
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

impl<X> Mapping for BuchiCondition<X>
where
    X: Eq + Hash + Clone,
{
    type Domain = X;
    type Range = bool;

    fn get_value(&self, of: &Self::Domain) -> Self::Range {
        if self.0.contains(of) {
            true
        } else {
            false
        }
    }

    fn universe(&self) -> Set<Self::Range> {
        vec![false, true].into_iter().collect()
    }

    fn domain(&self) -> Set<Self::Domain> {
        self.0.clone()
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
