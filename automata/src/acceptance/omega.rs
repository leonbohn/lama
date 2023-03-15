use std::{
    hash::Hash,
    ops::{AddAssign, SubAssign},
};

use crate::Set;

use super::{AcceptanceCondition, Finite, Parity, Priority, PriorityMapping};

/// Represents a parity condition as a vector of sets which encode a Zielonka path. The sets form an inclusion chain, where the first set contains the states with the lowest priority, the second set contains the states with the second lowest priority, and so on. The last set contains the states with the highest priority.
/// Note that we are using min-even, meaning the first set contains the most significant even priority, the second set contains the second most significant even priority, and so on.
pub struct ParityCondition<X>(pub Vec<Set<X>>);

impl<X: Eq + Hash> AcceptanceCondition for ParityCondition<X> {
    type Induced = Set<X>;

    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        if let Some(minimum) = induced.iter().map(|x| self.priority(x)).min() {
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
}

impl<X: Eq + Hash + Finite> ParityCondition<X> {
    /// Creates a new `ParityCondition` from the given mapping. *EXPENSIVE*
    pub fn from_mapping<C: PriorityMapping<X = X>>(condition: C) -> Self {
        let mut out = (0..condition.complexity())
            .map(|_| Set::new())
            .collect::<Vec<_>>();
        for x in X::universe() {
            out[condition.priority(&x).0 as usize].insert(x);
        }

        Self(out)
    }
}

impl<X: Eq + Hash> Default for ParityCondition<X> {
    fn default() -> Self {
        Self::new(vec![Set::new()])
    }
}

impl<X> PriorityMapping for ParityCondition<X>
where
    X: Eq + std::hash::Hash,
{
    type X = X;

    fn priority(&self, of: &Self::X) -> Priority {
        for (i, set) in self.0.iter().enumerate() {
            if set.contains(of) {
                return Priority(i as u32);
            }
        }
        Priority(self.complexity())
    }

    fn universe(&self) -> Set<Priority> {
        (0..self.complexity()).map(Priority).collect()
    }

    fn complexity(&self) -> u32 {
        self.0.len() as u32
    }
}

/// Represents a Buchi condition which marks a set of transitions as accepting.
pub struct BuchiCondition<X>(pub Set<X>);

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

impl<X> PriorityMapping for BuchiCondition<X>
where
    X: Eq + Hash,
{
    type X = X;

    fn priority(&self, of: &Self::X) -> Priority {
        if self.0.contains(of) {
            Priority(0)
        } else {
            Priority(1)
        }
    }

    fn universe(&self) -> Set<Priority> {
        vec![Priority(0), Priority(1)].into_iter().collect()
    }

    fn complexity(&self) -> u32 {
        2
    }
}

impl<X: Hash + Eq> Default for BuchiCondition<X> {
    fn default() -> Self {
        Self(Set::new())
    }
}
