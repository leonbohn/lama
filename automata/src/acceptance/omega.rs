use std::hash::Hash;

use crate::Set;

use super::{Finite, Priority, PriorityMapping};

/// Represents a parity condition as a vector of sets which encode a Zielonka path. The sets form an inclusion chain, where the first set contains the states with the lowest priority, the second set contains the states with the second lowest priority, and so on. The last set contains the states with the highest priority.
/// Note that we are using min-even, meaning the first set contains the most significant even priority, the second set contains the second most significant even priority, and so on.
pub struct ParityCondition<X>(pub Vec<Set<X>>);

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
    X: Eq + std::hash::Hash + Finite,
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

impl<X> PriorityMapping for BuchiCondition<X>
where
    X: Eq + std::hash::Hash,
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
