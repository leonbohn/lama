use std::hash::Hash;

use crate::Set;

use super::{Finite, Priority, PriorityMapping};

pub struct ParityCondition<X>(pub Vec<Set<X>>);

impl<X: Eq + Hash> ParityCondition<X> {
    pub fn new(parity: Vec<Set<X>>) -> Self {
        Self(parity)
    }
}

impl<X: Eq + Hash + Finite> ParityCondition<X> {
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
        unreachable!("Priority mapping is assumed to be total.")
    }

    fn universe(&self) -> Set<Priority> {
        (0..self.complexity()).map(Priority).collect()
    }

    fn complexity(&self) -> u32 {
        todo!()
    }
}

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
