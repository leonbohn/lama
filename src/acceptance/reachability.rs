use std::{
    hash::Hash,
    ops::{AddAssign, SubAssign},
};

use ahash::AHashSet;

use crate::FiniteKind;

use super::AcceptanceCondition;

/// Abstracts reachability conditions, the contained coloring is used to determine whether a state is accepting.
pub struct ReachabilityCondition<Id> {
    pub(crate) accepting: AHashSet<Id>,
}

impl<Id: Eq + Hash> ReachabilityCondition<Id> {
    /// Creates a new `ReachabilityAcceptance` object from the given set of accepting states.
    pub fn new(accepting: AHashSet<Id>) -> Self {
        Self { accepting }
    }

    /// Sets the given `state` as accepting.
    pub fn set_accepting(&mut self, state: Id) {
        self.accepting.insert(state);
    }
}

impl<C> AddAssign<C> for ReachabilityCondition<C>
where
    C: Eq + Hash,
{
    fn add_assign(&mut self, rhs: C) {
        self.accepting.insert(rhs);
    }
}

impl<C> SubAssign<C> for ReachabilityCondition<C>
where
    C: Eq + Hash,
{
    fn sub_assign(&mut self, rhs: C) {
        self.accepting.remove(&rhs);
    }
}

impl<C: Default> Default for ReachabilityCondition<C> {
    fn default() -> Self {
        Self {
            accepting: Default::default(),
        }
    }
}

impl<Col: Eq + Hash> FromIterator<Col> for ReachabilityCondition<Col> {
    fn from_iter<I: IntoIterator<Item = Col>>(iter: I) -> Self {
        Self {
            accepting: iter.into_iter().collect(),
        }
    }
}

impl<Id: Hash + Eq> AcceptanceCondition for ReachabilityCondition<Id> {
    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        self.accepting.contains(induced)
    }

    type Induced = Id;

    type Kind = FiniteKind;
}

/// Abstracts safety conditions, the contained coloring is used to determine whether a state is rejecting.
pub struct SafetyAcceptance<Col> {
    pub(crate) rejecting: Col,
}

impl<Col: Default> Default for SafetyAcceptance<Col> {
    fn default() -> Self {
        Self {
            rejecting: Default::default(),
        }
    }
}

impl<C> std::ops::Not for SafetyAcceptance<C> {
    type Output = SafetyAcceptance<C>;

    fn not(self) -> Self::Output {
        SafetyAcceptance {
            rejecting: self.rejecting,
        }
    }
}

impl<C> std::ops::Not for ReachabilityCondition<C> {
    type Output = ReachabilityCondition<C>;

    fn not(self) -> Self::Output {
        ReachabilityCondition {
            accepting: self.accepting,
        }
    }
}
