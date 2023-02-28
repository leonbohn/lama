use std::hash::Hash;

use ahash::AHashSet;

use crate::FiniteKind;

use super::AcceptanceCondition;

/// Abstracts reachability conditions, the contained coloring is used to determine whether a state is accepting.
pub struct ReachabilityAcceptance<Id> {
    pub(crate) accepting: AHashSet<Id>,
}

impl<C: Default> Default for ReachabilityAcceptance<C> {
    fn default() -> Self {
        Self {
            accepting: Default::default(),
        }
    }
}

impl<Col: Eq + Hash> FromIterator<Col> for ReachabilityAcceptance<Col> {
    fn from_iter<I: IntoIterator<Item = Col>>(iter: I) -> Self {
        Self {
            accepting: iter.into_iter().collect(),
        }
    }
}

impl<Id: Hash + Eq> AcceptanceCondition for ReachabilityAcceptance<Id> {
    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        self.accepting.contains(induced)
    }

    type Induced = Id;
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

impl<C> std::ops::Not for ReachabilityAcceptance<C> {
    type Output = ReachabilityAcceptance<C>;

    fn not(self) -> Self::Output {
        ReachabilityAcceptance {
            accepting: self.accepting,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ts::{deterministic::Deterministic, Growable, TransitionSystem},
        words::FiniteWord,
        Acceptor,
    };

    use super::*;

    #[test]
    fn reachability_acceptance() {
        let mut ts = Deterministic::new();
        let q0 = ts.add_state();
        let q1 = ts.add_state();
        let q2 = ts.add_state();
        ts.add_transition(q0, 'a', q1);
        ts.add_transition(q0, 'b', q0);
        ts.add_transition(q1, 'a', q2);
        ts.add_transition(q1, 'b', q0);
        ts.add_transition(q2, 'b', q0);

        let acc = ReachabilityAcceptance::from_iter(vec![q2]);
        let initialized = ts.with_initial(q0);
        // TODO contiue
    }
}
