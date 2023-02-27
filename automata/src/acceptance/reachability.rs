use std::{collections::HashSet, hash::Hash};

use crate::{coloring::Coloring, FiniteKind};

use super::AcceptanceCondition;

/// Abstracts reachability conditions, the contained coloring is used to determine whether a state is accepting.
pub struct ReachabilityAcceptance<Col> {
    pub(crate) accepting: Col,
}

impl<Col: Eq + Hash> FromIterator<Col> for ReachabilityAcceptance<HashSet<Col>> {
    fn from_iter<I: IntoIterator<Item = Col>>(iter: I) -> Self {
        Self {
            accepting: iter.into_iter().collect(),
        }
    }
}

impl<Id, Col> AcceptanceCondition for ReachabilityAcceptance<Col>
where
    Col: Coloring<Input = Id, Output = bool>,
{
    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        self.accepting.color(induced)
    }

    type Induced = Id;

    type Kind = FiniteKind;
}

/// Abstracts safety conditions, the contained coloring is used to determine whether a state is rejecting.
pub struct SafetyAcceptance<Col> {
    pub(crate) rejecting: Col,
}

impl<Id, Col> AcceptanceCondition for SafetyAcceptance<Col>
where
    Col: Coloring<Input = Id, Output = bool>,
{
    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        !self.rejecting.color(induced)
    }

    type Induced = Id;

    type Kind = FiniteKind;
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
        let aut = initialized.with_acceptance(acc);

        assert!(!aut.accepts(&FiniteWord::from("ab")));
        assert!(aut.accepts(&FiniteWord::from("aa")));
    }
}
