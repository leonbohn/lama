use crate::{coloring::Coloring, FiniteKind};

use super::AcceptanceCondition;

pub struct ReachabilityAcceptance<Col> {
    pub(crate) accepting: Col,
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
