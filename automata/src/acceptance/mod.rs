use crate::Boundedness;

mod omega;

/// Can verify for a given induced object whether it satisfies the condition or not.
pub trait AcceptanceCondition {
    type Kind: Boundedness;
    type Induced;
    fn is_accepting(&self, induced: &Self::Induced) -> bool;
}
