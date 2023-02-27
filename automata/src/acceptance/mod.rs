use crate::Boundedness;

mod omega;

/// Can verify for a given induced object whether it satisfies the condition or not.
pub trait AcceptanceCondition {
    /// Used to disambiguate between acceptance conditions for finite and infinite inputs.
    type Kind: Boundedness;
    /// The type of the induced object, depends on [`Self::Kind`].
    type Induced;

    /// Returns whether the given induced object satisfies the acceptance condition.
    fn is_accepting(&self, induced: &Self::Induced) -> bool;
}
