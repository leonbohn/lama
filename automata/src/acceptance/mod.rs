use crate::Boundedness;

mod omega;
mod reachability;
pub use omega::{BuchiAcceptance, ParityAcceptance};
pub use reachability::{ReachabilityAcceptance, SafetyAcceptance};

/// Can verify for a given induced object whether it satisfies the condition or not.
pub trait AcceptanceCondition {
    /// Used to disambiguate between acceptance conditions for finite and infinite inputs.
    type Kind: Boundedness;
    /// The type of the induced object, depends on [`Self::Kind`].
    type Induced;

    /// Returns whether the given induced object satisfies the acceptance condition.
    fn is_accepting(&self, induced: &Self::Induced) -> bool;
}

/// Helper trait to get the acceptance condition of a transition system.
pub trait HasAcceptanceCondition {
    /// The acceptance condition type.
    type AcceptanceCondition: AcceptanceCondition;
    /// Get the acceptance condition.
    fn acceptance_condition(&self) -> &Self::AcceptanceCondition;
}

impl<HAcc: HasAcceptanceCondition> AcceptanceCondition for HAcc {
    type Kind = <HAcc::AcceptanceCondition as AcceptanceCondition>::Kind;

    type Induced = <HAcc::AcceptanceCondition as AcceptanceCondition>::Induced;

    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        self.acceptance_condition().is_accepting(induced)
    }
}
