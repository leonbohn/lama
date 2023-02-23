mod omega;
mod reachability;
mod safety;

/// Can verify for a given induced object whether it satisfies the condition or not.
pub trait AcceptanceCondition {
    type Induced;
    fn is_accepting(&self, induced: &Self::Induced) -> bool;
}
