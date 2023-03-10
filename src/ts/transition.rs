use std::hash::Hash;

use crate::{StateIndex, Symbol};

/// A trait for the trigger of a transition. This allows for more generic implementations of [`crate::TransitionSystem`], in preparation for a future implementation that allows for non-deterministic transition systems.
pub trait Trigger: Clone + Eq + Hash {
    /// The symbol type.
    type S;
    /// The state type.
    type Q;

    /// The source state of the transition.
    fn source(&self) -> &Self::Q;
    /// The symbol on which the transition is triggered.
    fn sym(&self) -> &Self::S;
}

/// Abstracts a concrete transition in a transition system. In contrast to a [`TransitionTrigger`] this trait also contains the target state of the transition.
pub trait Transition: Trigger {
    /// The target state of the transition.
    fn to(&self) -> &Self::Q;
}

impl<Q: StateIndex, S: Symbol> Trigger for (Q, S) {
    type S = S;
    type Q = Q;
    fn source(&self) -> &Self::Q {
        &self.0
    }
    fn sym(&self) -> &Self::S {
        &self.1
    }
}

impl<Q: StateIndex, S: Symbol> Trigger for (Q, S, Q) {
    type S = S;

    type Q = Q;

    fn source(&self) -> &Self::Q {
        &self.0
    }

    fn sym(&self) -> &Self::S {
        &self.1
    }
}

impl<Q: StateIndex, S: Symbol> Transition for (Q, S, Q) {
    fn to(&self) -> &Self::Q {
        &self.2
    }
}
