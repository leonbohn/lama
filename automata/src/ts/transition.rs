use std::{borrow::Borrow, hash::Hash};

use impl_tools::autoimpl;

use crate::{StateIndex, Symbol};

pub trait StateReference {
    type Q: StateIndex;

    fn state(&self) -> &Self::Q;
}

impl<'a, Q: StateIndex> StateReference for &'a Q {
    type Q = Q;

    fn state(&self) -> &Self::Q {
        self
    }
}

/// A trait for the trigger of a transition. This allows for more generic implementations of [`crate::TransitionSystem`], in preparation for a future implementation that allows for non-deterministic transition systems.
#[autoimpl(for<T: trait> &T)]
pub trait Trigger: Clone + Eq + Hash {
    /// The symbol type.
    type S: Symbol;
    /// The state type.
    type Q: StateIndex;

    /// The source state of the transition.
    fn source(&self) -> &Self::Q;
    /// The symbol on which the transition is triggered.
    fn sym(&self) -> &Self::S;
}

/// Abstracts a concrete transition in a transition system. In contrast to a [`TransitionTrigger`] this trait also contains the target state of the transition.
#[autoimpl(for<T: trait> &T)]
pub trait Transition: Trigger {
    /// The target state of the transition.
    fn target(&self) -> &Self::Q;
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
    fn target(&self) -> &Self::Q {
        &self.2
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TransitionReference<'a, Q, S> {
    source: &'a Q,
    sym: &'a S,
    target: &'a Q,
}

impl<'a, Q: StateIndex, S: Symbol> TransitionReference<'a, Q, S> {
    pub fn new(source: &'a Q, sym: &'a S, target: &'a Q) -> Self {
        Self {
            source,
            sym,
            target,
        }
    }
}

impl<'a, Q: StateIndex, S: Symbol> Trigger for TransitionReference<'a, Q, S> {
    type S = S;

    type Q = Q;

    fn source(&self) -> &Self::Q {
        self.source
    }

    fn sym(&self) -> &Self::S {
        self.sym
    }
}

impl<'a, Q: StateIndex, S: Symbol> Transition for TransitionReference<'a, Q, S> {
    fn target(&self) -> &Self::Q {
        self.target
    }
}
