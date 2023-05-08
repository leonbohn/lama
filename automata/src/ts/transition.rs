use std::{borrow::Borrow, hash::Hash};

use impl_tools::autoimpl;

use crate::{StateIndex, Symbol, Value};

/// Abstracts a reference to a state.
pub trait StateReference: Clone {
    /// The type of the state index.
    type Q: StateIndex;

    /// Returns a reference to the state.
    fn state(&self) -> Self::Q;
}

impl<'a, Q: StateIndex> StateReference for &'a Q {
    type Q = Q;

    fn state(&self) -> Self::Q {
        (*self).clone()
    }
}

impl<P: StateIndex, Q: StateIndex> StateReference for crate::Pair<P, Q> {
    type Q = crate::Pair<P, Q>;

    fn state(&self) -> Self::Q {
        self.clone()
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

impl<Q: StateIndex, S: Symbol, O: Value> Trigger for (Q, S, Q, O) {
    type S = S;

    type Q = Q;

    fn source(&self) -> &Self::Q {
        &self.0
    }

    fn sym(&self) -> &Self::S {
        &self.1
    }
}

impl<Q: StateIndex, S: Symbol, O: Value> Transition for (Q, S, Q, O) {
    fn target(&self) -> &Self::Q {
        &self.2
    }
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

/// A reference to a [`Transition`]. This is useful for implementing [`TransitionSystem`] for types that do not own their transitions.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TransitionReference<'a, Q, S> {
    source: &'a Q,
    sym: &'a S,
    target: &'a Q,
}

impl<'a, Q: StateIndex, S: Symbol> TransitionReference<'a, Q, S> {
    /// Creates a new transition reference.
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
