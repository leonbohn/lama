use std::hash::Hash;

use crate::{Alphabet, Pair, StateIndex, Triple};

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

    /// Decomposes the trigger into its parts
    fn decompose(&self) -> (&Self::Q, &Self::S) {
        (self.source(), self.sym())
    }

    /// Creates a new trigger from its parts
    fn create(q: Self::Q, a: Self::S) -> Self;
}

impl<Q: StateIndex, S: Alphabet> Trigger for Pair<S, Q> {
    type S = S;
    type Q = Q;
    fn source(&self) -> &Self::Q {
        &self.0
    }
    fn sym(&self) -> &Self::S {
        &self.1
    }

    fn create(q: Self::Q, a: Self::S) -> Self {
        Self(q, a)
    }
}

impl<Q, S> From<(Q, S)> for Pair<S, Q> {
    fn from((q, s): (Q, S)) -> Self {
        Self(q, s)
    }
}

impl<Q: StateIndex, S: Alphabet> Trigger for (Q, S) {
    type S = S;
    type Q = Q;
    fn source(&self) -> &Self::Q {
        &self.0
    }
    fn sym(&self) -> &Self::S {
        &self.1
    }

    fn create(q: Self::Q, a: Self::S) -> Self {
        (q, a)
    }
}

/// Abstracts a concrete transition in a transition system. In contrast to a [`TransitionTrigger`] this trait also contains the target state of the transition.
pub trait Transition {
    /// The symbol type.
    type S;
    /// The state type.
    type Q;
    /// The source state of the transition.
    fn from(&self) -> &Self::Q;
    /// The target state of the transition.
    fn to(&self) -> &Self::Q;
    /// The symbol on which the transition is triggered.
    fn symbol(&self) -> &Self::S;
}

impl<T: Transition> Transition for &T {
    type S = T::S;
    type Q = T::Q;
    fn from(&self) -> &Self::Q {
        (*self).from()
    }
    fn to(&self) -> &Self::Q {
        (*self).to()
    }
    fn symbol(&self) -> &Self::S {
        (*self).symbol()
    }
}

impl<S, Q> From<(Q, S, Q)> for Triple<S, Q> {
    fn from((from, on, to): (Q, S, Q)) -> Self {
        Self(from, on, to)
    }
}

impl<Q, S> From<Triple<S, Q>> for (Q, S, Q) {
    fn from(transition: Triple<S, Q>) -> Self {
        (transition.0, transition.1, transition.2)
    }
}

impl<Q, S> Transition for Triple<S, Q>
where
    S: Clone + Eq + std::hash::Hash + std::fmt::Debug,
{
    type S = S;

    type Q = Q;

    fn from(&self) -> &Self::Q {
        &self.0
    }

    fn to(&self) -> &Self::Q {
        &self.2
    }

    fn symbol(&self) -> &Self::S {
        &self.1
    }
}

impl<S: PartialOrd, Q: PartialOrd> PartialOrd for Triple<S, Q> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0).and_then(|ord| {
            if ord == std::cmp::Ordering::Equal {
                self.1.partial_cmp(&other.1).and_then(|ord| {
                    if ord == std::cmp::Ordering::Equal {
                        self.2.partial_cmp(&other.2)
                    } else {
                        Some(ord)
                    }
                })
            } else {
                Some(ord)
            }
        })
    }
}

impl<S: Ord, Q: Ord> Ord for Triple<S, Q> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .cmp(&other.0)
            .then(self.1.cmp(&other.1))
            .then(self.2.cmp(&other.2))
    }
}

impl<T: Transition> From<T> for Pair<T::S, T::Q>
where
    T::Q: Clone,
    T::S: Clone,
{
    fn from(transition: T) -> Self {
        Pair(transition.from().clone(), transition.symbol().clone())
    }
}
