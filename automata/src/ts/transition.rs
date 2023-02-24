pub trait TransitionTarget {
    type Output;
    fn is_deterministic() -> bool;
}

pub trait TransitionTrigger {
    type S;
    type Q;
    fn from(&self) -> &Self::Q;
    fn on(&self) -> &Self::S;
}

impl<T: TransitionTrigger> TransitionTrigger for &T {
    type S = T::S;
    type Q = T::Q;
    fn from(&self) -> &Self::Q {
        (*self).from()
    }
    fn on(&self) -> &Self::S {
        (*self).on()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Trigger<S, Q>(pub Q, pub S);

impl<Q, S> TransitionTrigger for Trigger<S, Q> {
    type S = S;
    type Q = Q;
    fn from(&self) -> &Self::Q {
        &self.0
    }
    fn on(&self) -> &Self::S {
        &self.1
    }
}

impl<Q, S> From<(Q, S)> for Trigger<S, Q> {
    fn from((q, s): (Q, S)) -> Self {
        Self(q, s)
    }
}

impl<Q, S> TransitionTrigger for (Q, S) {
    type S = S;
    type Q = Q;
    fn from(&self) -> &Self::Q {
        &self.0
    }
    fn on(&self) -> &Self::S {
        &self.1
    }
}

pub trait Transition {
    type S;
    type Q;
    fn from(&self) -> &Self::Q;
    fn to(&self) -> &Self::Q;
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DeterministicTransition<S, Q>(pub Q, pub S, pub Q);

impl<S, Q> From<(Q, S, Q)> for DeterministicTransition<S, Q> {
    fn from((from, on, to): (Q, S, Q)) -> Self {
        Self(from, on, to)
    }
}

impl<Q, S> From<DeterministicTransition<S, Q>> for (Q, S, Q) {
    fn from(transition: DeterministicTransition<S, Q>) -> Self {
        (transition.0, transition.1, transition.2)
    }
}

impl<Q, S> Transition for DeterministicTransition<S, Q>
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

impl<S: PartialOrd, Q: PartialOrd> PartialOrd for DeterministicTransition<S, Q> {
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

impl<S: Ord, Q: Ord> Ord for DeterministicTransition<S, Q> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .cmp(&other.0)
            .then(self.1.cmp(&other.1))
            .then(self.2.cmp(&other.2))
    }
}

impl<T: Transition> From<T> for Trigger<T::S, T::Q>
where
    T::Q: Clone,
    T::S: Clone,
{
    fn from(transition: T) -> Self {
        Trigger(transition.from().clone(), transition.symbol().clone())
    }
}
