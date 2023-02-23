use crate::ts::Transition;

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
    type Sym = S;

    type Id = Q;

    fn from(&self) -> &Self::Id {
        &self.0
    }

    fn to(&self) -> &Self::Id {
        &self.2
    }

    fn symbol(&self) -> &Self::Sym {
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
