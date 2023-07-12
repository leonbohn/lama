use crate::Color;

use super::{State, StateIndex, Transition};

pub trait CanInduce<I> {
    fn induce(&self) -> I;
}

#[allow(missing_docs)]
pub mod finite {
    use crate::ts::StateIndex;

    #[derive(Debug, Clone, PartialEq)]
    pub struct ReachedColor<Q>(pub Q);

    #[derive(Debug, Clone, PartialEq)]
    pub struct ReachedState(pub StateIndex);

    #[derive(Debug, Clone, PartialEq)]

    pub struct StateColorSequence<C>(pub Vec<C>);

    #[derive(Debug, Clone, PartialEq)]

    pub struct TransitionColorSequence<C>(pub Vec<C>);

    impl<Q> std::ops::Deref for ReachedColor<Q> {
        type Target = Q;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl std::ops::Deref for ReachedState {
        type Target = StateIndex;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
}

#[allow(missing_docs)]
pub mod infinite {
    use std::collections::BTreeSet;

    #[derive(Debug, Clone, PartialEq)]
    pub struct InfinitySet<C>(pub BTreeSet<C>);

    impl<C> std::ops::Deref for InfinitySet<C> {
        type Target = BTreeSet<C>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
}
