use impl_tools::autoimpl;

/// Abstracts induced objects. Induced objects are objects that are induced by a run of a transition
/// system. For example, a run of a transition system can induce a state index or a [`finite::StateColorSequence`].
/// The trait [`CanInduce`] allows to convert runs of a transition system into induced objects.
pub trait Induced: Eq {}
impl<T: Eq> Induced for T {}

/// Allows converting runs of a transition system into induced objects (of type `I`). The type `I`
/// is given as a generic parameter since the same run might induce different objects, for example
/// a [`finite::ReachedState`] or a [`finite::StateColorSequence`].
#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait CanInduce<I: Induced> {
    /// Induces an object of type `I` from the given run.
    fn induce(&self) -> I;
}

#[allow(missing_docs)]
pub mod finite {
    use std::collections::BTreeSet;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct ReachedColor<Q>(pub Q);

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct ReachedState<Idx>(pub Idx);

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct InfinityColors<C>(pub BTreeSet<C>);
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct InfinityStateColors<C>(pub BTreeSet<C>);

    #[derive(Debug, Clone, PartialEq, Eq)]

    pub struct SeenColors<C>(pub Vec<C>);

    #[derive(Debug, Clone, PartialEq, Eq)]

    pub struct StateColorSequence<C>(pub Vec<C>);

    #[derive(Debug, Clone, PartialEq, Eq)]

    pub struct TransitionColorSequence<C>(pub Vec<C>);

    impl<Q> std::ops::Deref for ReachedColor<Q> {
        type Target = Q;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<Idx> std::ops::Deref for ReachedState<Idx> {
        type Target = Idx;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
}

#[allow(missing_docs)]
pub mod infinite {
    use std::collections::BTreeSet;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct InfinityStateColors<C>(pub BTreeSet<C>);

    impl<C> std::ops::Deref for InfinityStateColors<C> {
        type Target = BTreeSet<C>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
}
