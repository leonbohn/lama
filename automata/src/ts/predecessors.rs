use impl_tools::autoimpl;

use crate::{
    alphabet::{ExpressionOf, SymbolOf},
    automata::WithInitial,
    Alphabet, Color, RightCongruence, TransitionSystem,
};

use super::{
    operations::{
        MapEdgeColor, MapStateColor, MappedPreTransition, MappedTransitionsToIter, MatchingProduct,
        ProductEdgesTo, ProductPreTransition, RestrictByStateIndex, RestrictedEdgesToIter,
        StateIndexFilter,
    },
    EdgeColor, IndexType, BTS,
};

/// The counterpart to the [`super::transition_system::IsTransition`] trait for predecessors.
#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait IsPreTransition<Idx, E, C> {
    /// Return the source state of the pre-transition.
    fn source(&self) -> Idx;
    /// Return the color of the pre-transition.
    fn color(&self) -> C;
    /// Return the expression of the pre-transition.
    fn expression(&self) -> &E;
    /// Decompose `self` into a tuple of its components.
    fn into_tuple(self) -> (Idx, E, C)
    where
        E: Clone,
        Self: Sized,
    {
        (self.source(), self.expression().clone(), self.color())
    }
}

impl<'a, Idx: IndexType, E, C: Color> IsPreTransition<Idx, E, C> for (Idx, &'a E, C) {
    fn source(&self) -> Idx {
        self.0
    }

    fn color(&self) -> C {
        self.2.clone()
    }

    fn expression(&self) -> &E {
        self.1
    }
}
impl<Idx: IndexType, E, C: Color> IsPreTransition<Idx, E, C> for (Idx, E, C) {
    fn source(&self) -> Idx {
        self.0
    }

    fn color(&self) -> C {
        self.2.clone()
    }

    fn expression(&self) -> &E {
        &self.1
    }
}

/// Implementors of this trait are [`TransitionSystem`]s which allow iterating over the predecessors of a state.
pub trait PredecessorIterable: TransitionSystem {
    /// The type of pre-transition that the iterator yields.
    type PreTransitionRef<'this>: IsPreTransition<
        Self::StateIndex,
        ExpressionOf<Self>,
        EdgeColor<Self>,
    >
    where
        Self: 'this;

    /// The type of iterator over the predecessors of a state.
    type EdgesToIter<'this>: Iterator<Item = Self::PreTransitionRef<'this>>
    where
        Self: 'this;

    /// Returns an iterator over the predecessors of the given `state`. Returns `None` if the state does not exist.
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>>;
}

impl<Ts, F> PredecessorIterable for RestrictByStateIndex<Ts, F>
where
    Ts: PredecessorIterable,
    F: StateIndexFilter<Ts::StateIndex>,
{
    type PreTransitionRef<'this> = Ts::PreTransitionRef<'this> where Self: 'this;
    type EdgesToIter<'this> = RestrictedEdgesToIter<'this, Ts, F> where Self: 'this;
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        Some(RestrictedEdgesToIter::new(
            self.ts().predecessors(state)?,
            self.filter(),
        ))
    }
}

impl<Ts: PredecessorIterable> PredecessorIterable for &Ts {
    type PreTransitionRef<'this> = Ts::PreTransitionRef<'this> where Self: 'this;
    type EdgesToIter<'this> = Ts::EdgesToIter<'this> where Self: 'this;
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        Ts::predecessors(self, state)
    }
}
impl<Ts: PredecessorIterable> PredecessorIterable for &mut Ts {
    type PreTransitionRef<'this> = Ts::PreTransitionRef<'this> where Self: 'this;
    type EdgesToIter<'this> = Ts::EdgesToIter<'this> where Self: 'this;
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        Ts::predecessors(self, state)
    }
}

impl<D, Ts, F> PredecessorIterable for MapEdgeColor<Ts, F>
where
    D: Color,
    Ts: PredecessorIterable,
    F: Fn(Ts::EdgeColor) -> D,
{
    type PreTransitionRef<'this> = MappedPreTransition<Ts::PreTransitionRef<'this>, &'this F, Ts::EdgeColor> where Self: 'this;
    type EdgesToIter<'this> = MappedTransitionsToIter<'this, Ts::EdgesToIter<'this>, F, Ts::EdgeColor> where Self: 'this;
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        Some(MappedTransitionsToIter::new(
            self.ts().predecessors(state)?,
            self.f(),
        ))
    }
}

impl<D, Ts, F> PredecessorIterable for MapStateColor<Ts, F>
where
    D: Color,
    Ts: PredecessorIterable,
    F: Fn(Ts::StateColor) -> D,
{
    type EdgesToIter<'this> = Ts::EdgesToIter<'this> where Self: 'this;
    type PreTransitionRef<'this> = Ts::PreTransitionRef<'this> where Self: 'this;
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        self.ts().predecessors(state)
    }
}

impl<L, R> PredecessorIterable for MatchingProduct<L, R>
where
    L: PredecessorIterable,
    R: PredecessorIterable,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
    L::StateColor: Clone,
    R::StateColor: Clone,
{
    type PreTransitionRef<'this> = ProductPreTransition<L::StateIndex, R::StateIndex, ExpressionOf<L>, L::EdgeColor, R::EdgeColor> where Self: 'this;
    type EdgesToIter<'this> = ProductEdgesTo<'this, L, R> where Self: 'this;
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        ProductEdgesTo::new(&self.0, &self.1, state)
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> PredecessorIterable for BTS<A, Q, C, Idx> {
    type PreTransitionRef<'this> = &'this (Idx, A::Expression, C) where Self: 'this;
    type EdgesToIter<'this> = std::collections::hash_set::Iter<'this, (Idx, A::Expression, C)>
    where
        Self: 'this;
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        Some(self.states().get(&state)?.predecessors().iter())
    }
}

impl<A: Alphabet> PredecessorIterable for RightCongruence<A> {
    type PreTransitionRef<'this> = &'this (usize, A::Expression, ()) where Self: 'this;
    type EdgesToIter<'this> = std::collections::hash_set::Iter<'this, (usize, A::Expression, ())>
    where
        Self: 'this;
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        self.ts().predecessors(state)
    }
}

impl<Ts: PredecessorIterable> PredecessorIterable for WithInitial<Ts> {
    type PreTransitionRef<'this> = Ts::PreTransitionRef<'this> where Self: 'this;
    type EdgesToIter<'this> = Ts::EdgesToIter<'this> where Self: 'this;
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        self.ts().predecessors(state)
    }
}
