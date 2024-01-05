use impl_tools::autoimpl;

use crate::{automaton::WithInitial, Alphabet, Color, RightCongruence, TransitionSystem};

use super::{
    nts::{NTEdge, NTSEdgesTo},
    operations::{
        MapEdgeColor, MapStateColor, MappedPreTransition, MappedTransitionsToIter, MatchingProduct,
        ProductEdgesTo, ProductPreTransition, RestrictByStateIndex, RestrictedEdgesToIter,
        Reversed, StateIndexFilter,
    },
    transition_system::{EdgeReference, IsEdge},
    EdgeColor, ExpressionOf, IndexType, SymbolOf, BTS,
};

/// Implementors of this trait are [`TransitionSystem`]s which allow iterating over the predecessors of a state.
pub trait PredecessorIterable: TransitionSystem {
    /// The type of pre-transition that the iterator yields.
    type PreTransitionRef<'this>: IsEdge<
        'this,
        ExpressionOf<Self>,
        Self::StateIndex,
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

    fn reversed(self) -> Reversed<Self> {
        Reversed(self)
    }
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
    type PreTransitionRef<'this> = ProductPreTransition<'this, L::StateIndex, R::StateIndex, ExpressionOf<L>, L::EdgeColor, R::EdgeColor> where Self: 'this;
    type EdgesToIter<'this> = ProductEdgesTo<'this, L, R> where Self: 'this;
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        ProductEdgesTo::new(&self.0, &self.1, state)
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> PredecessorIterable for BTS<A, Q, C, Idx> {
    type PreTransitionRef<'this> = EdgeReference<'this, A::Expression, Idx, C> where Self: 'this;
    type EdgesToIter<'this> = BTSPredecessors<'this, A, C, Idx>
    where
        Self: 'this;
    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        Some(BTSPredecessors::new(
            self.raw_state_map().get(&state)?.predecessors().iter(),
            state,
        ))
    }
}

pub struct BTSPredecessors<'a, A: Alphabet, C: Color, Idx: IndexType> {
    it: std::collections::hash_set::Iter<'a, (Idx, A::Expression, C)>,
    state: Idx,
}

impl<'a, A: Alphabet, C: Color, Idx: IndexType> Iterator for BTSPredecessors<'a, A, C, Idx> {
    type Item = EdgeReference<'a, A::Expression, Idx, C>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it
            .next()
            .map(|(s, e, c)| EdgeReference::new(*s, e, c, self.state))
    }
}

impl<'a, A: Alphabet, C: Color, Idx: IndexType> BTSPredecessors<'a, A, C, Idx> {
    pub fn new(
        it: std::collections::hash_set::Iter<'a, (Idx, A::Expression, C)>,
        state: Idx,
    ) -> Self {
        Self { it, state }
    }
}

impl<A: Alphabet> PredecessorIterable for RightCongruence<A> {
    type PreTransitionRef<'this> = &'this NTEdge<A::Expression, ()>
    where
        Self: 'this;

    type EdgesToIter<'this> = NTSEdgesTo<'this, A::Expression, ()>
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
