use std::{fmt::Display, marker::PhantomData};

use itertools::Itertools;

use crate::{
    alphabet::{Alphabet, Expression, ExpressionOf, HasAlphabet, Symbol, SymbolOf},
    Acceptor, Color, FiniteLength, Length, Transformer,
};

use super::{
    successor::IsTransition, BTState, Edge, EdgeColor, FiniteState, FiniteStatesIterType,
    HasFiniteStates, HasStates, IndexType, Pointed, StateColor, StateIndex, Transition,
    TransitionSystem,
};

pub trait Product: TransitionSystem + Sized {
    type Output<Ts: TransitionSystem<Alphabet = Self::Alphabet>>: TransitionSystem<
        Alphabet = Self::Alphabet,
        StateColor = (Self::StateColor, Ts::StateColor),
    >;
    fn ts_product<Ts: TransitionSystem<Alphabet = Self::Alphabet>>(
        self,
        other: Ts,
    ) -> Self::Output<Ts>;
}

impl<Lts: TransitionSystem + Sized> Product for Lts {
    type Output<Ts: TransitionSystem<Alphabet = Self::Alphabet>> = MatchingProduct<Self, Ts>;

    fn ts_product<Ts: TransitionSystem<Alphabet = Self::Alphabet>>(
        self,
        other: Ts,
    ) -> Self::Output<Ts> {
        MatchingProduct(self, other)
    }
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProductIndex<L, R>(pub L, pub R);

impl<L, R> From<ProductIndex<L, R>> for (L, R) {
    fn from(value: ProductIndex<L, R>) -> Self {
        (value.0, value.1)
    }
}

impl<L: Display, R: Display> Display for ProductIndex<L, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.0, self.1)
    }
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MatchingProduct<L, R>(pub L, pub R);

impl<L, R> Pointed for MatchingProduct<L, R>
where
    L: Pointed,
    R: Pointed,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
{
    fn initial(&self) -> Self::StateIndex {
        ProductIndex(self.0.initial(), self.1.initial())
    }
}

impl<L: HasAlphabet, R> HasAlphabet for MatchingProduct<L, R> {
    type Alphabet = L::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.0.alphabet()
    }
}

impl<'a, L, R> HasFiniteStates<'a> for MatchingProduct<L, R>
where
    L: HasFiniteStates<'a>,
    R: HasFiniteStates<'a>,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
    L::StateColor: Clone,
    R::StateColor: Clone,
    R::StateIndicesIter: Clone,
{
    type StateIndicesIter = std::iter::Map<
        itertools::Product<L::StateIndicesIter, R::StateIndicesIter>,
        fn((L::StateIndex, R::StateIndex)) -> ProductIndex<L::StateIndex, R::StateIndex>,
    >;
}

impl<L, R> FiniteState for MatchingProduct<L, R>
where
    L: FiniteState,
    R: FiniteState,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
    L::StateColor: Clone,
    R::StateColor: Clone,
{
    fn state_indices(&self) -> FiniteStatesIterType<'_, Self> {
        self.0
            .state_indices()
            .cartesian_product(self.1.state_indices())
            .map(|(l, r)| ProductIndex(l, r))
    }
}

pub struct ProductTransition<L, R>(L, R);

impl<Idx, Jdx, E, C, D, L, R> IsTransition<E, ProductIndex<Idx, Jdx>, (C, D)>
    for ProductTransition<L, R>
where
    L: IsTransition<E, Idx, C>,
    R: IsTransition<E, Jdx, D>,
{
    fn source(&self) -> ProductIndex<Idx, Jdx> {
        ProductIndex(self.0.source(), self.1.source())
    }

    fn target(&self) -> ProductIndex<Idx, Jdx> {
        ProductIndex(self.0.target(), self.1.target())
    }

    fn color(&self) -> (C, D) {
        (self.0.color(), self.1.color())
    }

    fn expression(&self) -> &E {
        self.0.expression()
    }
}

impl<L, R> TransitionSystem for MatchingProduct<L, R>
where
    L: TransitionSystem,
    R: TransitionSystem,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
    L::StateColor: Clone,
    R::StateColor: Clone,
{
    type StateIndex = ProductIndex<L::StateIndex, R::StateIndex>;
    type EdgeColor = (L::EdgeColor, R::EdgeColor);
    type StateColor = (L::StateColor, R::StateColor);
    type TransitionRef<'this> = ProductTransition<L::TransitionRef<'this>, R::TransitionRef<'this>> where Self: 'this;

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        let ProductIndex(l, r) = state;

        let ll = self.0.transition(l, symbol)?;
        let rr = self.1.transition(r, symbol)?;
        Some(ProductTransition(ll, rr))
    }

    fn state_color(&self, state: Self::StateIndex) -> StateColor<Self> {
        let ProductIndex(l, r) = state;
        let left = self.0.state_color(l);
        let right = self.1.state_color(r);
        (left, right)
    }

    fn predecessors(
        &self,
        state: Self::StateIndex,
    ) -> Vec<(
        Self::StateIndex,
        crate::alphabet::ExpressionOf<Self>,
        EdgeColor<Self>,
    )> {
        let ProductIndex(l, r) = state;
        let mut result: Vec<(
            Self::StateIndex,
            crate::alphabet::ExpressionOf<Self>,
            EdgeColor<Self>,
        )> = Vec::new();
        for (ll, ex1, c1) in self.0.predecessors(l) {
            for (rr, ex2, c2) in self.1.predecessors(r) {
                if ex1 == ex2 {
                    result.push((ProductIndex(ll, rr), ex1.clone(), (c1.clone(), c2.clone())));
                }
            }
        }
        result
    }

    fn edges_from(
        &self,
        state: Self::StateIndex,
    ) -> Vec<super::Edge<ExpressionOf<Self>, EdgeColor<Self>, Self::StateIndex>> {
        let ProductIndex(l, r) = state;
        let mut result = Vec::new();

        for ledge in self.0.edges_from(l) {
            for redge in self.1.edges_from(r) {
                if ledge.trigger() == redge.trigger() {
                    result.push(Edge::new(
                        ProductIndex(l, r),
                        ProductIndex(ledge.target(), redge.target()),
                        (ledge.color().clone(), redge.color().clone()),
                        ledge.trigger().clone(),
                    ));
                }
            }
        }

        result
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        let ProductIndex(l, r) = state;
        let left = self.0.edge_color(l, expression)?;
        let right = self.1.edge_color(r, expression)?;
        Some((left, right))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapEdgeColor<Ts, F> {
    ts: Ts,
    f: F,
}

impl<Ts, F> MapEdgeColor<Ts, F> {
    pub fn new(ts: Ts, f: F) -> Self {
        Self { ts, f }
    }
}

impl<D: Color, Ts: TransitionSystem + Pointed, F: Fn(Ts::EdgeColor) -> D> Pointed
    for MapEdgeColor<Ts, F>
{
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

impl<Ts: TransitionSystem, F> HasAlphabet for MapEdgeColor<Ts, F> {
    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}

pub struct MappedTransition<T, F, C> {
    transition: T,
    f: F,
    _old_color: PhantomData<C>,
}

impl<T, F, C> MappedTransition<T, F, C> {
    pub fn new(transition: T, f: F) -> Self {
        Self {
            transition,
            f,
            _old_color: PhantomData,
        }
    }
}

impl<Idx: IndexType, E, C: Color, D: Color, F: Fn(C) -> D, T: IsTransition<E, Idx, C>>
    IsTransition<E, Idx, D> for MappedTransition<T, F, C>
{
    fn source(&self) -> Idx {
        self.transition.source()
    }

    fn target(&self) -> Idx {
        self.transition.target()
    }

    fn color(&self) -> D {
        (self.f)(self.transition.color())
    }

    fn expression(&self) -> &E {
        self.transition.expression()
    }
}

impl<D, Ts, F> TransitionSystem for MapEdgeColor<Ts, F>
where
    D: Color,
    Ts: TransitionSystem,
    F: Fn(Ts::EdgeColor) -> D,
{
    type StateIndex = Ts::StateIndex;
    type EdgeColor = D;
    type StateColor = Ts::StateColor;
    type TransitionRef<'this> = MappedTransition<Ts::TransitionRef<'this>, &'this F, Ts::EdgeColor> where Self: 'this;

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        Some(MappedTransition::new(
            self.ts.transition(state, symbol)?,
            &self.f,
        ))
    }

    fn state_color(&self, state: Self::StateIndex) -> StateColor<Self> {
        self.ts.state_color(state)
    }

    fn predecessors(
        &self,
        state: Self::StateIndex,
    ) -> Vec<(
        Self::StateIndex,
        crate::alphabet::ExpressionOf<Self>,
        EdgeColor<Self>,
    )> {
        self.ts
            .predecessors(state)
            .into_iter()
            .map(|(s, e, c)| (s, e, (self.f)(c)))
            .collect()
    }

    fn edges_from(
        &self,
        state: Self::StateIndex,
    ) -> Vec<super::Edge<ExpressionOf<Self>, EdgeColor<Self>, Self::StateIndex>> {
        self.ts
            .edges_from(state)
            .into_iter()
            .map(|e| {
                Edge::new(
                    e.source(),
                    e.target(),
                    (self.f)(e.color().clone()),
                    e.trigger().clone(),
                )
            })
            .collect()
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.ts.edge_color(state, expression).map(|c| (self.f)(c))
    }
}

impl<'a, D: Color, Ts: FiniteState, F: Fn(Ts::EdgeColor) -> D> HasFiniteStates<'a>
    for MapEdgeColor<Ts, F>
{
    type StateIndicesIter = FiniteStatesIterType<'a, Ts>;
}

impl<D: Color, Ts: FiniteState, F: Fn(Ts::EdgeColor) -> D> FiniteState for MapEdgeColor<Ts, F> {
    fn state_indices(&self) -> FiniteStatesIterType<'_, Self> {
        self.ts.state_indices()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapStateColor<Ts, F> {
    ts: Ts,
    f: F,
}

impl<Ts, F> MapStateColor<Ts, F> {
    pub fn new(ts: Ts, f: F) -> Self {
        Self { ts, f }
    }
}

impl<'a, D: Color, Ts: FiniteState, F: Fn(Ts::StateColor) -> D> HasFiniteStates<'a>
    for MapStateColor<Ts, F>
{
    type StateIndicesIter = FiniteStatesIterType<'a, Ts>;
}

impl<D: Color, Ts: FiniteState, F: Fn(Ts::StateColor) -> D> FiniteState for MapStateColor<Ts, F> {
    fn state_indices(&self) -> FiniteStatesIterType<'_, Self> {
        self.ts.state_indices()
    }
}

impl<D, Ts, F> TransitionSystem for MapStateColor<Ts, F>
where
    D: Color,
    Ts: TransitionSystem,
    F: Fn(Ts::StateColor) -> D,
{
    type StateIndex = Ts::StateIndex;
    type EdgeColor = Ts::EdgeColor;
    type StateColor = D;
    type TransitionRef<'this> = Ts::TransitionRef<'this> where Self: 'this;

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        self.ts.transition(state, symbol)
    }

    fn state_color(&self, state: Self::StateIndex) -> StateColor<Self> {
        let color = self.ts.state_color(state);
        (self.f)(color)
    }

    fn predecessors(
        &self,
        state: Self::StateIndex,
    ) -> Vec<(
        Self::StateIndex,
        crate::alphabet::ExpressionOf<Self>,
        EdgeColor<Self>,
    )> {
        self.ts.predecessors(state)
    }

    fn edges_from(
        &self,
        state: Self::StateIndex,
    ) -> Vec<super::Edge<ExpressionOf<Self>, EdgeColor<Self>, Self::StateIndex>> {
        self.ts.edges_from(state)
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.ts.edge_color(state, expression)
    }
}

impl<D: Color, Ts: TransitionSystem + Pointed, F: Fn(Ts::StateColor) -> D> Pointed
    for MapStateColor<Ts, F>
{
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

impl<Ts: TransitionSystem, F> HasAlphabet for MapStateColor<Ts, F> {
    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        alphabet::Simple,
        ts::{
            finite::ReachedState, HasColorMut, HasMutableStates, Pointed, Sproutable,
            TransitionSystem,
        },
        Transformer,
    };

    use super::{Product, ProductIndex};

    #[test]
    fn product() {
        let mut dfa = crate::DFA::new(Simple::new(['a', 'b']));
        let s0 = dfa.initial();
        dfa.state_mut(s0).unwrap().set_color(true);
        let s1 = dfa.add_state(false);
        let _e0 = dfa.add_edge(s0, 'a', s1, ());
        let _e1 = dfa.add_edge(s0, 'b', s0, ());
        let _e2 = dfa.add_edge(s1, 'a', s1, ());
        let _e3 = dfa.add_edge(s1, 'b', s0, ());

        let mut dfb = crate::DFA::new(Simple::new(['a', 'b']));
        let s0 = dfb.initial();
        dfb.state_mut(s0).unwrap().set_color(true);
        let s1 = dfb.add_state(false);
        let _e0 = dfb.add_edge(s0, 'a', s1, ());
        let _e1 = dfb.add_edge(s0, 'b', s0, ());
        let _e2 = dfb.add_edge(s1, 'a', s1, ());
        let _e3 = dfb.add_edge(s1, 'b', s0, ());

        let xxx = dfa.ts_product(dfb);
        if let Some(ReachedState(q)) = xxx.induced(&"abb", ProductIndex(0, 0)) {}
        let c = xxx.transform("aa");

        let yyy = xxx.map_colors(|(a, b)| a || b);
        let d = yyy.transform("aa");

        assert_eq!(c.0 || c.1, d);
    }
}
