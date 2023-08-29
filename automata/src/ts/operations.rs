use std::{fmt::Display, marker::PhantomData};

use itertools::Itertools;

use crate::{
    alphabet::{Alphabet, Expression, ExpressionOf, HasAlphabet, Symbol, SymbolOf},
    Acceptor, Color, FiniteLength, Length, Transformer,
};

use super::{
    ColorPosition, Edge, EdgeColor, FiniteState, HasStates, OnEdges, OnStates, Pointed, State,
    StateColor, StateIndex, Successor, Transition,
};

pub trait Product: Successor + Sized {
    type Output<Ts: Successor<Alphabet = Self::Alphabet, Position = Self::Position>>: Successor<
        Alphabet = Self::Alphabet,
        Position = Self::Position,
        Color = (Self::Color, Ts::Color),
    >;
    fn ts_product<Ts: Successor<Alphabet = Self::Alphabet, Position = Self::Position>>(
        self,
        other: Ts,
    ) -> Self::Output<Ts>;
}

impl<Lts: Successor + Sized> Product for Lts {
    type Output<Ts: Successor<Alphabet = Self::Alphabet, Position = Self::Position>> =
        MatchingProduct<Self, Ts>;

    fn ts_product<Ts: Successor<Alphabet = Self::Alphabet, Position = Self::Position>>(
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
    R: Pointed<Position = L::Position>,
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

impl<L, R> FiniteState for MatchingProduct<L, R>
where
    L: FiniteState,
    R: FiniteState<Position = L::Position>,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,

    L::Color: Clone,
    R::Color: Clone,
{
    fn state_indices(&self) -> Vec<Self::StateIndex> {
        self.0
            .state_indices()
            .into_iter()
            .cartesian_product(self.1.state_indices())
            .map(|(l, r)| ProductIndex(l, r))
            .collect()
    }
}

impl<L, R> Successor for MatchingProduct<L, R>
where
    L: Successor,
    R: Successor<Position = L::Position>,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
    L::Color: Clone,
    R::Color: Clone,
{
    type StateIndex = ProductIndex<L::StateIndex, R::StateIndex>;
    type Position = L::Position;
    type Color = (L::Color, R::Color);

    fn successor(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Transition<Self::StateIndex, SymbolOf<Self>, EdgeColor<Self>>> {
        let ProductIndex(l, r) = state;
        let symbol = symbol;
        let ll = self.0.successor(l, symbol)?;
        let rr = self.1.successor(r, symbol)?;
        Some(Transition::new(
            state,
            symbol,
            ProductIndex(ll.target(), rr.target()),
            <L::Position as ColorPosition>::combine_edges(ll.color().clone(), rr.color().clone()),
        ))
    }

    fn state_color(&self, state: Self::StateIndex) -> StateColor<Self> {
        let ProductIndex(l, r) = state;
        let left = self.0.state_color(l);
        let right = self.1.state_color(r);
        <L::Position as ColorPosition>::combine_states(left, right)
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
                    result.push((
                        ProductIndex(ll, rr),
                        ex1.clone(),
                        <L::Position as ColorPosition>::combine_edges(c1.clone(), c2),
                    ));
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
                        <L::Position as ColorPosition>::combine_edges(
                            ledge.color().clone(),
                            redge.color().clone(),
                        ),
                        ledge.trigger().clone(),
                    ));
                }
            }
        }

        result
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapColors<Ts, F> {
    ts: Ts,
    f: F,
}

impl<D: Color, Ts: FiniteState, F: Fn(Ts::Color) -> D> FiniteState for MapColors<Ts, F> {
    fn state_indices(&self) -> Vec<Self::StateIndex> {
        self.ts.state_indices()
    }
}

impl<Ts, F> MapColors<Ts, F> {
    pub fn new(ts: Ts, f: F) -> Self {
        Self { ts, f }
    }
}

impl<D, Ts, F> Successor for MapColors<Ts, F>
where
    D: Color,
    Ts: Successor,
    F: Fn(Ts::Color) -> D,
{
    type StateIndex = Ts::StateIndex;

    type Position = Ts::Position;

    type Color = D;

    fn successor(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Transition<Self::StateIndex, SymbolOf<Self>, EdgeColor<Self>>> {
        self.ts.successor(state, symbol).map(|t| {
            let edge_color =
                <Ts::Position as ColorPosition>::map_edge_color(t.color().clone(), &self.f);
            t.with_color(edge_color)
        })
    }

    fn state_color(&self, state: Self::StateIndex) -> StateColor<Self> {
        let color = self.ts.state_color(state);
        <Ts::Position as ColorPosition>::map_state_color(color, &self.f)
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
            .map(|(predecessor, expression, color)| {
                (
                    predecessor,
                    expression,
                    <Ts::Position as ColorPosition>::map_edge_color(color, &self.f),
                )
            })
            .collect()
    }

    fn edges_from(
        &self,
        state: Self::StateIndex,
    ) -> Vec<super::Edge<ExpressionOf<Self>, EdgeColor<Self>, Self::StateIndex>> {
        self.ts
            .edges_from(state)
            .into_iter()
            .map(|edge| {
                let color = edge.color().clone();
                edge.recolor(<Ts::Position as ColorPosition>::map_edge_color(
                    color, &self.f,
                ))
            })
            .collect()
    }
}

impl<D: Color, Ts: Successor + Pointed, F: Fn(Ts::Color) -> D> Pointed for MapColors<Ts, F> {
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

impl<Ts: Successor, F> HasAlphabet for MapColors<Ts, F> {
    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        alphabet::Simple,
        ts::{finite::ReachedState, HasColorMut, HasMutableStates, Pointed, Sproutable, Successor},
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
