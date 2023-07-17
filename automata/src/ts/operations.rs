use std::{fmt::Display, marker::PhantomData};

use crate::{
    alphabet::{Alphabet, Expression, HasAlphabet, Symbol, SymbolOf},
    Acceptor, Color, FiniteLength, Length, Transformer,
};

use super::{
    ColorPosition, EdgeColor, HasStateIndices, HasStates, OnEdges, OnStates, Pointed, State,
    StateColor, Successor, Transition,
};

pub trait Product: Successor + Sized {
    type Output<Ts: Successor<Alphabet = Self::Alphabet, Position = Self::Position>>: Successor<
        Alphabet = Self::Alphabet,
        Position = Self::Position,
        Color = (Self::Color, Ts::Color),
    >;
    fn product<Ts: Successor<Alphabet = Self::Alphabet, Position = Self::Position>>(
        self,
        other: Ts,
    ) -> Self::Output<Ts>;
}

impl<Lts: Successor + Sized> Product for Lts {
    type Output<Ts: Successor<Alphabet = Self::Alphabet, Position = Self::Position>> =
        MatchingProduct<Self, Ts>;

    fn product<Ts: Successor<Alphabet = Self::Alphabet, Position = Self::Position>>(
        self,
        other: Ts,
    ) -> Self::Output<Ts> {
        MatchingProduct(self, other)
    }
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProductIndex<L, R>(pub L, pub R);

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
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>>,
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

impl<L, R> Successor for MatchingProduct<L, R>
where
    L: Successor,
    R: Successor<Position = L::Position>,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>>,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapColors<Ts, F> {
    ts: Ts,
    f: F,
}

impl<D: Color, Ts: HasStateIndices, F: Fn(Ts::Color) -> D> HasStateIndices for MapColors<Ts, F> {
    type StateIndexIter<'this> = Ts::StateIndexIter<'this>
    where
        Self: 'this;

    fn state_indices(&self) -> Self::StateIndexIter<'_> {
        self.ts.state_indices()
    }
}

impl<Ts, F> MapColors<Ts, F> {
    pub fn new(ts: Ts, f: F) -> Self {
        Self { ts, f }
    }
}

impl<D: Color, Ts: Successor, F: Fn(Ts::Color) -> D> Successor for MapColors<Ts, F> {
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

        let xxx = dfa.product(dfb);
        if let Some(ReachedState(q)) = xxx.induced(&"abb", ProductIndex(0, 0)) {}
        let c = xxx.transform("aa");

        let yyy = xxx.map_colors(|(a, b)| a || b);
        let d = yyy.transform("aa");

        assert_eq!(c.0 || c.1, d);
    }
}
