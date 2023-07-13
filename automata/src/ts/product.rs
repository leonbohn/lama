use std::{fmt::Display, marker::PhantomData};

use crate::alphabet::{Alphabet, Expression, HasAlphabet, SymbolOf};

use super::{ColorPosition, EdgeColor, OnEdges, OnStates, StateColor, Successor, Transition};

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Product<L, R>(pub L, pub R);

impl<L: HasAlphabet, R> HasAlphabet for Product<L, R> {
    type Alphabet = L::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.0.alphabet()
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
pub struct ProductColors<Pos: ColorPosition, C, D>(pub C, pub D, pub PhantomData<Pos>);

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProductPosition<C, D>(pub C, pub D);

impl<L, R> Successor for Product<L, R>
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
        let symbol = symbol.clone();
        let ll = self.0.successor(l, symbol.clone())?;
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
