use crate::{
    prelude::Symbol,
    ts::SymbolOf,
    word::{FiniteWord, OmegaWord},
};

use super::{DFALike, IntoDFA, IntoMealyMachine, IntoMooreMachine, MealyLike, MooreLike};

pub trait FiniteWordTransformer<S, C> {
    fn transform_finite<W: FiniteWord<S>>(&self, word: W) -> C;
}

impl<D: MooreLike> FiniteWordTransformer<SymbolOf<D>, D::StateColor> for IntoMooreMachine<D> {
    fn transform_finite<W: FiniteWord<SymbolOf<D>>>(&self, word: W) -> D::StateColor {
        self.try_moore_map(word).expect("Transformer must be total")
    }
}

impl<D: MealyLike> FiniteWordTransformer<SymbolOf<D>, D::EdgeColor> for IntoMealyMachine<D> {
    fn transform_finite<W: FiniteWord<SymbolOf<D>>>(&self, word: W) -> D::EdgeColor {
        self.try_mealy_map(word).expect("Transformer must be total")
    }
}

impl<D: DFALike> FiniteWordTransformer<SymbolOf<D>, bool> for IntoDFA<D> {
    fn transform_finite<W: FiniteWord<SymbolOf<D>>>(&self, word: W) -> bool {
        self.try_moore_map(word).expect("Transformer must be total")
    }
}

pub trait FiniteWordAcceptor<S> {
    fn accepts_finite<W: FiniteWord<S>>(&self, word: W) -> bool;
}

pub trait OmegaWordTransformer<S, C> {
    fn transform_omega<W: OmegaWord<S>>(&self, word: W) -> C;
}

pub trait OmegaWordAcceptor<S> {
    fn accepts_omega<W: OmegaWord<S>>(&self, word: W) -> bool;
}
