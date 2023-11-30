use crate::word::{FiniteWord, OmegaWord};

pub trait FiniteWordTransformer<S, C> {
    fn transform_finite<W: FiniteWord<S>>(&self, word: W) -> C;
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
