use crate::{
    prelude::Symbol,
    ts::SymbolOf,
    word::{FiniteWord, OmegaWord},
};

use super::{DFALike, IntoDFA, IntoMealyMachine, IntoMooreMachine, MealyLike, MooreLike};

/// Implementors of this trait transform finite inputs into some other type. For example,
/// a DFA maps each finite word over its input alphabet to a boolean value, indicating
/// whether the word is accepted or not. More generally, a Moore machine maps each finite
/// word over its input alphabet to a state color, and a Mealy machine maps each finite
/// word over its input alphabet to an edge color. Note that for Mealy machines, this produces
/// undefined behaviour for empty words.
///
/// This has a counterpart for infinite/omega words, see [`OmegaWordTransformer`].
pub trait FiniteWordTransformer<S, C> {
    /// Takes a finite `word` over `S` and returns a value of type `C`. Usually, this is used in
    /// the context of a DFA/Moore machine over an alphabet with symbols `S`. Then, `word` is
    /// mapped to a boolean value/colour of the state that the automaton ends up in after reading
    /// `word`.
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

/// A finite word acceptor can classify each finite word consisting of symbols of type `S` as
/// accepted or rejected. The canonical example for this would be DFAs, which accept a word
/// if and only if it ends up in an accepting state after reading the word.
pub trait FiniteWordAcceptor<S> {
    /// Returns true if and only if the finite `word` consisting of symbols of type `S` is
    /// accepted by `self`. Prototypical example would be DFAs, which accept a word if and
    /// only if they end up in an accepting state after reading the word.
    fn accepts_finite<W: FiniteWord<S>>(&self, word: W) -> bool;
}

/// An omega word transformer can transform each omega word consisting of symbols of type `S`
/// into some other type `C`. This is the counterpart to [`FiniteWordTransformer`] but for
/// infinite words.
pub trait OmegaWordTransformer<S, C> {
    /// Transforms a given infinite word consisting of symbols of type `S` into a value of type
    /// `C`.
    fn transform_omega<W: OmegaWord<S>>(&self, word: W) -> C;
}

/// Classifies each omega word consisting of symbols of type `S` as accepted or rejected. The
/// counterpart to [`FiniteWordAcceptor`] but for infinite words.
pub trait OmegaWordAcceptor<S> {
    /// Returns true if and only if the given infinite word consisting of symbols of type `S`
    /// is accepted by `self`.
    fn accepts_omega<W: OmegaWord<S>>(&self, word: W) -> bool;
}
