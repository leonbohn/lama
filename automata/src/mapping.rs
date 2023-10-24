use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use impl_tools::autoimpl;

use crate::prelude::*;

#[autoimpl(for<T: trait + ?Sized> &T, &mut T, Box<T>)]
pub trait Morphism<S, Q> {
    fn morph<W: Word<Symbol = S, Length = FiniteLength>>(&self, word: W) -> Q;
}

impl<A: Alphabet> Morphism<A::Symbol, bool> for DFA<A> {
    fn morph<W: Word<Symbol = A::Symbol, Length = FiniteLength>>(&self, word: W) -> bool {
        self.accepts(word)
    }
}

impl<A: Alphabet, Q: Color, C: Color> Morphism<A::Symbol, Q> for MooreMachine<A, Q, C> {
    fn morph<W: Word<Symbol = A::Symbol, Length = FiniteLength>>(&self, word: W) -> Q {
        let Some(reached) = self.reached_color(&word.raw_to_vec()) else {
            panic!("Only deterministic and complete Moore Machines are morphisms");
        };
        reached.0
    }
}

impl<A: Alphabet, C: Color> Morphism<A::Symbol, C> for MealyMachine<A, C> {
    fn morph<W: Word<Symbol = A::Symbol, Length = FiniteLength>>(&self, word: W) -> C {
        let Ok(run) = self.run(&word, self.initial()) else {
            panic!("Only deterministic and complete Mealy Machines are morphisms");
        };
        assert!(
            run.loop_index().is_none(),
            "This can only have been called on a finite word!"
        );
        run.path()
            .last_transition_color(&self)
            .expect("This color must exist as the MM should be complete and deterministic")
    }
}

/// A mapping associates elements of one type with elements of another type. Note, that we assume
/// mappings to be total, i.e. every input element is mapped to an output element.
pub trait Mapping<Input> {
    /// The type of the output elements.
    type Output;

    /// Applies the mapping to the given input and returns the output. Assumes the mapping to be total
    /// and may panic if this is not the case.
    fn apply<I: Borrow<Input>>(&self, input: I) -> Self::Output;
}

impl<In: PartialEq, Out: Clone> Mapping<In> for Vec<(In, Out)> {
    type Output = Out;

    fn apply<I: Borrow<In>>(&self, input: I) -> Self::Output {
        self.iter()
            .find(|(i, _)| i == input.borrow())
            .map(|(_, o)| o)
            .unwrap()
            .clone()
    }
}

impl<In: Eq + Hash, Out: Clone> Mapping<In> for HashMap<In, Out> {
    type Output = Out;

    fn apply<I: Borrow<In>>(&self, input: I) -> Self::Output {
        self.get(input.borrow()).unwrap().clone()
    }
}

impl<In: Eq + Hash> Mapping<In> for HashSet<In> {
    type Output = bool;

    fn apply<I: Borrow<In>>(&self, input: I) -> Self::Output {
        self.contains(input.borrow())
    }
}
