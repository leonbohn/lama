use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use impl_tools::autoimpl;

use crate::prelude::*;

/// Abstracts the concept of a (total) morphism, which is in essence simply a map from finite words
/// over `S` (which in this case is the type of a symbol) to objects of type [`Self::Output`].
#[autoimpl(for<T: trait + ?Sized> &T, &mut T, Box<T>)]
pub trait Morphism<S> {
    /// The type that is output when a finite word consisting of symbols of type `S` is input.
    type Output: Color;
    /// Apply the morphism to a finite word consisting of objects of type `S`, returning the obtained
    /// object of type [`Self::Output`].
    fn morph<W: FiniteWord<S>>(&self, word: W) -> Self::Output;
}

// impl<A: Alphabet> Morphism<A::Symbol> for DFA<A> {
//     type Output = bool;
//     fn morph<W: FiniteWord<A::Symbol>>(&self, word: W) -> bool {
//         self.accepts(word)
//     }
// }

// impl<A: Alphabet, Q: Color, C: Color> Morphism<A::Symbol> for MooreMachine<A, Q, C> {
//     type Output = Q;
//     fn morph<W: FiniteWord<A::Symbol>>(&self, word: W) -> Q {
//         let Some(reached) = self.reached_color(&word.raw_to_vec()) else {
//             panic!("Only deterministic and complete Moore Machines are morphisms");
//         };
//         reached.0
//     }
// }

// impl<A: Alphabet, C: Color> Morphism<A::Symbol> for MealyMachine<A, C> {
//     type Output = C;
//     fn morph<W: FiniteWord<A::Symbol>>(&self, word: W) -> C {
//         let Ok(run) = self.run_from(&word, self.initial()) else {
//             panic!("Only deterministic and complete Mealy Machines are morphisms");
//         };
//         assert!(
//             run.loop_index().is_none(),
//             "This can only have been called on a finite word!"
//         );
//         run.path()
//             .last_transition_color(&self)
//             .expect("This color must exist as the MM should be complete and deterministic")
//     }
// }

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
