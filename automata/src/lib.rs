//! Library for working with finite automata in Rust.
//!
#![allow(missing_docs)]
#![allow(unused)]
#![allow(clippy::pedantic)]

/// The prelude is supposed to make using this package easier. Including everything, i.e.
/// `use automata::prelude::*;` should be enough to use the package.
pub mod prelude {
    pub use super::{
        alphabet,
        alphabet::{AlphabetOf, Expression, ExpressionOf, HasAlphabet, Simple, Symbol, SymbolOf},
        automata::{
            DBALike, DFALike, DPALike, FiniteWordAcceptor, FiniteWordTransformer, IntoMealyMachine,
            IntoMooreMachine, MealyLike, MealyMachine, MooreLike, MooreMachine, NoColor,
            OmegaWordAcceptor, OmegaWordTransformer, StateBasedDBA, StateBasedDPA, WithInitial,
            DBA, DFA, DPA,
        },
        mapping::Morphism,
        ts::{
            dag::Dag,
            finite::ReachedState,
            operations::{Product, ProductIndex},
            predecessors::{IsPreTransition, PredecessorIterable},
            transition_system::{EdgeColorOf, Indexes, IsTransition, StateColorOf},
            Congruence, Deterministic, DeterministicEdgesFrom, EdgeColor, HasColor, HasColorMut,
            HasMutableStates, HasStates, IndexType, Sproutable, StateColor, ToDot,
            TransitionSystem, BTS,
        },
        upw,
        word::{FiniteWord, LinearWord, OmegaWord, Periodic, Reduced, ReducedParseError},
        Alphabet, Class, Color, FiniteLength, HasLength, InfiniteLength, Length, Pointed,
        RightCongruence, Show,
    };
    #[cfg(test)]
    pub use pretty_assertions::{assert_eq, assert_ne};
}

/// Module that contains definitions for dealing with alphabets.
pub mod alphabet;
pub use alphabet::Alphabet;
use impl_tools::autoimpl;

/// Defines lengths of finite and infinite words.
pub mod length;
use std::hash::Hash;

/// Module that contains definitions for dealing with lengths. This is particularly
/// useful for dealing with infinite words.
pub use length::{FiniteLength, HasLength, InfiniteLength, Length};

/// This module defines transition systems and successor functions and such.
#[macro_use]
pub mod ts;
pub use ts::{Pointed, TransitionSystem};

/// Defines automata and common types of combinations of transition system with acceptance condition.
#[allow(clippy::upper_case_acronyms)]
pub mod automata;
use automata::{MealyMachine, MooreMachine, DBA, DFA, DPA};

/// Defines congruence relations and congruence classes.
pub mod congruence;
pub use congruence::{Class, RightCongruence};

/// Module that contains definitions for dealing with words.
#[macro_use]
pub mod word;

/// Module that contains definitions for dealing with mappings.
pub mod mapping;

mod algorithms;

/// A color is simply a type that can be used to color states or transitions.
pub trait Color: Clone + Eq + Ord + Hash {
    /// Reduces a sequence of colors (of type `Self`) to a single color of type `Self`.
    fn reduce<I: IntoIterator<Item = Self>>(iter: I) -> Self
    where
        Self: Sized,
    {
        iter.into_iter().min().unwrap()
    }
}

impl<T: Eq + Ord + Clone + Hash> Color for T {}

/// Type alias for sets, we use this to hide which type of `HashSet` we are actually using.
pub type Set<S> = fxhash::FxHashSet<S>;
/// Type alias for maps, we use this to hide which type of `HashMap` we are actually using.
pub type Map<K, V> = fxhash::FxHashMap<K, V>;

pub trait Show {
    fn show(&self) -> String;
}

impl Show for usize {
    fn show(&self) -> String {
        self.to_string()
    }
}

impl Show for () {
    fn show(&self) -> String {
        "-".into()
    }
}

impl Show for bool {
    fn show(&self) -> String {
        match self {
            true => "+",
            false => "-",
        }
        .to_string()
    }
}

/// A partition is a different view on a congruence relation, by grouping elements of
/// type `I` into their respective classes under the relation.
#[derive(Debug, Clone)]
#[autoimpl(Deref using self.0)]
pub struct Partition<I: Hash + Eq>(Vec<Set<I>>);

impl<I: Hash + Eq> PartialEq for Partition<I> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().all(|o| other.contains(o))
    }
}
impl<I: Hash + Eq> Eq for Partition<I> {}

impl<I: Hash + Eq> Partition<I> {
    /// Builds a new congruence relation from an iterator that yields iterators
    /// which yield elements of type `I`.
    pub fn new<X: IntoIterator<Item = I>, Y: IntoIterator<Item = X>>(iter: Y) -> Self {
        Self(
            iter.into_iter()
                .map(|it| it.into_iter().collect::<Set<_>>())
                .collect(),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{alphabet, prelude::*};

    pub fn wiki_dfa() -> DFA<Simple> {
        let mut dfa = DFA::new(alphabet!(simple 'a', 'b'));
        let a = dfa.initial();
        dfa.set_initial_color(false);
        let b = dfa.add_state(false);
        let c = dfa.add_state(true);
        let d = dfa.add_state(true);
        let e = dfa.add_state(true);
        let f = dfa.add_state(false);

        dfa.add_edge(a, 'a', b, ());
        dfa.add_edge(a, 'b', c, ());
        dfa.add_edge(b, 'a', a, ());
        dfa.add_edge(b, 'b', d, ());
        dfa.add_edge(c, 'a', e, ());
        dfa.add_edge(c, 'b', f, ());
        dfa.add_edge(d, 'a', e, ());
        dfa.add_edge(d, 'b', f, ());
        dfa.add_edge(e, 'a', e, ());
        dfa.add_edge(e, 'b', f, ());
        dfa.add_edge(f, 'a', f, ());
        dfa.add_edge(f, 'b', f, ());

        dfa
    }
}
