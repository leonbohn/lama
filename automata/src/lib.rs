//! Library for working with finite automata in Rust.
//!
#![warn(missing_docs)]
#![allow(unused)]

/// The prelude is supposed to make using this package easier. Including everything, i.e.
/// `use automata::prelude::*;` should be enough to use the package.
pub mod prelude {
    pub use super::{
        alphabet::{Expression, ExpressionOf, HasAlphabet, Simple, Symbol, SymbolOf},
        automata::{
            Acceptor, IsDba, IsDfa, IsDpa, MealyMachine, MooreMachine, Transformer, DBA, DFA, DPA,
            SBDBA, SBDPA,
        },
        simple,
        ts::{
            dag::Dag,
            operations::Product,
            predecessors::{IsPreTransition, PredecessorIterable},
            transition_system::{Indexes, IsTransition},
            Congruence, FiniteState, HasColor, HasColorMut, HasMutableStates, HasStates,
            Sproutable, ToDot, TransitionSystem,
        },
        upw,
        word::{Normalized, NormalizedParseError, NormalizedPeriodic, OmegaWord, Word},
        Alphabet, Class, Color, FiniteLength, HasLength, InfiniteLength, Length, Pointed,
        RightCongruence,
    };
}

/// Module that contains definitions for dealing with alphabets.
pub mod alphabet;
pub use alphabet::Alphabet;

/// Defines lengths of finite and infinite words.
pub mod length;
use std::hash::Hash;

/// Module that contains definitions for dealing with lengths. This is particularly
/// useful for dealing with infinite words.
pub use length::{FiniteLength, HasLength, InfiniteLength, Length};

/// This module defines transition systems and successor functions and such.
pub mod ts;
pub use ts::{Pointed, TransitionSystem};

/// Defines automata and common types of combinations of transition system with acceptance condition.
#[allow(clippy::upper_case_acronyms)]
pub mod automata;
use automata::{Acceptor, MealyMachine, MooreMachine, Transformer, DBA, DFA, DPA, SBDBA, SBDPA};

/// Defines congruence relations and congruence classes.
pub mod congruence;
pub use congruence::{Class, RightCongruence};

/// Module that contains definitions for dealing with words.
#[macro_use]
pub mod word;
pub use word::Word;

/// Module that contains definitions for dealing with mappings.
pub mod mapping;

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
