//! Library for working with finite automata in Rust.
//!
// #![warn(missing_docs)]
#![allow(unused)]

/// Module that contains definitions for dealing with alphabets.
pub mod alphabet;
pub use alphabet::Alphabet;

mod length;
use std::hash::Hash;

/// Module that contains definitions for dealing with lengths. This is particularly
/// useful for dealing with infinite words.
pub use length::{FiniteLength, InfiniteLength, Length};

/// This module defines transition systems and successor functions and such.
pub mod ts;

#[allow(clippy::upper_case_acronyms)]
mod automaton;
pub use automaton::{
    Acceptor, MealyMachine, MooreMachine, Transformer, DBA, DFA, DPA, SBDBA, SBDPA,
};

/// Module that contains definitions for dealing with words.
#[macro_use]
pub mod word;
pub use word::Word;

/// Module that contains definitions for dealing with mappings.
pub mod mapping;

/// A color is simply a type that can be used to color states or transitions.
pub trait Color: std::fmt::Debug + Clone + Eq + Ord + Hash {}

impl<T: Eq + Ord + std::fmt::Debug + Clone + Hash> Color for T {}

pub type Set<S> = std::collections::HashSet<S>;
pub type Map<K, V> = std::collections::HashMap<K, V>;
