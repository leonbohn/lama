//! Library for working with finite automata in Rust.
//!
#![warn(missing_docs)]
#![allow(unused)]
pub mod alphabet;

mod length;
use std::hash::Hash;

pub use length::{FiniteLength, InfiniteLength, Length};

pub mod ts;

pub mod acceptance;

mod automaton;

/// Module that contains definitions for dealing with words.
pub mod word;
pub use word::Word;

pub mod mapping;

pub trait Color: std::fmt::Debug + Clone + Eq + Ord + Hash {}

impl<T: Eq + Ord + std::fmt::Debug + Clone + Hash> Color for T {}
