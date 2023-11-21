//! A library for learning automata from data.
#![allow(missing_docs)]
#![allow(unused)]

/// Contains passive learners such as RPNI, DBAInf and DPAInf.
#[macro_use]
pub mod passive;

/// This module deals with acceptance conditions and their inference.
pub mod acceptance;
/// Greedily LEarn Right Congruence algorithm, an algorithm that infers a
/// right congruence relation from a consistency function.
// pub mod glerc;
mod priority_mapping;
use automata::prelude::*;
pub use priority_mapping::{AnnotatedCongruence, Annotation};

/// Deals with active learning algorithms such as L*.
pub mod active;

pub(crate) mod prefixtree;

#[cfg(test)]
mod tests {}
