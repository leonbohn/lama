//! A library for learning automata from data.
#![warn(missing_docs)]
#![allow(unused)]

/// This module deals with acceptance conditions and their inference.
pub mod acceptance;
/// Greedily LEarn Right Congruence algorithm, an algorithm that infers a
/// right congruence relation from a consistency function.
// pub mod glerc;
mod priority_mapping;
pub use priority_mapping::{AnnotatedCongruence, Annotation};

/// Deals with active learning algorithms such as L*.
pub mod active;

/// Contains passive learners such as RPNI, DBAInf and DPAInf.
pub mod passive;

pub(crate) mod prefixtree;

#[cfg(test)]
mod tests {}
