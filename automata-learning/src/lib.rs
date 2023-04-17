//! A library for learning automata from data.
#![warn(missing_docs)]

/// This module deals with acceptance conditions and their inference.
pub mod acceptance;
/// Greedily LEarn Right Congruence algorithm, an algorithm that infers a
/// right congruence relation from a consistency function.
pub mod glerc;
/// Definitions for dealing with samples, which are pairs of collections of
/// positive and negative instances.
pub mod sample;

#[cfg(test)]
mod tests {}