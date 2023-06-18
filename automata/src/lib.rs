//! Library for working with finite automata in Rust.
//!
#![warn(missing_docs)]

mod alphabet;
pub use alphabet::Alphabet;

mod ts;

pub mod word {}
