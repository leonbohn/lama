//! Library for working with finite automata in Rust.
//!
#![warn(missing_docs)]

mod alphabet;
pub use alphabet::Alphabet;
mod length;
pub use length::{FiniteLength, InfiniteLength, Length};

mod ts;

/// Module that contains definitions for dealing with words.
pub mod word;
