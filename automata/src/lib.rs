use std::fmt::Debug;

mod acceptance;
mod automaton;
mod coloring;
mod run;
mod ts;
pub mod words;

mod boundedness;
pub use boundedness::{Boundedness, FiniteKind, InfiniteKind};

pub trait Alphabet: Clone {
    type C: Clone + Eq + Ord + Debug;
}

impl<C: Clone + Eq + Ord + Debug> Alphabet for C {
    type C = C;
}

#[cfg(test)]
mod tests {}
