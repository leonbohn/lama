use std::fmt::Debug;

mod acceptance;
mod acceptor;
mod automaton;
mod coloring;
mod run;
mod ts;
pub mod words;

#[cfg(feature = "det")]
mod with_acceptance;
#[cfg(feature = "det")]
pub use with_acceptance::WithAcceptance;

mod boundedness;
pub use boundedness::{Boundedness, FiniteKind, InfiniteKind};

pub trait Alphabet: Clone + Eq + Debug + PartialEq {}

impl<C: Clone + Eq + Debug> Alphabet for C {}

#[cfg(test)]
mod tests {}
