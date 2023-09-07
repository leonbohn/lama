mod sample;
use automata::{alphabet::Alphabet, FiniteLength, InfiniteLength, Word, DBA, DFA, DPA};
pub use sample::*;
use tracing::trace;

/// Module containing the implementations of the sprout/glerc algorithm.
pub mod sprout;

/// Module containing definitions and implementations for families of weak priority mappings.
pub mod fwpm;

/// Executes the RPNI algorithm on the given sample. This returns a DFA that is
/// composed of a right congruence as well as an acceptance condition, which marks
/// a classes as accepting if it is reached by a positive sample word.
pub fn dfa_rpni<A: Alphabet>(sample: &Sample<A, FiniteLength>) -> DFA<A> {
    todo!()
}

/// Executes a variant of the RPNI algorithm for omega-words, producing a DBA.
pub fn dba_rpni<A: Alphabet>(sample: &Sample<A, InfiniteLength>) -> DBA<A> {
    todo!()
}

/// Similar to [`dba_rpni`], but produces a DPA instead.
pub fn dpa_rpni<A: Alphabet>(sample: &Sample<A, InfiniteLength>) -> DPA<A> {
    todo!()
}
