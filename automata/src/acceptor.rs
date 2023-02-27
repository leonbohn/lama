use crate::{ts::TransitionSystem, words::Word};

/// Implemented by objects which can accept a word. We use `W` as an ipnut type parameter to allow for different implementations based on the type of word.
pub trait Acceptor<W: Word> {
    /// The transition system type.
    type TS: TransitionSystem;

    /// Returns true iff the given `word` is accepted, i.e. it satisfies the acceptance condition.
    fn accepts(&self, word: &W) -> bool;
}
