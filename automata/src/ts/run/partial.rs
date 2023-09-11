use crate::ts::Path;

use crate::ts::TransitionSystem;

/// Represents a partial run through a transition system. It consists of a word, a transition system, a position
/// in the word and a path through the transition system.
#[derive(Debug, Clone, PartialEq)]
pub struct Partial<'a, 'b, R, Ts: TransitionSystem> {
    word: &'b R,
    ts: &'a Ts,
    position: usize,
    path: Path<Ts::Alphabet, Ts::StateIndex>,
}

impl<'a, 'b, R, Ts: TransitionSystem> Partial<'a, 'b, R, Ts> {
    /// Returns the stored path.
    pub fn path(self) -> Path<Ts::Alphabet, Ts::StateIndex> {
        self.path
    }

    /// Creates a new partial run from its constituent parts.
    pub fn new(
        word: &'b R,
        ts: &'a Ts,
        position: usize,
        path: Path<Ts::Alphabet, Ts::StateIndex>,
    ) -> Self {
        Self {
            word,
            ts,
            position,
            path,
        }
    }
}
