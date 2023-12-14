use crate::ts::Path;

use crate::ts::path::PathIn;
use crate::ts::TransitionSystem;

/// Represents a partial run through a transition system. It consists of a word, a transition system, a position
/// in the word and a path through the transition system.
#[derive(Clone)]
pub struct Partial<R, Ts: TransitionSystem> {
    word: R,
    ts: Ts,
    position: usize,
    path: PathIn<Ts>,
}

impl<R, Ts: TransitionSystem> Partial<R, Ts> {
    /// Returns the stored path.
    pub fn path(self) -> PathIn<Ts> {
        self.path
    }

    /// Creates a new partial run from its constituent parts.
    pub fn new(word: R, ts: Ts, position: usize, path: PathIn<Ts>) -> Self {
        Self {
            word,
            ts,
            position,
            path,
        }
    }
}
