use crate::{ts::Path, words::Length, State, Subword, Symbol};

/// Encapsulates an unsuccessful run of a transition system on some input, which is a run
/// that reaches a state such that no transition is available for the current input symbol.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FailedRun<Q, W: Subword> {
    path: Path<Q, W::S>,
    missing: W::S,
    word: W,
}

impl<Q: State, W: Subword> FailedRun<Q, W> {
    /// Creates a new [`FailedRun`] from a path, missing symbol and input word.
    pub fn new(path: Path<Q, W::S>, missing: W::S, word: W) -> Self {
        Self {
            path,
            missing,
            word,
        }
    }

    /// Computes the length of the successful computation.
    pub fn len(&self) -> usize {
        self.path.len()
    }

    /// Returns true if and only if the length of the successful computation
    /// is zero, i.e. no transition was taken.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns a reference to the successful (partial) computation.
    pub fn successful_path(&self) -> &Path<Q, W::S> {
        &self.path
    }

    /// Returns a reference to the missing transition.
    pub fn missing_symbol(&self) -> &W::S {
        &self.missing
    }

    /// Returns a reference to the state from which the transition system is left,
    /// i.e. the state for which a missing transition was encountered.
    pub fn reached(&self) -> &Q {
        self.path.reached()
    }

    /// Gives a reference to the input (word).
    pub fn input(&self) -> &W {
        &self.word
    }

    /// Returns the escaping suffix, that is whatever from the word is left and cannot be
    /// run on.
    pub fn suffix(&self) -> W::SuffixType {
        self.word.skip(self.len())
    }
}
