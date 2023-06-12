use tracing::trace;
mod induced_path;
mod partial_run;
/// Allows the evaluation of a run.
mod result;

use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
};

use crate::{
    ts::{InputOf, Path, StateOf, Successor, TransitionOf},
    words::{HasLength, Length, LengthOf, Word},
    Pointed, Set, State, Subword, Symbol, Trigger, Value,
};

pub use self::induced_path::{InducedPath, InfinitySet, ReachedState};
pub use partial_run::FailedRun;

/// A struct which deals with running words (be they finite or infinite) in a
/// transition system (implementor of [`Successor`]). For this purpose, it maintains
/// a position (in the raw representation of the input), a path representing the
/// successful computation that was already made and a set of position-state pairs
/// which were already encountered.
///
/// # Finite inputs
/// In this case, the computation is straightforward, we simply walk along the input
/// until we have reached the terminal position (this is checked via the [`Length`] of
/// the word, specifically via the [`Length::is_end()`] method).
///
/// # Infinite inputs
/// The situation is more involved in this case. As we assume the input to be representable,
/// it has an [`super::InfiniteLength`] associated with it. This is used to calculate an
/// updated raw position after taking a transition. We merely increment the current position
/// and subsequently use the [`Length::calculate_raw_position`] method to compute the update.
/// If a missing transition is encountered, we proceed as for the finite case and extract
/// the partial (unsuccessful) run that was already done. Otherwise, it must be that at some
/// point, the computation revisits a state while being in the same (raw) position in the
/// input, meaning that the computation has entered a loop. In this case, we can extract
/// the infinity set of the (successful) run and return it.
#[derive(Debug, Clone)]
pub struct Cane<TS: Successor, W: Subword<S = TS::Sigma>> {
    ts: TS,
    word: W,
    position: usize,
    seen: Set<(usize, TS::Q)>,
    seq: Path<TS::Q, TS::Sigma>,
}

type RunResult<TS, W> =
    Result<InducedPath<StateOf<TS>, InputOf<TS>, LengthOf<W>>, FailedRun<StateOf<TS>, W>>;

impl<TS: Successor, W: Subword<S = TS::Sigma>> Cane<TS, W> {
    /// Create a new struct from the given word, transition system and source state.
    pub fn new(word: W, ts: TS, source: TS::Q) -> Self {
        Self {
            ts,
            word,
            position: 0,
            seen: Set::new(),
            seq: Path::empty(source),
        }
    }

    fn step(&mut self) -> Option<TransitionOf<TS>> {
        // fail if the end of the input was reached
        self.position = self.word.length().calculate_raw_position(self.position)?;

        if let Some(transition) = self
            .word
            .nth(self.position)
            .and_then(|sym| self.ts.transition_for(self.seq.reached(), sym))
        {
            self.position += 1;
            if self
                .seen
                .insert((self.position, transition.source().clone()))
            {
                self.seq
                    .extend_with(transition.sym().clone(), transition.2.clone());
                Some(transition)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn take_all_steps(&mut self) {
        while let Some(transition) = self.step() {
            trace!("Took transition {:?}", transition);
        }
    }

    fn current_sym(&self) -> Option<TS::Sigma> {
        self.word.nth(self.position)
    }

    fn extract_partial_run(self) -> FailedRun<TS::Q, W> {
        let Some(sym) = self.current_sym() else {
            panic!("Cannot extract partial run if run has been completed!")
        };
        debug_assert!(
            self.word.length().is_end(self.position)
                || self.ts.successor(self.seq.reached(), &sym).is_none(),
        );
        FailedRun::new(self.seq, sym, self.word)
    }

    /// Performs as many computation steps as possible and returns the result of evaluating
    /// the run. If successful (i.e. all transitions exist), the function returns an
    /// [`InducedPath`] of the same [`Length`] as the input that was given. Otherwise,
    /// it returns a [`PartialRun`] containing all transitions that were successfully taken.
    pub fn result(mut self) -> RunResult<TS, W> {
        self.take_all_steps();
        let len = self.word.length();
        if len.is_end(self.position)
            || self
                .seen
                .contains(&(self.position, self.seq.reached().clone()))
        {
            // Either we are at the end of the word, which means the
            // input is finite and we are done reading it, or we have
            // reached a point where we are at the 'same' position of
            // the input and in the same state and thus we have per-
            // formed a full loop. Either way we can return the
            // induced path.
            Ok(InducedPath::new(self.seq, len, self.position))
        } else {
            // partial infinite run
            Err(self.extract_partial_run())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ts::{transitionsystem::TransitionSystem, AnonymousGrowable, Growable},
        upw,
        words::Str,
    };

    use super::*;

    #[test]
    fn input_to_run() {
        let mut ts: TransitionSystem<u32> = TransitionSystem::new();
        let q0 = ts.add_new_state();
        let q1 = ts.add_new_state();
        let q2 = ts.add_new_state();
        ts.add_transition(&q0, 'a', &q1);
        ts.add_transition(&q0, 'b', &q0);
        ts.add_transition(&q1, 'a', &q2);
        ts.add_transition(&q1, 'b', &q0);
        ts.add_transition(&q2, 'b', &q0);

        let upw = upw!("a", "b");
        assert_eq!(ts.run_from("abba", q0), Ok(ReachedState(q1)));
        assert_eq!(
            ts.run_from(upw, q0),
            Ok(InfinitySet(Set::from_iter([(q0, 'b')])))
        );

        assert_eq!(
            ts.run_from("aaa", q0),
            Err(FailedRun::new(
                Path::new([q0, q1, q2], ['a', 'a']),
                'a',
                "aaa"
            ))
        );
    }
}
