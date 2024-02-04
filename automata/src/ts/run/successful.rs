use std::collections::BTreeSet;

use crate::{
    ts::{
        finite::{InfinityColors, ReachedColor, ReachedState, SeenColors, TransitionColorSequence},
        infinite::InfinityStateColors,
        path::PathIn,
        CanInduce, Deterministic, EdgeColor, Path,
    },
    Color,
};

use crate::ts::TransitionSystem;

/// Represents a successful run through a transition system. It consists of a word, a transition system, a position
/// in the word and a path through the transition system.
#[derive(Clone)]
pub struct Successful<R, Ts: Deterministic> {
    word: R,
    ts: Ts,
    path: PathIn<Ts>,
    loop_index: Option<usize>,
}

impl<R, Ts: Deterministic> Successful<R, Ts> {
    /// Returns a reference to the path underlying the successful run.
    pub fn path(&self) -> &PathIn<Ts> {
        &self.path
    }

    /// Returns the loop index of self, if it exists. The loop index is the position to which
    /// the run "jumps back".
    pub fn loop_index(&self) -> Option<usize> {
        self.loop_index
    }
}

impl<R, Ts> CanInduce<SeenColors<Ts::StateColor>> for Successful<R, Ts>
where
    Ts: Deterministic,
    Ts::StateColor: Color,
{
    fn induce(&self) -> SeenColors<Ts::StateColor> {
        SeenColors(self.path.state_colors().cloned().collect())
    }
}

impl<R, Ts> CanInduce<InfinityColors<Ts::EdgeColor>> for Successful<R, Ts>
where
    Ts: Deterministic,
    EdgeColor<Ts>: Color,
{
    fn induce(&self) -> InfinityColors<Ts::EdgeColor> {
        InfinityColors(self.path.edge_colors().cloned().collect())
    }
}

impl<R, Ts> CanInduce<InfinityStateColors<Ts::StateColor>> for Successful<R, Ts>
where
    Ts: Deterministic,
    Ts::StateColor: Color,
{
    fn induce(&self) -> InfinityStateColors<Ts::StateColor> {
        let state_colors: BTreeSet<_> = self
            .path
            .state_colors()
            .skip(self.loop_index.unwrap_or(0))
            .cloned()
            .collect();
        debug_assert!(!state_colors.is_empty());
        InfinityStateColors(state_colors)
    }
}

impl<R, Ts> CanInduce<ReachedColor<Ts::StateColor>> for Successful<R, Ts>
where
    Ts: Deterministic,
    Ts::StateColor: Color,
{
    fn induce(&self) -> ReachedColor<Ts::StateColor> {
        ReachedColor(self.path.reached_state_color())
    }
}

impl<R, Ts: Deterministic> CanInduce<ReachedState<Ts::StateIndex>> for Successful<R, Ts> {
    fn induce(&self) -> ReachedState<Ts::StateIndex> {
        ReachedState(self.path.reached())
    }
}

impl<R, Ts: Deterministic> CanInduce<TransitionColorSequence<Ts::EdgeColor>> for Successful<R, Ts>
where
    EdgeColor<Ts>: Color,
{
    fn induce(&self) -> TransitionColorSequence<Ts::EdgeColor> {
        TransitionColorSequence(self.path.edge_colors().cloned().collect())
    }
}

impl<R, Ts: Deterministic> Successful<R, Ts> {
    /// Creates a new successful run from its constituent parts.
    pub fn new(word: R, ts: Ts, loop_index: Option<usize>, path: PathIn<Ts>) -> Self {
        Self {
            word,
            ts,
            path,
            loop_index,
        }
    }
}
