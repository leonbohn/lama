use std::collections::BTreeSet;

use crate::ts::{
    finite::{InfinityColors, ReachedColor, ReachedState, SeenColors, TransitionColorSequence},
    infinite::InfinityStateColors,
    CanInduce, Path,
};

use crate::ts::TransitionSystem;

/// Represents a successful run through a transition system. It consists of a word, a transition system, a position
/// in the word and a path through the transition system.
#[derive(Debug, Clone, PartialEq)]
pub struct Successful<'a, 'b, R, Ts: TransitionSystem> {
    word: &'b R,
    ts: &'a Ts,
    path: Path<Ts::Alphabet, Ts::StateIndex>,
    loop_index: Option<usize>,
}

impl<'a, 'b, R, Ts: TransitionSystem> Successful<'a, 'b, R, Ts> {
    pub fn path(&self) -> &Path<Ts::Alphabet, Ts::StateIndex> {
        &self.path
    }

    pub fn loop_index(&self) -> Option<usize> {
        self.loop_index
    }
}

impl<'a, 'b, R, Ts> CanInduce<SeenColors<Ts::StateColor>> for Successful<'a, 'b, R, Ts>
where
    Ts: TransitionSystem,
    Ts::StateColor: Clone,
{
    fn induce(&self) -> SeenColors<Ts::StateColor> {
        SeenColors(self.path.state_colors(self.ts).collect())
    }
}

impl<'a, 'b, R, Ts> CanInduce<InfinityColors<Ts::EdgeColor>> for Successful<'a, 'b, R, Ts>
where
    Ts: TransitionSystem,
    Ts::StateColor: Clone,
{
    fn induce(&self) -> InfinityColors<Ts::EdgeColor> {
        InfinityColors(self.path.edge_colors(self.ts).collect())
    }
}

impl<'a, 'b, R, Ts> CanInduce<InfinityStateColors<Ts::StateColor>> for Successful<'a, 'b, R, Ts>
where
    Ts: TransitionSystem,
    Ts::StateColor: Clone,
{
    fn induce(&self) -> InfinityStateColors<Ts::StateColor> {
        let state_colors: BTreeSet<_> = self
            .path
            .state_colors(self.ts)
            .skip(self.loop_index.unwrap_or(0))
            .collect();
        debug_assert!(!state_colors.is_empty());
        InfinityStateColors(state_colors)
    }
}

impl<'a, 'b, R, Ts> CanInduce<ReachedColor<Ts::StateColor>> for Successful<'a, 'b, R, Ts>
where
    Ts: TransitionSystem,
    Ts::StateColor: Clone,
{
    fn induce(&self) -> ReachedColor<Ts::StateColor> {
        ReachedColor(self.path.reached_state_color(self.ts))
    }
}

impl<'a, 'b, R, Ts: TransitionSystem> CanInduce<ReachedState<Ts::StateIndex>>
    for Successful<'a, 'b, R, Ts>
{
    fn induce(&self) -> ReachedState<Ts::StateIndex> {
        ReachedState(self.path.reached())
    }
}

impl<'a, 'b, R, Ts: TransitionSystem> CanInduce<TransitionColorSequence<Ts::EdgeColor>>
    for Successful<'a, 'b, R, Ts>
{
    fn induce(&self) -> TransitionColorSequence<Ts::EdgeColor> {
        TransitionColorSequence(self.path.edge_colors(self.ts).collect())
    }
}

impl<'a, 'b, R, Ts: TransitionSystem> Successful<'a, 'b, R, Ts> {
    /// Creates a new successful run from its constituent parts.
    pub fn new(
        word: &'b R,
        ts: &'a Ts,
        loop_index: Option<usize>,
        path: Path<Ts::Alphabet, Ts::StateIndex>,
    ) -> Self {
        Self {
            word,
            ts,
            path,
            loop_index,
        }
    }
}
