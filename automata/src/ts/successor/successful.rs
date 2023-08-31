use crate::{
    alphabet::{Alphabet, Symbol},
    length::{HasLength, RawPosition},
    ts::{
        finite::{
            InfinityColors, ReachedColor, ReachedState, SeenColors, StateColorSequence,
            TransitionColorSequence,
        },
        infinite::InfinityStateColors,
        CanInduce, EdgeColor, Path, StateColor, StateIndex, TransitionSystem,
    },
    word::OmegaWord,
    Color, FiniteLength, Length,
};

use super::Successor;

#[derive(Debug, Clone, PartialEq)]
pub struct Successful<'a, 'b, R, Ts: Successor> {
    word: &'b R,
    ts: &'a Ts,
    path: Path<Ts::Alphabet, Ts::StateIndex, Ts::StateColor, Ts::EdgeColor>,
    loop_index: Option<usize>,
}

impl<'a, 'b, R, Ts> CanInduce<SeenColors<Ts::StateColor>> for Successful<'a, 'b, R, Ts>
where
    Ts: Successor,
    Ts::StateColor: Clone,
{
    fn induce(&self) -> SeenColors<Ts::StateColor> {
        SeenColors(self.path.state_colors().collect())
    }
}

impl<'a, 'b, R, Ts> CanInduce<InfinityColors<Ts::EdgeColor>> for Successful<'a, 'b, R, Ts>
where
    Ts: Successor,
    Ts::StateColor: Clone,
{
    fn induce(&self) -> InfinityColors<Ts::EdgeColor> {
        InfinityColors(self.path.transition_colors().collect())
    }
}

impl<'a, 'b, R, Ts> CanInduce<InfinityStateColors<Ts::StateColor>> for Successful<'a, 'b, R, Ts>
where
    Ts: Successor,
    Ts::StateColor: Clone,
{
    fn induce(&self) -> InfinityStateColors<Ts::StateColor> {
        debug_assert!(self.path.state_colors_len() > 0);
        InfinityStateColors(
            self.path
                .state_colors()
                .skip(self.loop_index.unwrap_or(0))
                .collect(),
        )
    }
}

impl<'a, 'b, R, Ts> CanInduce<ReachedColor<Ts::StateColor>> for Successful<'a, 'b, R, Ts>
where
    Ts: Successor,
    Ts::StateColor: Clone,
{
    fn induce(&self) -> ReachedColor<Ts::StateColor> {
        ReachedColor(self.path.state_colors_last().clone())
    }
}

impl<'a, 'b, R, Ts: Successor> CanInduce<ReachedState<Ts::StateIndex>>
    for Successful<'a, 'b, R, Ts>
{
    fn induce(&self) -> ReachedState<Ts::StateIndex> {
        ReachedState(self.path.reached())
    }
}

impl<'a, 'b, R, Ts: Successor> CanInduce<TransitionColorSequence<Ts::EdgeColor>>
    for Successful<'a, 'b, R, Ts>
{
    fn induce(&self) -> TransitionColorSequence<Ts::EdgeColor> {
        TransitionColorSequence(self.path.transition_colors().collect())
    }
}

impl<'a, 'b, R, Ts: Successor> Successful<'a, 'b, R, Ts> {
    pub fn new(
        word: &'b R,
        ts: &'a Ts,
        loop_index: Option<usize>,
        path: Path<Ts::Alphabet, Ts::StateIndex, Ts::StateColor, Ts::EdgeColor>,
    ) -> Self {
        Self {
            word,
            ts,
            path,
            loop_index,
        }
    }
}
