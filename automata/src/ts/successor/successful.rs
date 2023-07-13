use crate::{
    alphabet::{Alphabet, Symbol},
    length::{HasLength, RawPosition},
    ts::{
        finite::{ReachedColor, ReachedState, StateColorSequence, TransitionColorSequence},
        infinite::InfinitySet,
        CanInduce, Path, StateIndex, TransitionSystem,
    },
    word::RawWithLength,
    Color, FiniteLength, Length,
};

use super::Successor;

#[derive(Debug, Clone, PartialEq)]
pub struct Successful<'a, 'b, R, Ts: Successor> {
    word: &'b R,
    ts: &'a Ts,
    path: Path<'a, Ts::Alphabet, Ts::Index, Ts::StateColor, Ts::EdgeColor>,
    loop_index: Option<usize>,
}

impl<'a, 'b, R, Ts: Successor> CanInduce<InfinitySet<Ts::EdgeColor>> for Successful<'a, 'b, R, Ts> {
    fn induce(&self) -> InfinitySet<Ts::EdgeColor> {
        InfinitySet(
            self.path
                .transition_colors()
                .skip(
                    self.loop_index
                        .expect("Do not try to get the infinity set of a finite run!"),
                )
                .cloned()
                .collect(),
        )
    }
}

impl<'a, 'b, R, Ts: Successor> CanInduce<TransitionColorSequence<Ts::EdgeColor>>
    for Successful<'a, 'b, R, Ts>
{
    fn induce(&self) -> TransitionColorSequence<Ts::EdgeColor> {
        TransitionColorSequence(self.path.transition_colors().cloned().collect())
    }
}

impl<'a, 'b, R, Ts: Successor> CanInduce<StateColorSequence<Ts::StateColor>>
    for Successful<'a, 'b, R, Ts>
{
    fn induce(&self) -> StateColorSequence<Ts::StateColor> {
        StateColorSequence(self.path.state_colors().cloned().collect())
    }
}

impl<'a, 'b, R, Ts: Successor> CanInduce<ReachedColor<Ts::StateColor>>
    for Successful<'a, 'b, R, Ts>
{
    fn induce(&self) -> ReachedColor<Ts::StateColor> {
        ReachedColor(self.ts.state_color(self.path.reached()).clone())
    }
}

impl<'a, 'b, R, Ts: Successor> CanInduce<ReachedState<Ts::Index>> for Successful<'a, 'b, R, Ts> {
    fn induce(&self) -> ReachedState<Ts::Index> {
        ReachedState(self.path.reached())
    }
}

impl<'a, 'b, R, Ts: Successor> Successful<'a, 'b, R, Ts> {
    pub fn new(
        word: &'b R,
        ts: &'a Ts,
        loop_index: Option<usize>,
        path: Path<'a, Ts::Alphabet, Ts::Index, Ts::StateColor, Ts::EdgeColor>,
    ) -> Self {
        Self {
            word,
            ts,
            path,
            loop_index,
        }
    }
}
