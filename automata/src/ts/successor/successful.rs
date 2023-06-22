use crate::{
    alphabet::{Alphabet, Symbol},
    length::{HasLength, RawPosition},
    ts::{
        finite::{ReachedColor, ReachedState, StateColorSequence, TransitionColorSequence},
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
    path: Path<'a, Ts::Alphabet, Ts::StateColor, Ts::EdgeColor>,
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

impl<'a, 'b, R, Ts: Successor> CanInduce<ReachedState> for Successful<'a, 'b, R, Ts> {
    fn induce(&self) -> ReachedState {
        ReachedState(self.path.reached())
    }
}

impl<'a, 'b, R, Ts: Successor> Successful<'a, 'b, R, Ts> {
    pub fn new(
        word: &'b R,
        ts: &'a Ts,
        path: Path<'a, Ts::Alphabet, Ts::StateColor, Ts::EdgeColor>,
    ) -> Self {
        Self { word, ts, path }
    }
}
