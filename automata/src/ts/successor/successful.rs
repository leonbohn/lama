use crate::{
    alphabet::{Alphabet, Symbol},
    length::{HasLength, RawPosition},
    ts::{
        finite::{
            ReachedColor, ReachedState, SeenColors, StateColorSequence, TransitionColorSequence,
        },
        infinite::InfinitySet,
        path::ColorSequence,
        CanInduce, EdgeColor, OnEdges, Path, StateColor, StateIndex, TransitionSystem,
    },
    word::{RawWithLength, Rawpresentation},
    Color, FiniteLength, Length,
};

use super::Successor;

#[derive(Debug, Clone, PartialEq)]
pub struct Successful<'a, 'b, R, Ts: Successor> {
    word: &'b R,
    ts: &'a Ts,
    path: Path<Ts::Alphabet, Ts::StateIndex, Ts::Color, Ts::Position>,
    loop_index: Option<usize>,
}

impl<'a, 'b, R, Ts> CanInduce<SeenColors<Ts::Color>> for Successful<'a, 'b, R, Ts>
where
    Ts: Successor,
    Path<Ts::Alphabet, Ts::StateIndex, Ts::Color, Ts::Position>: ColorSequence<Ts::Color>,
    Ts::Color: Clone,
{
    fn induce(&self) -> SeenColors<Ts::Color> {
        SeenColors(self.path.colors_vec())
    }
}

impl<'a, 'b, R, Ts> CanInduce<InfinitySet<Ts::Color>> for Successful<'a, 'b, R, Ts>
where
    Ts: Successor,
    Path<Ts::Alphabet, Ts::StateIndex, Ts::Color, Ts::Position>: ColorSequence<Ts::Color>,
    Ts::Color: Clone,
{
    fn induce(&self) -> InfinitySet<Ts::Color> {
        InfinitySet(
            (self
                .loop_index
                .expect("Cannot get the infinity set of a finite run!")
                ..self.path.colors_length())
                .map(|i| {
                    self.path
                        .nth_color(i)
                        .expect("The length does not match!")
                        .clone()
                })
                .collect(),
        )
    }
}

impl<'a, 'b, R, Ts> CanInduce<ReachedColor<Ts::Color>> for Successful<'a, 'b, R, Ts>
where
    Ts: Successor,
    Path<Ts::Alphabet, Ts::StateIndex, Ts::Color, Ts::Position>: ColorSequence<Ts::Color>,
    Ts::Color: Clone,
{
    fn induce(&self) -> ReachedColor<Ts::Color> {
        ReachedColor(self.path.last().unwrap().clone())
    }
}

impl<'a, 'b, R, Ts: Successor> CanInduce<ReachedState<Ts::StateIndex>>
    for Successful<'a, 'b, R, Ts>
{
    fn induce(&self) -> ReachedState<Ts::StateIndex> {
        ReachedState(self.path.reached())
    }
}

impl<'a, 'b, R, Ts: Successor<Position = OnEdges>> CanInduce<TransitionColorSequence<Ts::Color>>
    for Successful<'a, 'b, R, Ts>
{
    fn induce(&self) -> TransitionColorSequence<Ts::Color> {
        TransitionColorSequence(self.path.colors_vec())
    }
}

impl<'a, 'b, R, Ts: Successor> Successful<'a, 'b, R, Ts> {
    pub fn new(
        word: &'b R,
        ts: &'a Ts,
        loop_index: Option<usize>,
        path: Path<Ts::Alphabet, Ts::StateIndex, Ts::Color, Ts::Position>,
    ) -> Self {
        Self {
            word,
            ts,
            path,
            loop_index,
        }
    }
}
