use crate::{
    ts::{predecessors::PredecessorIterable, transition_system::IsEdge},
    TransitionSystem,
};

#[derive(Clone, Debug)]
pub struct Reversed<Ts>(pub Ts);

impl<'ts, E, Idx, C, T: IsEdge<'ts, E, Idx, C>> IsEdge<'ts, E, Idx, C> for Reversed<T> {
    fn source(&self) -> Idx {
        self.0.target()
    }

    fn target(&self) -> Idx {
        self.0.source()
    }

    fn color(&self) -> C {
        self.0.color()
    }

    fn expression(&self) -> &'ts E {
        self.0.expression()
    }
}

impl<Ts> TransitionSystem for Reversed<Ts>
where
    Ts: PredecessorIterable,
{
    type Alphabet = Ts::Alphabet;

    type StateIndex = Ts::StateIndex;

    type StateColor = Ts::StateColor;

    type EdgeColor = Ts::EdgeColor;

    type TransitionRef<'this> = Reversed<Ts::PreTransitionRef<'this>>
    where
        Self: 'this;

    type EdgesFromIter<'this> = std::iter::Map<Ts::EdgesToIter<'this>, fn(Ts::PreTransitionRef<'this>) -> Reversed<Ts::PreTransitionRef<'this>>>
    where
        Self: 'this;

    type StateIndices<'this> = Ts::StateIndices<'this>
    where
        Self: 'this;

    fn alphabet(&self) -> &Self::Alphabet {
        self.0.alphabet()
    }

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.0.state_indices()
    }

    fn edges_from<Idx: crate::ts::transition_system::Indexes<Self>>(
        &self,
        state: Idx,
    ) -> Option<Self::EdgesFromIter<'_>> {
        Some(
            self.0
                .predecessors(state.to_index(self)?)?
                .map(|e| Reversed(e)),
        )
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        self.0.state_color(state)
    }
}

impl<Ts> PredecessorIterable for Reversed<Ts>
where
    Ts: TransitionSystem + PredecessorIterable,
{
    type PreTransitionRef<'this> = Reversed<Ts::TransitionRef<'this>>
    where
        Self: 'this;

    type EdgesToIter<'this> = std::iter::Map<Ts::EdgesFromIter<'this>, fn(Ts::TransitionRef<'this>) -> Reversed<Ts::TransitionRef<'this>>>
    where
        Self: 'this;

    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        Some(self.0.edges_from(state)?.map(|e| Reversed(e)))
    }
}
