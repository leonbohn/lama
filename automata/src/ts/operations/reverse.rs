use crate::{ts::predecessors::PredecessorIterable, TransitionSystem};

#[derive(Clone, Debug)]
pub struct Reversed<Ts>(pub Ts);

impl<Ts> TransitionSystem for Reversed<Ts>
where
    Ts: PredecessorIterable,
{
    type Alphabet = Ts::Alphabet;

    type StateIndex = Ts::StateIndex;

    type StateColor = Ts::StateColor;

    type EdgeColor = Ts::EdgeColor;

    type TransitionRef<'this> = Ts::PreTransitionRef<'this>
    where
        Self: 'this;

    type EdgesFromIter<'this> = Ts::EdgesToIter<'this>
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
        self.0.predecessors(state.to_index(self)?)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        self.0.state_color(state)
    }
}

impl<Ts> PredecessorIterable for Reversed<Ts>
where
    Ts: TransitionSystem + PredecessorIterable,
{
    type PreTransitionRef<'this> = Ts::TransitionRef<'this>
    where
        Self: 'this;

    type EdgesToIter<'this> = Ts::EdgesFromIter<'this>
    where
        Self: 'this;

    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        self.0.edges_from(state)
    }
}
