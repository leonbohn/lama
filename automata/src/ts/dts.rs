use crate::prelude::*;

use super::nts::{NTEdge, NTSEdgesFromIter, NTSEdgesTo};

#[derive(Clone)]
pub struct DTS<A: Alphabet, Q, C>(pub(crate) NTS<A, Q, C>);

impl<A: Alphabet, Q: Color, C: Color> TryFrom<NTS<A, Q, C>> for DTS<A, Q, C> {
    type Error = ();

    fn try_from(value: NTS<A, Q, C>) -> Result<Self, Self::Error> {
        if !value.is_deterministic() {
            return Err(());
        }
        Ok(Self(value))
    }
}

impl<A: Alphabet, Q: Color, C: Color> TryFrom<WithInitial<NTS<A, Q, C>>>
    for WithInitial<DTS<A, Q, C>>
{
    /// Only fails if nts is not deterministic.
    type Error = ();

    fn try_from(value: WithInitial<NTS<A, Q, C>>) -> Result<Self, Self::Error> {
        let (nts, initial) = value.into_parts();
        Ok(WithInitial::from((nts.try_into()?, initial)))
    }
}

impl<A: Alphabet, Q: Color, C: Color> TryFrom<&NTS<A, Q, C>> for DTS<A, Q, C> {
    type Error = ();

    fn try_from(value: &NTS<A, Q, C>) -> Result<Self, Self::Error> {
        if !value.is_deterministic() {
            return Err(());
        }
        Ok(Self(value.clone()))
    }
}

impl<A: Alphabet, Q: Color, C: Color> TryFrom<&WithInitial<NTS<A, Q, C>>>
    for WithInitial<DTS<A, Q, C>>
{
    /// Only fails if nts is not deterministic.
    type Error = ();

    fn try_from(value: &WithInitial<NTS<A, Q, C>>) -> Result<Self, Self::Error> {
        let (nts, initial) = value.clone().into_parts();
        Ok(WithInitial::from((nts.try_into()?, initial)))
    }
}

impl<A: Alphabet, Q: Color, C: Color> From<DTS<A, Q, C>> for NTS<A, Q, C> {
    fn from(value: DTS<A, Q, C>) -> Self {
        value.0
    }
}

impl<A: Alphabet, Q: Color, C: Color> TransitionSystem for DTS<A, Q, C> {
    type StateIndex = usize;

    type StateColor = Q;

    type EdgeColor = C;

    type TransitionRef<'this> = &'this NTEdge<A::Expression, C>
    where
        Self: 'this;

    type EdgesFromIter<'this> = NTSEdgesFromIter<'this, A::Expression, C>
    where
        Self: 'this;

    type StateIndices<'this> = std::ops::Range<usize>
    where
        Self: 'this;

    type Alphabet = A;

    fn alphabet(&self) -> &Self::Alphabet {
        self.0.alphabet()
    }

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.0.state_indices()
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        self.0.edges_from(state.to_index(self)?)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        self.0.state_color(state)
    }
}

impl<A: Alphabet, Q: Color, C: Color> Deterministic for DTS<A, Q, C> {
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        let mut it = self
            .0
            .edges_from(state.to_index(self)?)?
            .filter(|e| IsTransition::expression(e).matches(symbol));
        let out = it.next();
        //todo: See if this has performance impact
        assert!(it.next().is_none());
        out
    }
}

impl<A: Alphabet, Q: Color, C: Color> PredecessorIterable for DTS<A, Q, C> {
    type PreTransitionRef<'this> = &'this NTEdge<A::Expression, C>
    where
        Self: 'this;

    type EdgesToIter<'this> = NTSEdgesTo<'this, A::Expression, C>
    where
        Self: 'this;

    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        self.0.predecessors(state)
    }
}

impl<A: Alphabet, Q: Color, C: Color> Sproutable for DTS<A, Q, C> {
    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        Self(NTS::new_for_alphabet(alphabet))
    }

    fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
        self.0.add_state(color)
    }

    type ExtendStateIndexIter = std::ops::Range<usize>;

    fn extend_states<I: IntoIterator<Item = StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter {
        self.0.extend_states(iter)
    }

    fn set_state_color<X: Into<StateColor<Self>>>(&mut self, index: Self::StateIndex, color: X) {
        self.0.set_state_color(index, color)
    }

    fn add_edge<X, Y>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: EdgeColor<Self>,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Into<Self::StateIndex>,
        Y: Into<Self::StateIndex>,
    {
        let source = from.into();
        let target = to.into();
        on.for_each(|sym| assert!(self.transition(source, sym).is_none()));

        self.0.add_edge(source, on, target, color)
    }

    fn remove_edge(
        &mut self,
        from: Self::StateIndex,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool {
        unimplemented!("Might be added but might not be")
    }
}
