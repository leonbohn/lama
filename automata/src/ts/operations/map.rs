use std::marker::PhantomData;

use crate::{
    alphabet::HasAlphabet,
    ts::{
        predecessors::IsPreTransition, transition_system::IsTransition, FiniteState,
        FiniteStatesIterType, HasFiniteStates, IndexType,
    },
    Color, Pointed, TransitionSystem,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapEdgeColor<Ts, F> {
    ts: Ts,
    f: F,
}

impl<Ts, F> MapEdgeColor<Ts, F> {
    pub fn new(ts: Ts, f: F) -> Self {
        Self { ts, f }
    }

    pub fn f(&self) -> &F {
        &self.f
    }

    pub fn ts(&self) -> &Ts {
        &self.ts
    }
}

impl<D: Color, Ts: TransitionSystem + Pointed, F: Fn(Ts::EdgeColor) -> D> Pointed
    for MapEdgeColor<Ts, F>
{
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

impl<Ts: TransitionSystem, F> HasAlphabet for MapEdgeColor<Ts, F> {
    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappedTransition<T, F, C> {
    transition: T,
    f: F,
    _old_color: PhantomData<C>,
}

impl<T, F, C> MappedTransition<T, F, C> {
    pub fn new(transition: T, f: F) -> Self {
        Self {
            transition,
            f,
            _old_color: PhantomData,
        }
    }
}

impl<Idx: IndexType, E, C: Color, D: Color, F: Fn(C) -> D, T: IsTransition<E, Idx, C>>
    IsTransition<E, Idx, D> for MappedTransition<T, F, C>
{
    fn target(&self) -> Idx {
        self.transition.target()
    }

    fn color(&self) -> D {
        (self.f)(self.transition.color())
    }

    fn expression(&self) -> &E {
        self.transition.expression()
    }
}

impl<'a, D: Color, Ts: FiniteState, F: Fn(Ts::EdgeColor) -> D> HasFiniteStates<'a>
    for MapEdgeColor<Ts, F>
{
    type StateIndicesIter = FiniteStatesIterType<'a, Ts>;
}

impl<D: Color, Ts: FiniteState, F: Fn(Ts::EdgeColor) -> D> FiniteState for MapEdgeColor<Ts, F> {
    fn state_indices(&self) -> FiniteStatesIterType<'_, Self> {
        self.ts.state_indices()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappedEdgesFromIter<'a, I, F, C> {
    it: I,
    f: &'a F,
    _old_color: PhantomData<C>,
}

impl<'a, I, F, C> Iterator for MappedEdgesFromIter<'a, I, F, C>
where
    I: Iterator,
{
    type Item = MappedTransition<I::Item, &'a F, C>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|t| MappedTransition::new(t, self.f))
    }
}

impl<'a, I, F, C> MappedEdgesFromIter<'a, I, F, C> {
    pub fn new(it: I, f: &'a F) -> Self {
        Self {
            it,
            f,
            _old_color: PhantomData,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappedPreTransition<T, F, C> {
    pre_transition: T,
    f: F,
    _old_color: PhantomData<C>,
}

// impl<Idx: IndexType, E, C: Color, D: Color, F: Fn(C) -> D, T: IsTransition<E, Idx, C>>
// IsTransition<E, Idx, D> for MappedTransition<T, F, C>
impl<Idx: IndexType, E, C: Color, D: Color, F: Fn(C) -> D, T: IsPreTransition<Idx, E, C>>
    IsPreTransition<Idx, E, D> for MappedPreTransition<T, F, C>
{
    fn source(&self) -> Idx {
        self.pre_transition.source()
    }

    fn color(&self) -> D {
        (self.f)(self.pre_transition.color())
    }

    fn expression(&self) -> &E {
        self.pre_transition.expression()
    }
}

impl<T, F, C> MappedPreTransition<T, F, C> {
    pub fn new(pre_transition: T, f: F) -> Self {
        Self {
            pre_transition,
            f,
            _old_color: PhantomData,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappedEdgesToIter<'a, I, F, C> {
    it: I,
    f: &'a F,
    _old_color: PhantomData<C>,
}

impl<'a, I, F, C> Iterator for MappedEdgesToIter<'a, I, F, C>
where
    I: Iterator,
{
    type Item = MappedPreTransition<I::Item, &'a F, C>;
    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|t| MappedPreTransition::new(t, self.f))
    }
}

impl<'a, I, F, C> MappedEdgesToIter<'a, I, F, C> {
    pub fn new(it: I, f: &'a F) -> Self {
        Self {
            it,
            f,
            _old_color: PhantomData,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapStateColor<Ts, F> {
    ts: Ts,
    f: F,
}

impl<Ts, F> MapStateColor<Ts, F> {
    pub fn new(ts: Ts, f: F) -> Self {
        Self { ts, f }
    }

    pub fn ts(&self) -> &Ts {
        &self.ts
    }

    pub fn f(&self) -> &F {
        &self.f
    }
}

impl<'a, D: Color, Ts: FiniteState, F: Fn(Ts::StateColor) -> D> HasFiniteStates<'a>
    for MapStateColor<Ts, F>
{
    type StateIndicesIter = FiniteStatesIterType<'a, Ts>;
}

impl<D: Color, Ts: FiniteState, F: Fn(Ts::StateColor) -> D> FiniteState for MapStateColor<Ts, F> {
    fn state_indices(&self) -> FiniteStatesIterType<'_, Self> {
        self.ts.state_indices()
    }
}

impl<D: Color, Ts: TransitionSystem + Pointed, F: Fn(Ts::StateColor) -> D> Pointed
    for MapStateColor<Ts, F>
{
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

impl<Ts: TransitionSystem, F> HasAlphabet for MapStateColor<Ts, F> {
    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}
