use std::marker::PhantomData;

use crate::{prelude::*, ts::nts::NTSPartsFor};

/// A transition system that maps the edge colors of a given transition system to a new type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapEdges<Ts, F> {
    ts: Ts,
    f: F,
}

/// Counterpart to [`MappedTransition`] but for predecessors, i.e. for pre-transitions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappedPreEdge<Idx, T, F, C> {
    pre_transition: T,
    f: F,
    target: Idx,
    _old_color: PhantomData<C>,
}

impl<'a, Idx, E: 'a, C, D, F, T> IsEdge<'a, E, Idx, D> for MappedPreEdge<Idx, T, F, C>
where
    Idx: IndexType,
    C: Clone,
    D: Clone,
    F: Fn(Idx, &E, C, Idx) -> D,
    T: IsEdge<'a, E, Idx, C>,
{
    fn source(&self) -> Idx {
        self.pre_transition.source()
    }

    fn target(&self) -> Idx {
        self.target
    }

    fn color(&self) -> D {
        (self.f)(
            self.pre_transition.source(),
            self.pre_transition.expression(),
            self.pre_transition.color(),
            self.target,
        )
    }

    fn expression(&self) -> &'a E {
        self.pre_transition.expression()
    }
}

/// Iterator over the pre-edges of a transition system whose colors are mapped by some function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappedEdgesToIter<'a, Idx, I, F, C> {
    it: I,
    target: Idx,
    f: &'a F,
    _old_color: PhantomData<C>,
}

impl<'a, Idx, I, F, C> Iterator for MappedEdgesToIter<'a, Idx, I, F, C>
where
    I: Iterator,
    Idx: IndexType,
{
    type Item = MappedPreEdge<Idx, I::Item, &'a F, C>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|t| MappedPreEdge {
            pre_transition: t,
            target: self.target,
            f: self.f,
            _old_color: PhantomData,
        })
    }
}

impl<Ts, D, F> PredecessorIterable for MapEdges<Ts, F>
where
    D: Clone,
    Ts: PredecessorIterable,
    F: Fn(Ts::StateIndex, &ExpressionOf<Ts>, Ts::EdgeColor, Ts::StateIndex) -> D,
{
    type PreEdgeRef<'this> = MappedPreEdge<Ts::StateIndex, Ts::PreEdgeRef<'this>, &'this F, Ts::EdgeColor>
    where
        Self: 'this;

    type EdgesToIter<'this>  = MappedEdgesToIter<'this, Ts::StateIndex, Ts::EdgesToIter<'this>, F, Ts::EdgeColor>
    where
        Self: 'this;

    fn predecessors<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesToIter<'_>> {
        todo!()
    }
}

/// Iterator over the successors of a transition system whose colors are mapped by some function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapEdgesSuccessorsIter<'a, Idx, I, F, C> {
    it: I,
    source: Idx,
    f: &'a F,
    _old_color: PhantomData<C>,
}

impl<'a, Idx: IndexType, I: Iterator, F, C> Iterator for MapEdgesSuccessorsIter<'a, Idx, I, F, C> {
    type Item = MappedEdge<Idx, I::Item, &'a F, C>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|t| MappedEdge {
            transition: t,
            from: self.source,
            f: self.f,
            _old_color: PhantomData,
        })
    }
}

/// Represents a transition whose color is mapped by some function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappedEdge<Idx, T, F, C> {
    transition: T,
    from: Idx,
    f: F,
    _old_color: PhantomData<C>,
}

impl<Idx, T, F, C> MappedEdge<Idx, T, F, C> {
    /// Create a new mapped edge instance.
    pub fn new(transition: T, from: Idx, f: F) -> Self {
        Self {
            transition,
            from,
            f,
            _old_color: PhantomData,
        }
    }
}

impl<'ts, Idx, E: 'ts, C, D, F, T> IsEdge<'ts, E, Idx, D> for MappedEdge<Idx, T, F, C>
where
    Idx: IndexType,
    C: Clone,
    D: Clone,
    F: Fn(Idx, &E, C, Idx) -> D,
    T: IsEdge<'ts, E, Idx, C>,
{
    fn target(&self) -> Idx {
        self.transition.target()
    }

    fn source(&self) -> Idx {
        self.from
    }

    fn color(&self) -> D {
        (self.f)(
            self.from,
            self.transition.expression(),
            self.transition.color(),
            self.transition.target(),
        )
    }

    fn expression(&self) -> &'ts E {
        self.transition.expression()
    }
}

impl<Ts, D, F> TransitionSystem for MapEdges<Ts, F>
where
    Ts: TransitionSystem,
    D: Clone,
    F: Fn(Ts::StateIndex, &ExpressionOf<Ts>, Ts::EdgeColor, Ts::StateIndex) -> D,
{
    type StateIndex = Ts::StateIndex;

    type StateColor = Ts::StateColor;

    type EdgeColor = D;

    type EdgeRef<'this> = MappedEdge<Ts::StateIndex, Ts::EdgeRef<'this>, &'this F, Ts::EdgeColor>
    where
        Self: 'this;

    type EdgesFromIter<'this> = MapEdgesSuccessorsIter<'this, Ts::StateIndex, Ts::EdgesFromIter<'this>, F, Ts::EdgeColor>
    where
        Self: 'this;

    type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts().alphabet()
    }

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts().state_indices()
    }

    fn state_color<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::StateColor> {
        let state = state.to_index(self)?;
        self.ts().state_color(state)
    }

    fn edges_from<Idx: crate::ts::transition_system::Indexes<Self>>(
        &self,
        state: Idx,
    ) -> Option<Self::EdgesFromIter<'_>> {
        Some(MapEdgesSuccessorsIter {
            it: self.ts().edges_from(state.to_index(self)?)?,
            source: state.to_index(self)?,
            f: self.f(),
            _old_color: PhantomData,
        })
    }
}

impl<Ts, F> MapEdges<Ts, F> {
    /// Create a new instance of `Self`.
    pub fn new(ts: Ts, f: F) -> Self {
        Self { ts, f }
    }

    /// Returns a reference to the function with which the edge colors are mapped.
    pub fn f(&self) -> &F {
        &self.f
    }

    /// Returns a reference to the underlying transition system.
    pub fn ts(&self) -> &Ts {
        &self.ts
    }
}
impl<D, Ts, F> Pointed for MapEdges<Ts, F>
where
    D: Clone,
    Ts: TransitionSystem + Pointed,
    F: Fn(Ts::StateIndex, &ExpressionOf<Ts>, Ts::EdgeColor, Ts::StateIndex) -> D,
{
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

/// A transition system that maps the edge colors of a given transition system to a new type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapEdgeColor<Ts, F> {
    ts: Ts,
    f: F,
}

#[allow(missing_docs)]
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

    pub fn into_parts(self) -> (Ts, F) {
        (self.ts, self.f)
    }
}

impl<D: Clone, Ts: TransitionSystem + Pointed, F: Fn(Ts::EdgeColor) -> D> Pointed
    for MapEdgeColor<Ts, F>
{
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

/// Represents a transition whose color is mapped by some function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappedTransition<T, F, C> {
    transition: T,
    f: F,
    _old_color: PhantomData<C>,
}

#[allow(missing_docs)]
impl<T, F, C> MappedTransition<T, F, C> {
    pub fn new(transition: T, f: F) -> Self {
        Self {
            transition,
            f,
            _old_color: PhantomData,
        }
    }
}

impl<'ts, Idx, E, C, D, F, T> IsEdge<'ts, E, Idx, D> for MappedTransition<T, F, C>
where
    Idx: IndexType,
    C: Clone,
    D: Clone,
    F: Fn(C) -> D,
    T: IsEdge<'ts, E, Idx, C>,
{
    fn source(&self) -> Idx {
        self.transition.source()
    }

    fn target(&self) -> Idx {
        self.transition.target()
    }

    fn color(&self) -> D {
        (self.f)(self.transition.color())
    }

    fn expression(&self) -> &'ts E {
        self.transition.expression()
    }
}

/// Iterator over the edges of a transition system whose colors are mapped by some function.
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

#[allow(missing_docs)]
impl<'a, I, F, C> MappedEdgesFromIter<'a, I, F, C> {
    pub fn new(it: I, f: &'a F) -> Self {
        Self {
            it,
            f,
            _old_color: PhantomData,
        }
    }
}

/// Counterpart to [`MappedTransition`] but for predecessors, i.e. for pre-transitions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappedPreTransition<T, F, C> {
    pre_transition: T,
    f: F,
    _old_color: PhantomData<C>,
}

// impl<Idx: IndexType, E, C: Clone, D: Clone, F: Fn(C) -> D, T: IsTransition<E, Idx, C>>
// IsTransition<E, Idx, D> for MappedTransition<T, F, C>
impl<'a, Idx: IndexType, E, C: Clone, D: Clone, F: Fn(C) -> D, T: IsEdge<'a, E, Idx, C>>
    IsEdge<'a, E, Idx, D> for MappedPreTransition<T, F, C>
{
    fn source(&self) -> Idx {
        self.pre_transition.source()
    }

    fn target(&self) -> Idx {
        self.pre_transition.target()
    }

    fn color(&self) -> D {
        (self.f)(self.pre_transition.color())
    }

    fn expression(&self) -> &'a E {
        self.pre_transition.expression()
    }
}

#[allow(missing_docs)]
impl<T, F, C> MappedPreTransition<T, F, C> {
    pub fn new(pre_transition: T, f: F) -> Self {
        Self {
            pre_transition,
            f,
            _old_color: PhantomData,
        }
    }
}

/// Iterator over the pre-transitions of a transition system whose colors are mapped by some function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappedTransitionsToIter<'a, I, F, C> {
    it: I,
    f: &'a F,
    _old_color: PhantomData<C>,
}

impl<'a, I, F, C> Iterator for MappedTransitionsToIter<'a, I, F, C>
where
    I: Iterator,
{
    type Item = MappedPreTransition<I::Item, &'a F, C>;
    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|t| MappedPreTransition::new(t, self.f))
    }
}

#[allow(missing_docs)]
impl<'a, I, F, C> MappedTransitionsToIter<'a, I, F, C> {
    pub fn new(it: I, f: &'a F) -> Self {
        Self {
            it,
            f,
            _old_color: PhantomData,
        }
    }
}

/// A transition system that maps the state colors of a given transition system to a new type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapStateColor<Ts, F> {
    ts: Ts,
    f: F,
}

#[allow(missing_docs)]
impl<Ts, F> MapStateColor<Ts, F> {
    pub fn new(ts: Ts, f: F) -> Self {
        Self { ts, f }
    }

    pub fn ts(&self) -> &Ts {
        &self.ts
    }

    pub fn into_parts(self) -> (Ts, F) {
        (self.ts, self.f)
    }

    pub fn f(&self) -> &F {
        &self.f
    }
}

impl<D: Clone, Ts: TransitionSystem + Pointed, F: Fn(Ts::StateColor) -> D> Pointed
    for MapStateColor<Ts, F>
{
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}
