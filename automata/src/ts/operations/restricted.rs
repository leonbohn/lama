use std::marker::PhantomData;

use crate::{
    prelude::*,
    ts::{predecessors::PredecessorIterable, transition_system::IsEdge, IndexType},
    Pointed, Set, TransitionSystem,
};

/// Abstracts the filtering of a transition system's state indices. This trait is implemented by
/// functions which take a state index and return a boolean value indicating whether the state index
/// should be filtered out or not. It is also implemented by [`Vec`] and [`Set`] which are used to
/// filter out state indices that are not contained in the vector or set.
pub trait StateIndexFilter<Idx: IndexType> {
    /// This method is called to check whether an index should be present in a filtered transition
    /// system or not. Any index for which the function returns `true`, will be present, while all those
    /// for which the function returns `false` are masked out.
    fn is_unmasked(&self, idx: Idx) -> bool;

    /// The counterpart to [`Self::is_unmasked`]. This method is called to check whether an index
    /// should be masked out or not. Any index for which the function returns `true`, will be masked
    /// out, while all those for which the function returns `false` are present.
    fn is_masked(&self, idx: Idx) -> bool {
        !self.is_unmasked(idx)
    }
}

impl<Idx, F> StateIndexFilter<Idx> for F
where
    Idx: IndexType,
    F: Fn(Idx) -> bool,
{
    fn is_unmasked(&self, idx: Idx) -> bool {
        (self)(idx)
    }
}

impl<Idx> StateIndexFilter<Idx> for Vec<Idx>
where
    Idx: IndexType,
{
    fn is_unmasked(&self, idx: Idx) -> bool {
        self.contains(&idx)
    }
}

impl<Idx> StateIndexFilter<Idx> for Set<Idx>
where
    Idx: IndexType,
{
    fn is_unmasked(&self, idx: Idx) -> bool {
        self.contains(&idx)
    }
}

/// Restricts a transition system to a subset of its state indices, which is defined by a filter
/// function.
#[derive(Debug, Clone)]
pub struct RestrictByStateIndex<Ts: TransitionSystem, F> {
    ts: Ts,
    filter: F,
}

/// Iterator over the state indices of a transition system that are restricted by a filter function.
pub struct RestrictByStateIndexIter<'a, Ts: TransitionSystem + 'a, F> {
    filter: &'a F,
    it: Ts::StateIndices<'a>,
}

impl<'a, Ts, F> Iterator for RestrictByStateIndexIter<'a, Ts, F>
where
    Ts: TransitionSystem,
    F: StateIndexFilter<Ts::StateIndex>,
{
    type Item = Ts::StateIndex;
    fn next(&mut self) -> Option<Self::Item> {
        self.it.find(|idx| self.filter.is_unmasked(*idx))
    }
}

impl<'a, Ts: TransitionSystem, F> RestrictByStateIndexIter<'a, Ts, F> {
    /// Creates a new iterator over the state indices of a transition system that are restricted by a
    /// filter function.
    pub fn new(filter: &'a F, it: Ts::StateIndices<'a>) -> Self {
        Self { filter, it }
    }
}

impl<Ts: TransitionSystem + Pointed, F> Pointed for RestrictByStateIndex<Ts, F>
where
    F: StateIndexFilter<Ts::StateIndex>,
{
    fn initial(&self) -> Self::StateIndex {
        let initial = self.ts.initial();
        assert!(
            (self.filter).is_unmasked(initial),
            "initial state is filtered out"
        );
        initial
    }
}

#[allow(missing_docs)]
impl<Ts: TransitionSystem, F> RestrictByStateIndex<Ts, F> {
    pub fn new(ts: Ts, filter: F) -> Self {
        Self { ts, filter }
    }

    pub fn filter(&self) -> &F {
        &self.filter
    }

    pub fn ts(&self) -> &Ts {
        &self.ts
    }
}

/// Iterator over the edges of a transition system that are restricted by a filter function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RestrictedEdgesFromIter<'a, Ts: TransitionSystem + 'a, F> {
    filter: &'a F,
    it: Ts::EdgesFromIter<'a>,
}

#[allow(missing_docs)]
impl<'a, Ts: TransitionSystem + 'a, F> RestrictedEdgesFromIter<'a, Ts, F> {
    pub fn new(it: Ts::EdgesFromIter<'a>, filter: &'a F) -> Self {
        Self { filter, it }
    }
}

impl<'a, Ts: TransitionSystem + 'a, F> Iterator for RestrictedEdgesFromIter<'a, Ts, F>
where
    F: StateIndexFilter<Ts::StateIndex>,
{
    type Item = Ts::EdgeRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.it
            .by_ref()
            .find(|edge| (self.filter).is_unmasked(edge.target()))
    }
}

/// Iterator over the predecessors in a transition system that are restricted by a filter function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RestrictedEdgesToIter<'a, Ts: PredecessorIterable + 'a, F> {
    filter: &'a F,
    it: Ts::EdgesToIter<'a>,
}

impl<'a, Ts: PredecessorIterable + 'a, F> Iterator for RestrictedEdgesToIter<'a, Ts, F>
where
    F: StateIndexFilter<Ts::StateIndex>,
{
    type Item = Ts::PreEdgeRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.it
            .by_ref()
            .find(|edge| (self.filter).is_unmasked(edge.source()))
    }
}

#[allow(missing_docs)]
impl<'a, Ts: PredecessorIterable + 'a, F> RestrictedEdgesToIter<'a, Ts, F> {
    pub fn new(it: Ts::EdgesToIter<'a>, filter: &'a F) -> Self {
        Self { filter, it }
    }
}

/// Takes a transition system and restricts the the possible edge colors. For this, we assume that the colors
/// can be ordered and we are given a minimal and maximal allowed color.
#[derive(Clone, Debug)]
pub struct ColorRestricted<D: TransitionSystem> {
    ts: D,
    min: D::EdgeColor,
    max: D::EdgeColor,
}

impl<D: Congruence> Pointed for ColorRestricted<D>
where
    EdgeColor<D>: Ord,
{
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

impl<D: TransitionSystem> TransitionSystem for ColorRestricted<D>
where
    EdgeColor<D>: Ord,
{
    type StateIndex = D::StateIndex;

    type StateColor = D::StateColor;

    type EdgeColor = D::EdgeColor;

    type EdgeRef<'this> = D::EdgeRef<'this>
    where
        Self: 'this;

    type EdgesFromIter<'this> = ColorRestrictedEdgesFrom<'this, D>    where
        Self: 'this;

    type StateIndices<'this> = D::StateIndices<'this>
    where
        Self: 'this;

    type Alphabet = D::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts().alphabet()
    }
    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts().state_indices()
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        let min = self.min.clone();
        let max = self.max.clone();
        Some(ColorRestrictedEdgesFrom {
            min,
            max,
            _phantom: PhantomData,
            it: self.ts().edges_from(state.to_index(self)?)?,
        })
    }

    fn state_color<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::StateColor> {
        let state = state.to_index(self)?;
        self.ts().state_color(state)
    }
}

impl<D: PredecessorIterable<EdgeColor = usize>> PredecessorIterable for ColorRestricted<D> {
    type PreEdgeRef<'this> = D::PreEdgeRef<'this>
    where
        Self: 'this;

    type EdgesToIter<'this> = ColorRestrictedEdgesTo<'this, D>
    where
        Self: 'this;

    fn predecessors<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesToIter<'_>> {
        todo!()
    }
}

impl<D: DPALike> Deterministic for ColorRestricted<D> {
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        self.ts()
            .transition(state.to_index(self)?, symbol)
            .and_then(|t| {
                if t.color() <= self.max && self.min <= t.color() {
                    Some(t)
                } else {
                    None
                }
            })
    }
}

/// Adapted iterator giving the edges from a state in a transition system that are restricted by a
/// color range. See [`ColorRestricted`] for more information.
pub struct ColorRestrictedEdgesFrom<'a, D: TransitionSystem> {
    _phantom: PhantomData<&'a D>,
    it: D::EdgesFromIter<'a>,
    min: D::EdgeColor,
    max: D::EdgeColor,
}

impl<'a, D: TransitionSystem> Iterator for ColorRestrictedEdgesFrom<'a, D>
where
    EdgeColor<D>: Ord,
{
    type Item = D::EdgeRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it
            .find(|t| t.color() <= self.max && self.min <= t.color())
    }
}

/// Adapted iterator giving the edges to a state in a transition system that are restricted by a
/// color range. See [`ColorRestricted`] for more information.
pub struct ColorRestrictedEdgesTo<'a, D: PredecessorIterable> {
    _phantom: PhantomData<&'a D>,
    it: D::EdgesToIter<'a>,
    min: D::EdgeColor,
    max: D::EdgeColor,
}

impl<'a, D: PredecessorIterable> Iterator for ColorRestrictedEdgesTo<'a, D>
where
    EdgeColor<D>: Ord,
{
    type Item = D::PreEdgeRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it
            .find(|t| t.color() <= self.max && self.min <= t.color())
    }
}

impl<D: TransitionSystem> ColorRestricted<D> {
    /// Returns a reference to the underlying transition system.
    pub fn ts(&self) -> &D {
        &self.ts
    }
    /// Creates a new instance for a given transition system and a color range (as specified by the `min` and `max`
    /// allowed color)
    pub fn new(ts: D, min: D::EdgeColor, max: D::EdgeColor) -> Self {
        Self { ts, min, max }
    }
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    #[test]
    fn restrict_ts_by_state_index() {
        let mut dfa = DFA::new_for_alphabet(alphabet! {simple 'a', 'b'});
        let q0 = dfa.add_state(false);
        let q1 = dfa.add_state(false);
        let q2 = dfa.add_state(true);

        dfa.add_edge(q0, 'a', q1, Void);
        dfa.add_edge(q0, 'b', q0, Void);
        dfa.add_edge(q1, 'a', q2, Void);
        dfa.add_edge(q1, 'b', q1, Void);
        dfa.add_edge(q2, 'a', q0, Void);
        dfa.add_edge(q2, 'b', q2, Void);
        assert!(dfa.accepts("aa"));

        let restricted = dfa.restrict_state_indices(|idx| idx != q2);
        assert!(!restricted.into_dfa().accepts("aa"));
    }
}
