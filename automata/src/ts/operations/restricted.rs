use crate::{
    alphabet::HasAlphabet,
    ts::{
        predecessors::{IsPreTransition, PredecessorIterable},
        transition_system::IsTransition,
        IndexType,
    },
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

impl<Ts, F> HasAlphabet for RestrictByStateIndex<Ts, F>
where
    Ts: TransitionSystem,
{
    type Alphabet = Ts::Alphabet;
    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
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
    type Item = Ts::TransitionRef<'a>;
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
    type Item = Ts::PreTransitionRef<'a>;
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

#[cfg(test)]
mod tests {
    use crate::{alphabet, ts::Sproutable, Acceptor, Pointed, TransitionSystem, DFA};

    #[test]
    fn restrict_ts_by_state_index() {
        let mut dfa = DFA::new(alphabet! {simple 'a', 'b'});
        let q0 = dfa.initial();
        let q1 = dfa.add_state(false);
        let q2 = dfa.add_state(true);

        dfa.add_edge(q0, 'a', q1, ());
        dfa.add_edge(q0, 'b', q0, ());
        dfa.add_edge(q1, 'a', q2, ());
        dfa.add_edge(q1, 'b', q1, ());
        dfa.add_edge(q2, 'a', q0, ());
        dfa.add_edge(q2, 'b', q2, ());
        assert!(dfa.accepts("aa"));

        let restricted = dfa.restrict_state_indices(|idx| idx != q2);
        assert!(!restricted.accepts("aa"));
    }
}
