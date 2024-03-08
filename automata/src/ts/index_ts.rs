use std::{fmt::Debug, hash::Hash};

use itertools::Itertools;

use crate::{
    alphabet::Alphabet,
    prelude::Initialized,
    ts::{transition_system::IsEdge, Deterministic},
    Color, Map, Set, Show,
};

use super::{
    transition_system::Indexes, EdgeColor, ExpressionOf, IndexType, Sproutable, StateColor,
    SymbolOf, TransitionSystem,
};

/// A state in a transition system. This stores the color of the state and the index of the
/// first edge leaving the state.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct HashTsState<A: Alphabet, Q, C: Hash + Eq, Idx: IndexType> {
    color: Q,
    edges: Map<A::Expression, (Idx, C)>,
    predecessors: Set<(Idx, A::Expression, C)>,
}

impl<A: Alphabet, Q, C: Hash + Eq, Idx: IndexType> HashTsState<A, Q, C, Idx> {
    /// Creates a new state with the given color.
    pub fn new(color: Q) -> Self {
        Self {
            color,
            edges: Map::default(),
            predecessors: Set::default(),
        }
    }

    pub fn set_color(&mut self, color: Q) {
        self.color = color;
    }

    pub fn predecessors(&self) -> &Set<(Idx, A::Expression, C)> {
        &self.predecessors
    }

    pub fn add_pre_edge(&mut self, from: Idx, on: A::Expression, color: C) -> bool {
        self.predecessors.insert((from, on, color))
    }

    pub fn remove_pre_edge(&mut self, from: Idx, on: A::Expression, color: C) -> bool {
        self.predecessors.remove(&(from, on, color))
    }

    pub fn remove_incoming_edges_from(&mut self, target: Idx) {
        self.predecessors.retain(|(idx, _, _)| *idx != target);
    }

    pub fn remove_outgoing_edges_to(&mut self, target: Idx) {
        self.edges.retain(|_, (idx, _)| *idx != target);
    }

    pub fn edges(&self) -> impl Iterator<Item = (&A::Expression, &(Idx, C))> {
        self.edges.iter()
    }

    pub fn edge_map(&self) -> &Map<A::Expression, (Idx, C)> {
        &self.edges
    }

    pub fn add_edge(&mut self, on: A::Expression, to: Idx, color: C) -> Option<(Idx, C)> {
        self.edges.insert(on, (to, color))
    }

    pub fn remove_edge(&mut self, on: &A::Expression) -> Option<(Idx, C)> {
        self.edges.remove(on)
    }

    pub fn recolor<P: Color>(self, color: P) -> HashTsState<A, P, C, Idx> {
        HashTsState {
            color,
            edges: self.edges,
            predecessors: self.predecessors,
        }
    }

    /// Obtains a reference to the color of the state.
    pub fn color(&self) -> &Q {
        &self.color
    }
}

impl<A: Alphabet, Q: std::fmt::Display, C: Color, Idx: IndexType> std::fmt::Display
    for HashTsState<A, Q, C, Idx>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.color)
    }
}

/// An implementation of a transition system with states of type `Q` and colors of type `C`. It stores
/// the states and edges in a vector, which allows for fast access and iteration. The states and edges
/// are indexed by their position in the respective vector.
#[derive(Clone, PartialEq, Eq)]
pub struct HashTs<A: Alphabet, Q = crate::Void, C: Hash + Eq = crate::Void, Idx: IndexType = usize>
{
    pub(crate) alphabet: A,
    pub(crate) states: Map<Idx, HashTsState<A, Q, C, Idx>>,
}

/// Type alias that takes a [`TransitionSystem`] and gives the type of a corresponding [`HashTs`], i.e. one
/// with the same alphabet, edge and state colors.
pub type IntoHashTs<Ts> = HashTs<
    <Ts as TransitionSystem>::Alphabet,
    <Ts as TransitionSystem>::StateColor,
    <Ts as TransitionSystem>::EdgeColor,
>;

/// Type alias for a [`HashTs`] with initial states. Given a [`TransitionSystem`] `Ts`, returns a [`HashTs`] with the
/// same alphabet, edge and state colors, wrapped in a [`Initialized`] type.
pub type IntoInitialHashTs<Ts> = Initialized<
    HashTs<
        <Ts as TransitionSystem>::Alphabet,
        <Ts as TransitionSystem>::StateColor,
        <Ts as TransitionSystem>::EdgeColor,
    >,
>;

impl<A, C, Q, Idx> std::fmt::Debug for HashTs<A, Q, C, Idx>
where
    A: Alphabet,
    C: Debug + Clone + Hash + Eq,
    Q: Debug + Clone + Hash + Eq,
    Idx: IndexType,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}",
            self.build_transition_table(
                |idx, c| format!("{} : {:?}", idx, c),
                |edge| format!("{:?}->{}", edge.color(), edge.target())
            )
        )
    }
}

pub type MealyTS<A, C, Idx = usize> = HashTs<A, (), C, Idx>;
pub type MooreTS<A, C, Idx = usize> = HashTs<A, C, (), Idx>;

impl<A: Alphabet, Idx: IndexType, C: Clone + Hash + Eq, Q: Clone> HashTs<A, Q, C, Idx> {
    /// Creates a new transition system with the given alphabet.
    pub fn new(alphabet: A) -> Self {
        Self {
            alphabet,
            states: Map::default(),
        }
    }

    pub(crate) fn hashts_remove_state(&mut self, idx: Idx) -> Option<Q> {
        let state = self.states.remove(&idx)?;
        self.states.iter_mut().for_each(|(_, s)| {
            s.remove_incoming_edges_from(idx);
            s.remove_outgoing_edges_to(idx)
        });
        Some(state.color)
    }

    pub(crate) fn hashts_remove_edge(&mut self, from: Idx, on: &A::Expression) -> Option<(C, Idx)> {
        let (to, color) = self.states.get_mut(&from)?.remove_edge(on)?;
        self.states
            .get_mut(&to)?
            .remove_pre_edge(from, on.clone(), color.clone());
        Some((color, to))
    }

    pub(crate) fn hashts_remove_transitions(
        &mut self,
        from: Idx,
        symbol: SymbolOf<Self>,
    ) -> Set<(ExpressionOf<Self>, C, Idx)> {
        let mut edges = self
            .states
            .get_mut(&from)
            .map(|s| s.edges.clone())
            .unwrap_or_default();
        edges
            .drain()
            .map(move |(on, (to, color))| {
                self.states
                    .get_mut(&to)
                    .map(|s| s.remove_pre_edge(from, on.clone(), color.clone()));
                (on, color, to)
            })
            .collect()
    }

    /// Creates a `HASHTS` from the given alphabet and states.
    pub(crate) fn from_parts(alphabet: A, states: Map<Idx, HashTsState<A, Q, C, Idx>>) -> Self {
        Self { alphabet, states }
    }

    /// Decomposes the `HASHTS` into its constituent parts.
    #[allow(clippy::type_complexity)]
    pub(crate) fn into_parts(self) -> (A, Map<Idx, HashTsState<A, Q, C, Idx>>) {
        (self.alphabet, self.states)
    }

    /// Creates an empty `HASHTS` ensuring the given capacity.
    pub fn with_capacity(alphabet: A, states: usize) -> Self
    where
        StateColor<Self>: Default,
        Idx: From<usize> + IndexType,
    {
        Self {
            alphabet,
            states: (0..states)
                .map(|i| {
                    (
                        i.into(),
                        HashTsState::new(<StateColor<Self> as Default>::default()),
                    )
                })
                .collect(),
        }
    }

    /// Gets a mutable reference to the alphabet of the transition system.
    pub fn alphabet(&self) -> &A {
        &self.alphabet
    }

    /// Returns a reference to the underlying statemap.
    pub fn raw_state_map(&self) -> &Map<Idx, HashTsState<A, Q, C, Idx>> {
        &self.states
    }

    /// Attempts to find the index of a state with the given `color`. If no such state is
    /// found, `None` is returned. Note, that the function simply returns the first state
    /// with the given color. As the order in which the states are stored is not guaranteed,
    /// subsequent calls may lead to different results, if two states with the same color
    /// exist.
    #[inline(always)]
    pub fn find_by_color(&self, color: &Q) -> Option<Idx>
    where
        Q: Eq,
    {
        self.states.iter().find_map(|(idx, state)| {
            if state.color() == color {
                Some(*idx)
            } else {
                None
            }
        })
    }

    /// Returns an iterator emitting pairs of state indices and their colors.
    pub fn indices_with_color(&self) -> impl Iterator<Item = (Idx, &StateColor<Self>)> {
        self.states.iter().map(|(idx, state)| (*idx, state.color()))
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Clone, C: Clone + Hash + Eq> HashTs<A, Q, C, Idx> {
    /// Returns an iterator over the [`EdgeIndex`]es of the edges leaving the given state.
    pub(crate) fn index_ts_edges_from(
        &self,
        source: Idx,
    ) -> Option<impl Iterator<Item = (&'_ A::Expression, &'_ (Idx, C))> + '_> {
        self.states.get(&source).map(|s| s.edges.iter())
    }

    /// Checks whether the state exists.
    pub fn contains_state<I: Into<Idx>>(&self, index: I) -> bool {
        self.states.contains_key(&index.into())
    }
}

impl<A: Alphabet, Q: Clone, C: Clone + Hash + Eq> Sproutable for HashTs<A, Q, C, usize> {
    type ExtendStateIndexIter = std::ops::Range<Self::StateIndex>;
    fn extend_states<I: IntoIterator<Item = StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter {
        let n = self.states.len();
        let it = (n..).zip(iter.into_iter().map(|c| HashTsState::new(c)));
        self.states.extend(it);
        n..self.states.len()
    }

    /// Adds a state with given `color` to the transition system, returning the index of
    /// the new state.
    fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
        let id = self.states.len();
        let state = HashTsState::new(color.into());
        self.states.insert(id, state);
        id
    }

    /// Adds an edge from `source` to `target` with the given `trigger` and `color`. If an edge
    /// was already present, its target index and color are returned, otherwise, the function gives back
    /// `None`. This method panics if `source` or `target` do not exist in the graph.
    fn add_edge<X, Y, CI>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: CI,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Indexes<Self>,
        Y: Indexes<Self>,
        CI: Into<EdgeColor<Self>>,
    {
        let source = from.to_index(self)?;
        let target = to.to_index(self)?;
        let color = color.into();

        assert!(
            self.contains_state(source) && self.contains_state(target),
            "Source {} or target {} vertex does not exist in the graph.",
            source,
            target
        );
        self.states
            .get_mut(&target)
            .expect("We know this exists")
            .add_pre_edge(source, on.clone(), color.clone());
        self.states
            .get_mut(&source)
            .and_then(|o| o.add_edge(on, target, color))
    }

    fn set_state_color<Idx: Indexes<Self>, X: Into<StateColor<Self>>>(
        &mut self,
        index: Idx,
        color: X,
    ) {
        let Some(index) = index.to_index(self) else {
            tracing::error!("cannot set color of state that does not exist");
            return;
        };
        self.states
            .get_mut(&index)
            .expect("State must exist")
            .set_color(color.into());
    }

    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        Self {
            alphabet,
            states: Map::default(),
        }
    }

    fn remove_edges<X: Indexes<Self>>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool {
        let Some(from) = from.to_index(self) else {
            return false;
        };
        let target = self.states.get_mut(&from).and_then(|o| o.remove_edge(&on));
        if let Some((target, color)) = target {
            let removed = self
                .states
                .get_mut(&target)
                .expect("Something must have gone wrong...")
                .remove_pre_edge(from, on, color);
            debug_assert!(removed);
            true
        } else {
            false
        }
    }
}

trait Increment {
    fn increment(&mut self);
}

impl<X> Increment for X
where
    X: std::ops::AddAssign<u8>,
{
    fn increment(&mut self) {
        *self += 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        alphabet,
        ts::{
            index_ts::MealyTS, transition_system::IsEdge, Deterministic, Sproutable,
            TransitionSystem,
        },
    };

    #[test]
    fn build_ts() {
        let mut ts: MealyTS<_, usize, _> =
            MealyTS::new(alphabet::CharAlphabet::from_iter(['a', 'b']));
        let s0 = ts.add_state(());
        let s1 = ts.add_state(());
        let _e0 = ts.add_edge(s0, 'a', s1, 0usize);
        let _e1 = ts.add_edge(s0, 'b', s0, 1usize);
        let _e2 = ts.add_edge(s1, 'a', s1, 0usize);
        let _e3 = ts.add_edge(s1, 'b', s0, 1usize);
        println!("{:?}", ts);
        assert!(ts.transition(s0, 'a').is_some());
        assert_eq!(ts.transition(s1, 'a').unwrap().target(), s1);
        assert_eq!(ts.index_ts_edges_from(s0).unwrap().count(), 2);
    }
}
