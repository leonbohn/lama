use itertools::Itertools;

use crate::{
    alphabet::{Alphabet, HasAlphabet},
    Color, Map, Set, Show,
};

use super::{
    EdgeColor, HasColor, HasColorMut, HasMutableStates, HasStates, IndexType, Sproutable,
    StateColor, TransitionSystem,
};

/// A state in a transition system. This stores the color of the state and the index of the
/// first edge leaving the state.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct BTState<A: Alphabet, Q, C: Color, Idx: IndexType> {
    color: Q,
    edges: Map<A::Expression, (Idx, C)>,
    predecessors: Set<(Idx, A::Expression, C)>,
}

impl<A: Alphabet, Q: Color, C: Color, Idx: IndexType> HasColorMut for BTState<A, Q, C, Idx> {
    fn set_color(&mut self, color: Q) {
        self.color = color;
    }
}

impl<A: Alphabet, Q: Color, C: Color, Idx: IndexType> HasColor for BTState<A, Q, C, Idx> {
    type Color = Q;
    fn color(&self) -> &Q {
        &self.color
    }
}

impl<A: Alphabet, Q, C: Color, Idx: IndexType> BTState<A, Q, C, Idx> {
    /// Creates a new state with the given color.
    pub fn new(color: Q) -> Self {
        Self {
            color,
            edges: Map::default(),
            predecessors: Set::default(),
        }
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

    pub fn edges(&self) -> impl Iterator<Item = (&A::Expression, &(Idx, C))> {
        self.edges.iter()
    }

    pub fn edge_map(&self) -> &Map<A::Expression, (Idx, C)> {
        &self.edges
    }

    pub fn add_edge(&mut self, on: A::Expression, to: Idx, color: C) -> Option<(Idx, C)> {
        self.edges.insert(on, (to, color))
    }

    pub fn remove_edge(&mut self, on: A::Expression) -> Option<(Idx, C)> {
        self.edges.remove(&on)
    }

    pub fn recolor<P: Color>(self, color: P) -> BTState<A, P, C, Idx> {
        BTState {
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
    for BTState<A, Q, C, Idx>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.color)
    }
}

/// An implementation of a transition system with states of type `Q` and colors of type `C`. It stores
/// the states and edges in a vector, which allows for fast access and iteration. The states and edges
/// are indexed by their position in the respective vector.
#[derive(Clone, PartialEq, Eq)]
pub struct BTS<A: Alphabet, Q, C: Color, Idx: IndexType = usize> {
    alphabet: A,
    pub(crate) states: Map<Idx, BTState<A, Q, C, Idx>>,
}

impl<A, C, Q, Idx> std::fmt::Debug for BTS<A, Q, C, Idx>
where
    A: Alphabet,
    C: Color + Show,
    Q: Color + Show,
    Idx: IndexType,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}",
            self.build_transition_table(|idx, c| format!("{} : {}", idx, c.show()))
        )
    }
}

pub type MealyTS<A, C, Idx = usize> = BTS<A, (), C, Idx>;
pub type MooreTS<A, C, Idx = usize> = BTS<A, C, (), Idx>;

impl<A: Alphabet, Idx: IndexType, C: Color, Q: Color> BTS<A, Q, C, Idx> {
    /// Creates a new transition system with the given alphabet.
    pub fn new(alphabet: A) -> Self {
        Self {
            alphabet,
            states: Map::default(),
        }
    }

    /// Creates a `BTS` from the given alphabet and states.
    pub(crate) fn from_parts(alphabet: A, states: Map<Idx, BTState<A, Q, C, Idx>>) -> Self {
        Self { alphabet, states }
    }

    /// Decomposes the `BTS` into its constituent parts.
    #[allow(clippy::type_complexity)]
    pub(crate) fn into_parts(self) -> (A, Map<Idx, BTState<A, Q, C, Idx>>) {
        (self.alphabet, self.states)
    }

    /// Creates an empty `BTS` ensuring the given capacity.
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
                        BTState::new(<StateColor<Self> as Default>::default()),
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
    pub fn raw_state_map(&self) -> &Map<Idx, BTState<A, Q, C, Idx>> {
        &self.states
    }

    /// Attempts to find the index of a state with the given `color`. If no such state is
    /// found, `None` is returned. Note, that the function simply returns the first state
    /// with the given color. As the order in which the states are stored is not guaranteed,
    /// subsequent calls may lead to different results, if two states with the same color
    /// exist.
    #[inline(always)]
    pub fn find_by_color(&self, color: &Q) -> Option<Idx> {
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

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> BTS<A, Q, C, Idx> {
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

impl<A: Alphabet, Q: Color, C: Color> Sproutable for BTS<A, Q, C, usize> {
    type ExtendStateIndexIter = std::ops::Range<Self::StateIndex>;
    fn extend_states<I: IntoIterator<Item = StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter {
        let n = self.states.len();
        let it = (n..).zip(iter.into_iter().map(|c| BTState::new(c)));
        self.states.extend(it);
        n..self.states.len()
    }

    /// Adds a state with given `color` to the transition system, returning the index of
    /// the new state.
    fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
        let id = self.states.len();
        let state = BTState::new(color.into());
        self.states.insert(id, state);
        id
    }

    /// Adds an edge from `source` to `target` with the given `trigger` and `color`. If an edge
    /// was already present, its target index and color are returned, otherwise, the function gives back
    /// `None`. This method panics if `source` or `target` do not exist in the graph.
    fn add_edge<X, Y>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: EdgeColor<Self>,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Into<usize>,
        Y: Into<usize>,
    {
        let source = from.into();
        let target = to.into();
        assert!(
            self.contains_state(source) && self.contains_state(target),
            "Source or target vertex does not exist in the graph."
        );
        self.states
            .get_mut(&target)
            .expect("We know this exists")
            .add_pre_edge(source, on.clone(), color.clone());
        self.states
            .get_mut(&source)
            .and_then(|o| o.add_edge(on, target, color))
    }

    fn set_state_color<X: Into<StateColor<Self>>>(&mut self, index: Self::StateIndex, color: X) {
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

    fn remove_edge(
        &mut self,
        from: Self::StateIndex,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool {
        let target = self
            .states
            .get_mut(&from)
            .and_then(|o| o.remove_edge(on.clone()));
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

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> HasStates for BTS<A, Q, C, Idx> {
    type State<'this> = &'this BTState<A, Q, C, Idx> where Self: 'this;

    type StatesIter<'this> = std::collections::hash_map::Iter<'this, Idx, BTState<A, Q, C, Idx>>
    where
        Self: 'this;

    fn states_iter(&self) -> Self::StatesIter<'_> {
        self.states.iter()
    }
    fn state(&self, index: Idx) -> Option<Self::State<'_>> {
        self.states.get(&index)
    }
    fn hs_size(&self) -> usize {
        self.states.len()
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> HasMutableStates for BTS<A, Q, C, Idx> {
    type StateMut<'this> = &'this mut BTState<A, Q, C, Idx>where Self: 'this;

    fn state_mut(&mut self, index: Idx) -> Option<Self::StateMut<'_>> {
        self.states.get_mut(&index)
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> HasAlphabet for BTS<A, Q, C, Idx> {
    type Alphabet = A;
    fn alphabet(&self) -> &Self::Alphabet {
        &self.alphabet
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
        ts::{index_ts::MealyTS, transition_system::IsTransition, Sproutable, TransitionSystem},
    };

    #[test]
    fn build_ts() {
        let mut ts = MealyTS::new(alphabet::Simple::from_iter(['a', 'b']));
        let s0 = ts.add_state(());
        let s1 = ts.add_state(());
        let _e0 = ts.add_edge(s0, 'a', s1, 0);
        let _e1 = ts.add_edge(s0, 'b', s0, 1);
        let _e2 = ts.add_edge(s1, 'a', s1, 0);
        let _e3 = ts.add_edge(s1, 'b', s0, 1);
        println!("{:?}", ts);
        assert!(ts.transition(s0, 'a').is_some());
        assert_eq!(ts.transition(s1, 'a').unwrap().target(), s1);
        assert_eq!(ts.index_ts_edges_from(s0).unwrap().count(), 2);
    }
}
