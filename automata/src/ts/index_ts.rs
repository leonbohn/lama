use std::collections::BTreeMap;

use itertools::Itertools;
use tabled::builder::Builder;

use crate::{
    alphabet::{Alphabet, HasAlphabet},
    ts::TransitionSystem,
    Color,
};

use super::{
    BTState, Edge, EdgeColor, EdgeIndex, EdgeIndicesFrom, EdgesFrom, FiniteState, HasColorMut,
    HasMutableStates, HasStates, Index, IndexType, Sproutable, StateColor, StateIndex, Successor,
    Transition,
};
/// An implementation of a transition system with states of type `Q` and colors of type `C`. It stores
/// the states and edges in a vector, which allows for fast access and iteration. The states and edges
/// are indexed by their position in the respective vector.
#[derive(Clone, PartialEq, Eq)]
pub struct BTS<A: Alphabet, Q: Color, C: Color, Idx = usize> {
    alphabet: A,
    states: BTreeMap<Idx, BTState<A, Q, C, Idx>>,
}

impl<A: Alphabet, C: Color, Q: Color, Idx: IndexType> std::fmt::Debug for BTS<A, Q, C, Idx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}",
            self.build_transition_table(|idx, c| format!("{} : {:?}", idx, c))
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
            states: BTreeMap::new(),
        }
    }

    pub(crate) fn from_parts(alphabet: A, states: BTreeMap<Idx, BTState<A, Q, C, Idx>>) -> Self {
        Self { alphabet, states }
    }

    pub(crate) fn into_parts(self) -> (A, BTreeMap<Idx, BTState<A, Q, C, Idx>>) {
        (self.alphabet, self.states)
    }

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

    pub fn states(&self) -> impl Iterator<Item = &BTState<A, Q, C, Idx>> {
        self.states.values()
    }

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
    /// Adds a state with given `color` to the transition system, returning the [Idx] of
    /// the new state.
    fn add_state(&mut self, color: StateColor<Self>) -> usize {
        let id = self.states.len();
        let state = BTState::new(color);
        self.states.insert(id, state);
        id
    }

    /// Adds an edge from `source` to `target` with the given `trigger` and `color`, returning the
    /// [`EdgeIndex`] of the new edge. This method panics if `source` or `target` do not exist in
    /// the graph.
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
            .get_mut(&source)
            .and_then(|o| o.add_edge(on, target, color))
    }

    fn set_state_color(&mut self, index: Self::StateIndex, color: StateColor<Self>) {
        self.state_mut(index).unwrap().set_color(color);
    }

    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        Self {
            alphabet,
            states: BTreeMap::new(),
        }
    }

    fn remove_edge(
        &mut self,
        from: Self::StateIndex,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)> {
        self.states.get_mut(&from).and_then(|o| o.remove_edge(on))
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> Successor for BTS<A, Q, C, Idx> {
    type StateColor = Q;
    type EdgeColor = C;
    type StateIndex = Idx;
    fn successor(
        &self,
        state: Idx,
        symbol: A::Symbol,
    ) -> Option<Transition<Idx, A::Expression, EdgeColor<Self>>> {
        self.states
            .get(&state)
            .and_then(|o| A::search_edge(&o.edges, symbol))
            .map(|(expression, (target, color))| {
                Transition::new(state, expression.clone(), *target, color.clone())
            })
    }

    fn state_color(&self, index: Idx) -> StateColor<Self> {
        self.state(index)
            .map(|s| s.color().clone())
            .expect("cannot be called if state does not exist!")
    }

    fn predecessors(
        &self,
        state: Self::StateIndex,
    ) -> Vec<(
        Self::StateIndex,
        crate::alphabet::ExpressionOf<Self>,
        EdgeColor<Self>,
    )> {
        self.states
            .iter()
            .flat_map(|(id, q)| {
                q.edges().filter_map(|(e, (p, c))| {
                    if *p == state {
                        Some((*id, e.clone(), c.clone()))
                    } else {
                        None
                    }
                })
            })
            .collect()
    }

    fn edges_from(
        &self,
        state: Self::StateIndex,
    ) -> Vec<Edge<crate::alphabet::ExpressionOf<Self>, EdgeColor<Self>, Self::StateIndex>> {
        self.states
            .get(&state)
            .map(|o| {
                o.edges()
                    .map(|(e, (p, c))| Edge::new(state, *p, c.clone(), e.clone()))
                    .collect_vec()
            })
            .unwrap_or_default()
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &crate::alphabet::ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.states
            .get(&state)
            .and_then(|o| o.edges.get(expression).map(|(_, c)| c.clone()))
    }
}

impl<A, Q, Idx, C> FiniteState for BTS<A, Q, C, Idx>
where
    A: Alphabet,
    Q: Color,
    Idx: IndexType,
    C: Color,
{
    fn state_indices(&self) -> Vec<Self::StateIndex> {
        self.states.keys().copied().collect()
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> HasStates for BTS<A, Q, C, Idx> {
    type State<'this> = &'this BTState<A, Q, C, Idx> where Self: 'this;

    type StatesIter<'this> = std::collections::btree_map::Iter<'this, Idx, BTState<A, Q, C, Idx>>
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
        ts::{index_ts::MealyTS, Sproutable, Successor, Transition},
    };

    use super::BTS;

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
        assert!(ts.successor(s0, 'a').is_some());
        assert_eq!(ts.successor(s1, 'a'), Some(Transition::new(s1, 'a', s1, 0)));
        assert_eq!(ts.index_ts_edges_from(s0).unwrap().count(), 2);
    }
}
