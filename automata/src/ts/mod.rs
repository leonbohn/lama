mod successor;
use std::{collections::BTreeMap, fmt::Display, hash::Hash, ops::Deref};

use impl_tools::autoimpl;
use itertools::{Itertools, Position};
pub use successor::Successor;

mod transition;
use tabled::builder::Builder;
pub use transition::{Edge, EdgeIndex, EdgeIndicesFrom, EdgesFrom, Transition};

pub mod operations;
pub use operations::Product;

use crate::{
    alphabet::{Alphabet, HasAlphabet},
    Class, Color, Map, RightCongruence,
};

mod index_ts;
pub use index_ts::BTS;

pub mod path;
pub use path::Path;

mod induces;
pub use induces::{finite, infinite, CanInduce, Induced};

pub trait IndexType: Copy + std::hash::Hash + std::fmt::Debug + Eq + Ord + Display {}
impl<Idx: Copy + std::hash::Hash + std::fmt::Debug + Eq + Ord + Display> IndexType for Idx {}

/// Type for indices of states and edges.
pub type Idx = usize;

/// Helper trait for index types, which also allows conversion to a state or edge index.
pub trait Index {
    /// Turns self into an index of type [`Idx`].
    fn index(&self) -> Idx;

    /// Turns self into a state index.
    fn as_state_index(&self) -> StateIndex {
        StateIndex::new(self.index())
    }

    /// Turns self into an edge index.
    fn as_edge_index(&self) -> EdgeIndex {
        EdgeIndex::new(self.index())
    }
}

impl Index for Idx {
    fn index(&self) -> Idx {
        *self
    }
}

/// Wrapper type for indices of states in a transition system.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct StateIndex(Idx);

impl StateIndex {
    /// Creates a new state index.
    pub fn new(index: Idx) -> Self {
        Self(index)
    }
}

impl Deref for StateIndex {
    type Target = Idx;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Index for StateIndex {
    fn index(&self) -> Idx {
        self.0
    }
}

impl Display for StateIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.index())
    }
}

/// A state in a transition system. This stores the color of the state and the index of the
/// first edge leaving the state.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct BTState<A: Alphabet, Q, C, Idx> {
    color: Q,
    edges: Map<A::Expression, (Idx, C)>,
}

impl<A: Alphabet, Q: Color, C, Idx> HasColorMut for BTState<A, Q, C, Idx> {
    fn set_color(&mut self, color: Q) {
        self.color = color;
    }
}

impl<A: Alphabet, Q: Color, C, Idx> HasColor for BTState<A, Q, C, Idx> {
    type Color = Q;
    fn color(&self) -> &Q {
        &self.color
    }
}

impl<A: Alphabet, Q, C, Idx> BTState<A, Q, C, Idx> {
    /// Creates a new state with the given color.
    pub fn new(color: Q) -> Self {
        Self {
            color,
            edges: Map::default(),
        }
    }

    pub fn edges(&self) -> impl Iterator<Item = (&A::Expression, &(Idx, C))> {
        self.edges.iter()
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
        }
    }

    /// Obtains a reference to the color of the state.
    pub fn color(&self) -> &Q {
        &self.color
    }
}

impl<A: Alphabet, Q: Display, C, Idx> Display for BTState<A, Q, C, Idx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.color)
    }
}

/// Implementors of this trait have a color, which can be obtained.
#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait HasColor {
    /// The color type of the implementor.
    type Color: Color;
    /// Returns a reference to the color of the implementor.
    fn color(&self) -> &Self::Color;
}

/// Implementors of this trait have a color, which can be obtained and set.
#[autoimpl(for<T: trait + ?Sized> &mut T)]
pub trait HasColorMut: HasColor {
    /// Sets the color of the implementor to the given color.
    fn set_color(&mut self, color: Self::Color);
}

/// A reference to a state in a transition system. This stores the index of the state and a
/// reference to the color of the state.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct StateReference<'a, Q> {
    /// The [`StateIndex`] of the state that is referenced.
    pub index: StateIndex,
    /// A reference to the color of the state.
    pub color: &'a Q,
}

impl<'a, Q: Color> HasColor for StateReference<'a, Q> {
    type Color = Q;
    fn color(&self) -> &Q {
        self.color
    }
}

impl<'a, Q> StateReference<'a, Q> {
    /// Creates a new state reference.
    pub fn new(index: StateIndex, color: &'a Q) -> Self {
        Self { index, color }
    }
}

pub type StateColor<X> = <X as Successor>::StateColor;
pub type EdgeColor<X> = <X as Successor>::EdgeColor;

/// Abstracts possessing a set of states. Note, that implementors of this trait must
/// be able to iterate over the set of states.
#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait HasStates: Successor + Sized {
    /// The type of the states.
    type State<'this>: HasColor<Color = StateColor<Self>>
    where
        Self: 'this;

    /// The type of the iterator over the states.
    type StatesIter<'this>: Iterator<Item = (&'this Self::StateIndex, Self::State<'this>)>
    where
        Self: 'this;

    /// Returns a reference to the state with the given index, if it exists and `None` otherwise.
    fn state(&self, index: Self::StateIndex) -> Option<Self::State<'_>>;

    /// Returns an iterator over the states of the implementor.
    fn states_iter(&self) -> Self::StatesIter<'_>;

    fn hs_size(&self) -> usize {
        self.states_iter().count()
    }
}

#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait FiniteState: Successor + Sized {
    fn state_indices(&self) -> Vec<Self::StateIndex>;

    fn size(&self) -> usize {
        self.state_indices().len()
    }

    fn contains_state_index(&self, index: Self::StateIndex) -> bool {
        self.state_indices().contains(&index)
    }

    fn find_by_color(&self, color: &StateColor<Self>) -> Option<Self::StateIndex> {
        self.state_indices()
            .into_iter()
            .find(|index| &self.state_color(*index) == color)
    }

    fn contains_state_color(&self, color: &StateColor<Self>) -> bool {
        self.find_by_color(color).is_some()
    }
}

/// Abstracts possessing a set of states, which can be mutated. Note, that implementors of this
/// trait must be able to iterate over the set of states.
#[autoimpl(for<T: trait + ?Sized> &mut T)]

pub trait HasMutableStates: HasStates {
    /// The type of the mutable iterator over the states.
    type StateMut<'this>: HasColorMut<Color = StateColor<Self>>
    where
        Self: 'this;

    /// Returns an iterator over mutable references to the states of the implementor.
    fn state_mut(&mut self, index: Self::StateIndex) -> Option<Self::StateMut<'_>>;
}

pub trait Sproutable: Successor {
    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self;

    fn add_state(&mut self, color: StateColor<Self>) -> Self::StateIndex;

    fn set_state_color(&mut self, index: Self::StateIndex, color: StateColor<Self>);

    fn set_initial_color(&mut self, color: StateColor<Self>)
    where
        Self: Pointed,
    {
        self.set_state_color(self.initial(), color);
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
        Y: Into<Self::StateIndex>;

    fn remove_edge(
        &mut self,
        from: Self::StateIndex,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool;

    /// Turns the automaton into a complete one, by adding a sink state and adding transitions
    /// to it from all states that do not have a transition for a given symbol.
    fn complete_with_sink(&mut self, sink_color: Self::StateColor) -> Self::StateIndex
    where
        Self: FiniteState,
    {
        let sink = self.add_state(sink_color.clone());

        let universe = self.alphabet().universe().cloned().collect_vec();

        todo!()
    }
}

/// Implementors of this trait have a distinguished (initial) state.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait Pointed: Successor {
    /// Returns the index of the initial state.
    fn initial(&self) -> Self::StateIndex;
}

/// One of the main exported traits of this module. A Transition system is a collection of states,
/// between which there exist directed transitions that are annotated with an expression from an
/// alphabet. This trait merely combines the traits [`HasStates`], [`Successor`] and [`HasAlphabet`]
/// and is automatically implemented.
pub trait TransitionSystem: FiniteState + Successor {
    fn build_transition_table<SD>(&self, state_decorator: SD) -> String
    where
        SD: Fn(Self::StateIndex, StateColor<Self>) -> String,
    {
        let mut builder = Builder::default();
        builder.set_header(
            std::iter::once("State".to_string())
                .chain(self.alphabet().universe().map(|s| format!("{:?}", s))),
        );
        for id in self.state_indices() {
            let mut row = vec![format!("{}", state_decorator(id, self.state_color(id)))];
            for &sym in self.alphabet().universe() {
                if let Some(edge) = self.successor(id, sym) {
                    row.push(format!("{} : {:?}", edge.target(), edge.color()));
                } else {
                    row.push("-".to_string());
                }
            }
            builder.push_record(row);
        }

        builder
            .build()
            .with(tabled::settings::Style::rounded())
            .to_string()
    }

    fn collect_ts(&self) -> BTS<Self::Alphabet, Self::StateColor, Self::EdgeColor> {
        let mut ts = BTS::new_for_alphabet(self.alphabet().clone());
        let mut map = std::collections::HashMap::new();
        for index in self.state_indices() {
            map.insert(index, ts.add_state(self.state_color(index)));
        }
        for index in self.state_indices() {
            for sym in self.alphabet().universe() {
                if let Some(edge) = self.successor(index, *sym) {
                    ts.add_edge(
                        *map.get(&index).unwrap(),
                        <Self::Alphabet as Alphabet>::expression(*sym),
                        *map.get(&edge.target()).unwrap(),
                        edge.color().clone(),
                    );
                }
            }
        }
        ts
    }

    fn collect_into_ts<
        Ts: TransitionSystem<
                StateColor = Self::StateColor,
                EdgeColor = Self::EdgeColor,
                Alphabet = Self::Alphabet,
            > + Sproutable,
    >(
        &self,
    ) -> Ts {
        let mut ts = Ts::new_for_alphabet(self.alphabet().clone());
        let mut map = std::collections::HashMap::new();
        for index in self.state_indices() {
            map.insert(index, ts.add_state(self.state_color(index)));
        }
        for index in self.state_indices() {
            for sym in self.alphabet().universe() {
                if let Some(edge) = self.successor(index, *sym) {
                    ts.add_edge(
                        *map.get(&index).unwrap(),
                        <Self::Alphabet as Alphabet>::expression(*sym),
                        *map.get(&edge.target()).unwrap(),
                        edge.color().clone(),
                    );
                }
            }
        }
        ts
    }
}

impl<Ts: FiniteState + Successor> TransitionSystem for Ts {}

pub mod dot;
pub use dot::ToDot;

/// A congruence is a [`TransitionSystem`], which additionally has a distinguished initial state. On top
/// of that, a congruence does not have any coloring on either states or symbols. This
/// functionality is abstracted in [`Pointed`]. This trait is automatically implemented.
pub trait Congruence: TransitionSystem + Pointed {
    fn build_right_congruence(
        &self,
    ) -> (
        RightCongruence<Self::Alphabet>,
        Map<Self::StateIndex, usize>,
    ) {
        let mut cong = RightCongruence::new_for_alphabet(self.alphabet().clone());
        let mut map = Map::default();

        let states = self.state_indices();
        for state in &states {
            if self.initial() == *state {
                map.insert(*state, cong.initial());
                continue;
            }
            map.insert(*state, cong.add_state(Class::epsilon()));
        }

        for state in states {
            for edge in self.edges_from(state) {
                let target = edge.target();
                let target_class = map.get(&target).unwrap();
                let color = edge.color().clone();
                let target_class = cong.add_edge(
                    *map.get(&state).unwrap(),
                    edge.trigger().clone(),
                    *target_class,
                    (),
                );
            }
        }

        cong.recompute_labels();

        (cong, map)
    }
}
impl<Sim: TransitionSystem + Pointed> Congruence for Sim {}
