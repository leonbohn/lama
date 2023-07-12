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
#[derive(Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct State<Q> {
    color: Q,
    first_edge: Option<EdgeIndex>,
}

impl<Q: Color> HasColorMut<Q> for State<Q> {
    fn set_color(&mut self, color: Q) {
        self.color = color;
    }
}

impl<Q: Color> HasColor<Q> for State<Q> {
    fn color(&self) -> &Q {
        &self.color
    }
}

impl<Q> State<Q> {
    /// Creates a new state with the given color.
    pub fn new(color: Q) -> Self {
        Self {
            color,
            first_edge: None,
        }
    }

    /// Obtains a reference to the color of the state.
    pub fn color(&self) -> &Q {
        &self.color
    }

    /// Sets the first outgoing edge of the state to the given index.
    pub fn set_first_edge(&mut self, index: EdgeIndex) {
        self.first_edge = Some(index);
    }

    /// Obtains the index of the first outgoing edge.
    pub fn first_edge(&self) -> Option<EdgeIndex> {
        self.first_edge
    }
}

impl<Q: Display> Display for State<Q> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.color)
    }
}

#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait HasColor<C> {
    fn color(&self) -> &C;
}

#[autoimpl(for<T: trait + ?Sized> &mut T)]
pub trait HasColorMut<C>: HasColor<C> {
    fn set_color(&mut self, color: C);
}

pub struct StateReference<'a, Q> {
    pub index: StateIndex,
    pub color: &'a Q,
}

impl<'a, Q> StateReference<'a, Q> {
    pub fn new(index: StateIndex, color: &'a Q) -> Self {
        Self { index, color }
    }

    pub fn color(&self) -> &'a Q {
        self.color
    }
}

#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]

pub trait StateColored {
    type StateColor: Color;

    fn state_color(&self, index: StateIndex) -> &Self::StateColor;
}

#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait HasStates: StateColored + Sized {
    type State<'this>: HasColor<Self::StateColor>
    where
        Self: 'this;

    type StatesIter<'this>: Iterator<Item = (StateIndex, Self::State<'this>)>
    where
        Self: 'this;

    fn state(&self, index: StateIndex) -> Option<Self::State<'_>>;

    fn states_iter(&self) -> Self::StatesIter<'_>;
}

#[autoimpl(for<T: trait + ?Sized> &mut T)]

pub trait HasMutableStates: HasStates {
    type StateMut<'this>: HasColorMut<Self::StateColor>
    where
        Self: 'this;
    fn state_mut(&mut self, index: StateIndex) -> Option<Self::StateMut<'_>>;
}

pub trait Sproutable: HasMutableStates + Successor {
    fn add_state(&mut self, color: Self::StateColor) -> StateIndex;
    fn add_edge<X, Y>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: Self::EdgeColor,
    ) -> EdgeIndex
    where
        X: Into<StateIndex>,
        Y: Into<StateIndex>;
}

mod successor;
use std::{fmt::Display, ops::Deref};

use impl_tools::autoimpl;
pub use successor::Successor;

mod transition;
pub use transition::{Edge, EdgeIndex, EdgeIndicesFrom, EdgesFrom, Transition};

use crate::{
    alphabet::{Alphabet, HasAlphabet},
    Color,
};

mod index_ts;
pub use index_ts::{IndexTS, IndexTSStates};

mod path;
pub use path::Path;

mod induces;
pub use induces::{finite, infinite, CanInduce};

/// Implementors of this trait have a distinguished (initial) state.
pub trait Pointed {
    /// Returns the index of the initial state.
    fn initial(&self) -> StateIndex;
}

pub trait TransitionSystem: HasStates + Successor + HasAlphabet {}
impl<Ts: HasStates + Successor + HasAlphabet> TransitionSystem for Ts {}

pub trait Congruence: TransitionSystem + Pointed {}
impl<Sim: TransitionSystem + Pointed> Congruence for Sim {}
