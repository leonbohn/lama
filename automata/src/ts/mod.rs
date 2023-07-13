mod successor;
use std::{fmt::Display, ops::Deref};

use impl_tools::autoimpl;
pub use successor::Successor;

mod transition;
pub use transition::{Edge, EdgeIndex, EdgeIndicesFrom, EdgesFrom, Transition};

use crate::{alphabet::HasAlphabet, Color};

mod index_ts;
pub use index_ts::IndexTS;

mod path;
pub use path::Path;

mod induces;
pub use induces::{finite, infinite, CanInduce, Induced};

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

impl<Q: Color> HasColorMut for State<Q> {
    fn set_color(&mut self, color: Q) {
        self.color = color;
    }
}

impl<Q: Color> HasColor for State<Q> {
    type Color = Q;
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

/// Implemented by things/objects that have a coloring on the state level.
#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]

pub trait StateColored {
    /// The color type of the implementor.
    type StateColor: Color;

    /// Returns a reference to the color of the state with the given index.
    fn state_color(&self, index: StateIndex) -> &Self::StateColor;
}

/// Abstracts possessing a set of states. Note, that implementors of this trait must
/// be able to iterate over the set of states.
#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait HasStates: StateColored + Sized {
    /// The type of the states.
    type State<'this>: HasColor<Color = Self::StateColor>
    where
        Self: 'this;

    /// The type of the iterator over the states.
    type StatesIter<'this>: Iterator<Item = (StateIndex, Self::State<'this>)>
    where
        Self: 'this;

    /// Returns a reference to the state with the given index, if it exists and `None` otherwise.
    fn state(&self, index: StateIndex) -> Option<Self::State<'_>>;

    /// Returns an iterator over the states of the implementor.
    fn states_iter(&self) -> Self::StatesIter<'_>;
}

/// Abstracts possessing a set of states, which can be mutated. Note, that implementors of this
/// trait must be able to iterate over the set of states.
#[autoimpl(for<T: trait + ?Sized> &mut T)]

pub trait HasMutableStates: HasStates {
    /// The type of the mutable iterator over the states.
    type StateMut<'this>: HasColorMut<Color = Self::StateColor>
    where
        Self: 'this;

    /// Returns an iterator over mutable references to the states of the implementor.
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

/// One of the main exported traits of this module. A Transition system is a collection of states,
/// between which there exist directed transitions that are annotated with an expression from an
/// alphabet. This trait merely combines the traits [`HasStates`], [`Successor`] and [`HasAlphabet`]
/// and is automatically implemented.
pub trait TransitionSystem: HasStates + Successor + HasAlphabet {}
impl<Ts: HasStates + Successor + HasAlphabet> TransitionSystem for Ts {}

/// A congruence is a [`TransitionSystem`], which additionally has a distinguished initial state. This
/// functionality is abstracted in [`Pointed`]. This trait is automatically implemented.
pub trait Congruence: TransitionSystem + Pointed {}
impl<Sim: TransitionSystem + Pointed> Congruence for Sim {}
