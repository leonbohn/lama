pub mod transition_system;
use std::{collections::BTreeMap, fmt::Display, hash::Hash, ops::Deref};

use impl_tools::autoimpl;
use itertools::{Itertools, Position};
pub use transition_system::TransitionSystem;

mod transition;
use tabled::builder::Builder;
pub use transition::{Edge, EdgeIndex, EdgeIndicesFrom, EdgesFrom, Transition};

pub mod operations;

use crate::{
    alphabet::{Alphabet, HasAlphabet},
    Class, Color, Map, RightCongruence, Set,
};

mod index_ts;
pub use index_ts::BTS;

pub mod path;
pub use path::Path;

mod sproutable;
pub use sproutable::Sproutable;

mod induces;
pub use induces::{finite, infinite, CanInduce, Induced};

pub mod reachable;
pub mod sccs;

pub mod run;

pub mod predecessors;

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

pub type StateColor<X> = <X as TransitionSystem>::StateColor;
pub type EdgeColor<X> = <X as TransitionSystem>::EdgeColor;

/// Abstracts possessing a set of states. Note, that implementors of this trait must
/// be able to iterate over the set of states.
#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait HasStates: TransitionSystem + Sized {
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

mod sealed {
    use crate::TransitionSystem;

    pub type FiniteStatesIterType<'a, This> = <This as HasFiniteStates<'a>>::StateIndicesIter;

    pub trait HasFiniteStates<'a, Outlives = &'a Self>: TransitionSystem {
        type StateIndicesIter: Iterator<Item = Self::StateIndex> + Clone;
    }

    impl<'a, 'b, HFS: HasFiniteStates<'a>> HasFiniteStates<'a> for &'b HFS {
        type StateIndicesIter = <HFS as HasFiniteStates<'a>>::StateIndicesIter;
    }
}
pub(crate) use sealed::*;

// #[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait FiniteState: Sized + for<'a> sealed::HasFiniteStates<'a> {
    fn state_indices(&self) -> sealed::FiniteStatesIterType<'_, Self>;

    fn size(&self) -> usize {
        self.state_indices().count()
    }

    fn contains_state_index(&self, index: Self::StateIndex) -> bool {
        self.state_indices().contains(&index)
    }

    fn find_by_color(&self, color: &StateColor<Self>) -> Option<Self::StateIndex> {
        self.state_indices()
            .find(|index| &self.state_color(*index) == color)
    }

    fn contains_state_color(&self, color: &StateColor<Self>) -> bool {
        self.find_by_color(color).is_some()
    }
}

impl<'a, FS: FiniteState> FiniteState for &'a FS {
    fn state_indices(&self) -> sealed::FiniteStatesIterType<'_, Self> {
        FS::state_indices(self)
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

/// Implementors of this trait have a distinguished (initial) state.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait Pointed: TransitionSystem {
    /// Returns the index of the initial state.
    fn initial(&self) -> Self::StateIndex;
}

pub mod dot;
pub use dot::ToDot;

use self::transition_system::IsTransition;

/// A congruence is a [`TransitionSystem`], which additionally has a distinguished initial state. On top
/// of that, a congruence does not have any coloring on either states or symbols. This
/// functionality is abstracted in [`Pointed`]. This trait is automatically implemented.
pub trait Congruence: TransitionSystem + Pointed {
    fn build_right_congruence(
        &self,
    ) -> (
        RightCongruence<Self::Alphabet>,
        Map<Self::StateIndex, usize>,
    )
    where
        Self: FiniteState,
    {
        let mut cong = RightCongruence::new_for_alphabet(self.alphabet().clone());
        let mut map = Map::default();

        for state in self.state_indices() {
            if self.initial() == state {
                map.insert(state, cong.initial());
                continue;
            }
            map.insert(state, cong.add_state(Class::epsilon()));
        }

        for state in self.state_indices() {
            if let Some(mut it) = self.edges_from(state) {
                for edge in it {
                    let target = edge.target();
                    let target_class = map.get(&target).unwrap();
                    let color = edge.color().clone();
                    let target_class = cong.add_edge(
                        *map.get(&state).unwrap(),
                        edge.expression().clone(),
                        *target_class,
                        (),
                    );
                }
            }
        }

        cong.recompute_labels();

        (cong, map)
    }
}
impl<Sim: TransitionSystem + Pointed> Congruence for Sim {}
