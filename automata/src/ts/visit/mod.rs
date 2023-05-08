mod paths;
pub use paths::{LLexPaths, Path};

mod bfs;
pub use bfs::Bfs;
mod dfs;
pub use dfs::Dfs;
mod tarjan;
pub use tarjan::Tarjan;

use std::collections::{BTreeSet, VecDeque};

use tracing::trace;

use crate::{Pointed, Set, StateIndex, Successor, Transition, Trigger};

use super::{StateOf, TransitionOf};

/// Trait that encapsulates the ability to visit `Places` of a transition system.
/// A `place` could be a state or a transition, in the future, we might also allow
/// loops and SCCs as places.
/// The main imlementors of this trait will be `LengthLexicographic` and
/// `LengthLexicographicEdges`, which are objects that visit states/transitions of a
/// transition system in length-lexicographic order (which essentially corresponds to
/// visitation in breadth-first order from the initial state).
pub trait Visitor {
    /// The type of the transition system that is visited.
    type Place;

    /// Visits the next place.
    fn visit_next(&mut self) -> Option<Self::Place>;

    /// Returns an iterator over the places visited by this visitor.
    fn iter(self) -> VisitorIter<Self>
    where
        Self: Sized,
    {
        VisitorIter { visitor: self }
    }
}

pub trait Place<TS: Successor> {
    fn from_transition(transition: TransitionOf<TS>) -> Self;
}

/// Stores a visitor and allows iteration over the places visited by it.
#[derive(Debug, Clone)]
pub struct VisitorIter<V: Visitor> {
    pub(crate) visitor: V,
}

impl<V: Visitor> Iterator for VisitorIter<V> {
    type Item = V::Place;

    fn next(&mut self) -> Option<Self::Item> {
        self.visitor.visit_next()
    }
}
