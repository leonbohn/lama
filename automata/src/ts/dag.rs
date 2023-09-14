use std::{collections::BTreeSet, fmt::Debug};

use itertools::Itertools;

use crate::{
    ts::{transition_system::IsTransition, FiniteState},
    Color, Set, TransitionSystem,
};

/// Represents a directed acyclic graph. The nodes have usize indices and are
/// colored with some type `C`. The edges are represented as a vector of pairs
/// of indices.
#[derive(Clone)]
pub struct Dag<C = ()> {
    colors: Vec<C>,
    edges: Vec<(usize, usize)>,
}

impl<C> Dag<C> {
    /// Returns true iff the graph is empty, i.e. there are no nodes.
    pub fn is_empty(&self) -> bool {
        self.colors.is_empty()
    }

    /// Attempts to find the index of a node satisfying the given predicate.
    pub fn find<F: Fn(&C) -> bool>(&self, pred: F) -> Option<usize> {
        self.colors.iter().position(pred)
    }

    /// Masks the nodes given in the set and returns true iff all nodes are masked,
    /// i.e. no nodes are left unmasked.
    pub fn masked_is_empty(&self, mask: &Set<usize>) -> bool {
        debug_assert!(mask.iter().all(|i| i < &self.colors.len()));
        mask.len() == self.colors.len()
    }

    /// Tries to return the color associated with a node. Returns `None` if no node
    /// with the given index exists.
    pub fn color(&self, node: usize) -> Option<&C> {
        self.colors.get(node)
    }

    /// Does the same as [`Self::color()`], but returning a mutable reference instead.
    pub fn color_mut(&mut self, node: usize) -> Option<&mut C> {
        self.colors.get_mut(node)
    }

    /// Returns an iterator over the _immediate_ successors of a node. An immediate
    /// successor is a node that is directly reachable from the given node.
    pub fn immediate(&self, node: usize) -> impl Iterator<Item = usize> + '_ {
        self.edges
            .iter()
            .filter_map(move |(f, j)| if f != j && *f == node { Some(*j) } else { None })
    }

    /// Returns an iterator over all nodes which are terminal, when ignoring/masking out
    /// the nodes given in the set.
    pub fn masked_terminal_nodes<'a>(
        &'a self,
        mask: &'a Set<usize>,
    ) -> impl Iterator<Item = usize> + 'a {
        (0..self.colors.len()).filter(|i| {
            !mask.contains(i)
                && self
                    .edges
                    .iter()
                    .all(|(x, y)| x != i || x == y || mask.contains(y))
        })
    }

    /// Calls [`Self::remove_node()`] on all indices in the given iterator.
    pub fn prune<I: IntoIterator<Item = usize>>(&mut self, nodes: I) {
        nodes.into_iter().for_each(|i| self.remove_node(i))
    }

    /// Removes the node with index `id` from `self`. This also removes all edges
    /// that are incident to the node. If the node does not exist, this function
    /// does nothing.
    pub fn remove_node(&mut self, id: usize) {
        self.colors.remove(id);
        self.edges.retain(|(x, y)| x != &id && y != &id);
    }

    /// Creates a new [`Dag`] from the given set of colors and edges.
    pub fn from_parts<I: IntoIterator<Item = C>>(colors: I, edges: Vec<(usize, usize)>) -> Self {
        let colors: Vec<_> = colors.into_iter().collect();
        debug_assert_eq!(
            Some(colors.len()),
            edges.iter().map(|(x, y)| std::cmp::max(*x, *y)).max()
        );
        Self { colors, edges }
    }

    /// Returns an iterator yielding the indices of all nodes which are reachable
    /// from the given `origin`.
    pub fn reachable_from(&self, origin: usize) -> ReachableIter<'_, C> {
        ReachableIter::new(self, origin)
    }

    /// Reduces each node's color to a single value by applying the given function.
    pub fn reduce<D, F>(&self, f: F) -> Dag<D>
    where
        for<'a> F: Fn(&'a C) -> D,
    {
        Dag::from_parts(self.colors.iter().map(f), self.edges.clone())
    }
}

impl<C> Default for Dag<C> {
    fn default() -> Self {
        Self {
            colors: Default::default(),
            edges: Default::default(),
        }
    }
}

impl<C: Debug> Debug for Dag<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            self.colors
                .iter()
                .enumerate()
                .map(|(i, x)| format!("{i}:{:?}", x))
                .join(", "),
            self.edges
                .iter()
                .map(|(x, y)| format!("{x} -> {y}"))
                .join(", ")
        )
    }
}

/// This struct is used for iterating over the indices of SCCs that are reachable
/// from some SCC. It uses a worklist and simply iterates through the successors
/// of all indices it contains. As it uses a [`BTreeSet`] as its queue, the SCCs
/// are guaranteed to be iterated through in ascending order.
#[derive(Debug, Clone)]
pub struct ReachableIter<'a, C> {
    dag: &'a Dag<C>,
    queue: BTreeSet<usize>,
    seen: Set<usize>,
}

impl<'a, C> Iterator for ReachableIter<'a, C> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let q = self.queue.pop_first()?;
        debug_assert!(!self.seen.contains(&q));
        self.seen.insert(q);

        let elems = self
            .dag
            .edges
            .iter()
            .filter_map(|(x, y)| {
                if x == &q && !self.queue.contains(y) && !self.seen.contains(y) {
                    Some(*y)
                } else {
                    None
                }
            })
            .collect_vec();
        self.queue.extend(&elems);
        Some(q)
    }
}

impl<'a, C> ReachableIter<'a, C> {
    /// Create a new iterator over the SCCs reachable from the given source SCC.
    pub fn new(ts: &'a Dag<C>, source: usize) -> Self {
        Self {
            dag: ts,
            queue: BTreeSet::from([source]),
            seen: Set::default(),
        }
    }
}
