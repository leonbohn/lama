use std::{collections::BTreeSet, fmt::Debug};

use itertools::Itertools;

use crate::{
    ts::{transition_system::IsTransition, FiniteState},
    Color, Set, TransitionSystem,
};

#[derive(Clone)]
pub struct Dag<C = ()> {
    colors: Vec<C>,
    edges: Vec<(usize, usize)>,
}

impl<C> Dag<C> {
    pub fn is_empty(&self) -> bool {
        self.colors.is_empty()
    }

    pub fn find<F: Fn(&C) -> bool>(&self, pred: F) -> Option<usize> {
        self.colors.iter().position(pred)
    }

    pub fn masked_is_empty(&self, mask: &Set<usize>) -> bool {
        debug_assert!(mask.iter().all(|i| i < &self.colors.len()));
        mask.len() == self.colors.len()
    }

    pub fn color(&self, node: usize) -> Option<&C> {
        self.colors.get(node)
    }

    pub fn color_mut(&mut self, node: usize) -> Option<&mut C> {
        self.colors.get_mut(node)
    }

    pub fn immediate(&self, node: usize) -> impl Iterator<Item = usize> + '_ {
        self.edges
            .iter()
            .filter_map(move |(f, j)| if f != j && *f == node { Some(*j) } else { None })
    }

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

    pub fn prune<I: IntoIterator<Item = usize>>(&mut self, nodes: I) {
        nodes.into_iter().for_each(|i| self.remove_node(i))
    }

    pub fn remove_node(&mut self, id: usize) {
        self.colors.remove(id);
        self.edges.retain(|(x, y)| x != &id && y != &id);
    }

    pub fn from_parts<I: IntoIterator<Item = C>>(colors: I, edges: Vec<(usize, usize)>) -> Self {
        Self {
            colors: colors.into_iter().collect(),
            edges,
        }
    }

    pub fn reachable_from(&self, origin: usize) -> ReachableIter<'_, C> {
        ReachableIter::new(self, origin)
    }

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
    pub fn new(ts: &'a Dag<C>, source: usize) -> Self {
        Self {
            dag: ts,
            queue: BTreeSet::from([source]),
            seen: Set::default(),
        }
    }
}
