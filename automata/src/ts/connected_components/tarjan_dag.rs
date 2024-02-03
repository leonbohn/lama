use itertools::Itertools;
use std::hash::Hash;

use crate::{
    ts::{
        dag::{Dag, ReachableIter},
        transition_system::{Indexes, IsEdge},
    },
    TransitionSystem,
};

use super::{EdgeColor, Scc, SccDecomposition};

/// Newtype wrapper for storing indices of SCCs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SccIndex(usize);

/// Represents a hierarchical view on the SCCs of a transition system. It stores a reference ot the
/// original transition system and a [`Dag`] of SCCs. The SCCs are stored in topological order, i.e.,
/// the first SCC is the root of the DAG and all other SCCs are reachable from it.
#[derive(Clone)]
pub struct TarjanDAG<'a, Ts: TransitionSystem> {
    ts: &'a Ts,
    dag: Dag<Scc<'a, Ts>>,
}

impl<'a, Ts: TransitionSystem> TarjanDAG<'a, Ts> {
    /// Returns an iterator over all transient edges in the transition system. An edge is transient
    /// if its source and target are in different SCCs.
    pub fn transient_edges(&self) -> impl Iterator<Item = Ts::EdgeRef<'a>> + '_ {
        self.ts.transitions().filter(move |t| {
            let source = t.source();
            let target = t.target();
            self.scc_index(source) != self.scc_index(target)
        })
    }

    /// Returns an iterator over all transient states in the transition system. A state is transient
    /// if it is in a singleton SCC and there are no self loops. This means that from the state, there
    /// is no way to reach itself.
    pub fn transient_states(&self) -> impl Iterator<Item = Ts::StateIndex> + '_
    where
        EdgeColor<Ts>: Hash + Eq,
    {
        self.dag
            .nodes()
            .filter_map(|scc| {
                if scc.is_transient() {
                    Some(scc.iter().cloned())
                } else {
                    None
                }
            })
            .flatten()
    }

    /// Returns the index of the SCC containing the given state, if it exists.
    pub fn scc_index(&self, state: Ts::StateIndex) -> Option<SccIndex> {
        self.dag.find(|scc| scc.contains(&state)).map(SccIndex)
    }

    /// Returns the SCC containing the given state, if it exists.
    pub fn scc_of(&self, state: Ts::StateIndex) -> Option<&Scc<'a, Ts>> {
        self.scc_index(state).map(|i| &self.dag[i.0])
    }

    /// Returns the SCC with the given index.
    pub fn scc(&self, index: SccIndex) -> &Scc<'a, Ts> {
        &self.dag[index.0]
    }

    /// Returns an iterator over all SCCs in the transition system.
    pub fn iter(&self) -> impl Iterator<Item = &Scc<'a, Ts>> + '_ {
        self.dag.nodes()
    }

    /// Folds the state colors of the SCCs of the transition system into a single value.
    pub fn fold_state_colors<F, D>(&self, init: D, f: F) -> Dag<D>
    where
        D: Clone,
        F: FnMut(D, Ts::StateColor) -> D + Copy,
    {
        self.dag.reduce(|x| x.state_colors().fold(init.clone(), f))
    }

    /// Folds the colors of all edges whose source and target are in the same SCC into
    /// a single value.
    pub fn fold_edge_colors<F, D>(&self, init: D, f: F) -> Dag<D>
    where
        D: Clone,
        F: FnMut(D, &Ts::EdgeColor) -> D + Copy,
        EdgeColor<Ts>: Hash + Eq,
    {
        self.dag
            .reduce(|x| x.interior_edge_colors().iter().fold(init.clone(), f))
    }

    /// Returns an iterator over sccs which are reachable from the given source scc.
    pub fn reachable_from(&self, source: usize) -> ReachableIter<'_, Scc<'a, Ts>> {
        self.dag.reachable_from(source)
    }

    /// Attempts to identify the index of the SCC containing the given state. If the state
    /// is not in any SCC, `None` is returned.
    pub fn get<X: Indexes<Ts>>(&self, state: X) -> Option<usize> {
        let q = state.to_index(self.ts)?;
        self.dag.find(|c| c.contains(&q))
    }

    /// Returns the number of SCCs in the transition system which is equal to the size
    /// of the DAG.
    pub fn size(&self) -> usize {
        self.dag.size()
    }
}

impl<'a, Ts: TransitionSystem> From<SccDecomposition<'a, Ts>> for TarjanDAG<'a, Ts> {
    fn from(value: SccDecomposition<'a, Ts>) -> Self {
        let mut edges = Vec::new();
        for (l, ls) in value.1.iter().enumerate() {
            edges.extend(
                ls.iter()
                    .flat_map(|q| value.0.edges_from(*q).expect("We know this state exists!"))
                    .map(|o| (l, value.scc_of(o.target()).expect("Must be in some SCC")))
                    .unique(),
            );
        }
        Self {
            ts: value.0,
            dag: Dag::from_parts(value.1, edges),
        }
    }
}

impl<'a, Ts: TransitionSystem> std::fmt::Debug for TarjanDAG<'a, Ts> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{ts::connected_components::tests::ts, TransitionSystem};

    #[test]
    fn tarjan_tree() {
        let cong = ts();
        let sccs = cong.sccs();
        let tree = super::TarjanDAG::from(sccs);
        assert_eq!(
            tree.reachable_from(0).sorted().collect::<Vec<_>>(),
            vec![0, 1, 2]
        );
    }
}
