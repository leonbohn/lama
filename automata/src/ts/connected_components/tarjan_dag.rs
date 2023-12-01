use itertools::Itertools;

use crate::{
    ts::{
        dag::{Dag, ReachableIter},
        transition_system::{Indexes, IsTransition},
    },
    TransitionSystem,
};

use super::{Scc, SccDecomposition};

/// Represents a hierarchical view on the SCCs of a transition system.
#[derive(Clone)]
pub struct TarjanDAG<'a, Ts: TransitionSystem> {
    ts: &'a Ts,
    dag: Dag<Scc<'a, Ts>>,
}

impl<'a, Ts: TransitionSystem> TarjanDAG<'a, Ts> {
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
        F: FnMut(D, Ts::EdgeColor) -> D + Copy,
    {
        self.dag.reduce(|x| x.edge_colors().fold(init.clone(), f))
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
}

impl<'a, Ts: TransitionSystem + Clone> From<SccDecomposition<'a, Ts>> for TarjanDAG<'a, Ts> {
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
