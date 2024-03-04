use std::{
    collections::{BTreeSet, VecDeque},
    fmt::Debug,
};

use itertools::Itertools;
use tracing::trace;

use crate::prelude::*;

use super::IsEdge;

mod scc;
pub use scc::Scc;

mod tarjan;
pub(crate) use tarjan::{tarjan_scc_iterative, tarjan_scc_recursive};

mod tarjan_dag;
pub use tarjan_dag::TarjanDAG;

/// Represents a decomposition of a transition system into strongly connected components.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SccDecomposition<'a, Ts: TransitionSystem>(&'a Ts, Vec<Scc<'a, Ts>>);

impl<'a, Ts: TransitionSystem> std::ops::Deref for SccDecomposition<'a, Ts> {
    type Target = Vec<Scc<'a, Ts>>;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<'a, Ts: TransitionSystem> SccDecomposition<'a, Ts> {
    /// Creates a new SCC decomposition from a transition system and a vector of SCCs.
    pub fn new(ts: &'a Ts, sccs: Vec<Scc<'a, Ts>>) -> Self {
        Self(ts, sccs)
    }

    /// Gives the first [`Scc`] in the decomposition. This must exist as we only allow
    /// non-empty decompositions.
    pub fn first(&self) -> &Scc<'a, Ts> {
        self.1.first().expect("At least one SCC must exist!")
    }

    /// Attepmts to find the index of a the SCC containing the given `state`. Returns this index if
    /// it exists, otherwise returns `None`.
    pub fn scc_of(&self, state: Ts::StateIndex) -> Option<usize> {
        self.1
            .iter()
            .enumerate()
            .find_map(|(i, scc)| if scc.contains(&state) { Some(i) } else { None })
    }

    /// Tests whether two SCC decompositions are isomorphic. This is done by checking whether each
    /// SCC in one decomposition has a matching SCC in the other decomposition.
    pub fn isomorphic(&self, other: &SccDecomposition<'a, Ts>) -> bool {
        for scc in &self.1 {
            if !other.1.iter().any(|other_scc| scc == other_scc) {
                trace!("found no scc matching {scc:?} in other");
                return false;
            }
        }
        for scc in &other.1 {
            if !self.1.iter().any(|other_scc| scc == other_scc) {
                trace!("found no scc matching {scc:?} in self");
                return false;
            }
        }
        true
    }
}

impl<'a, Ts: TransitionSystem> std::fmt::Debug for SccDecomposition<'a, Ts> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{{{}}}",
            self.1
                .iter()
                .map(|scc| format!("[{}]", scc.iter().map(|q| q.show()).join(", ")))
                .join(", "),
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{
        alphabet,
        alphabet::CharAlphabet,
        ts::{
            connected_components::{Scc, SccDecomposition},
            Sproutable,
        },
        Pointed, RightCongruence, Set, TransitionSystem, Void,
    };

    use super::NTS;

    pub(super) fn ts() -> RightCongruence<CharAlphabet> {
        let mut cong = RightCongruence::new(alphabet!(simple 'a', 'b'));
        let q0 = cong.add_state(vec![]);
        let q1 = cong.add_state(vec!['a']);
        let q2 = cong.add_state(vec!['b']);
        let q3 = cong.add_state(vec!['b', 'b']);
        cong.add_edge(q0, 'a', q1, Void);
        cong.add_edge(q0, 'b', q2, Void);
        cong.add_edge(q1, 'a', q1, Void);
        cong.add_edge(q1, 'b', q1, Void);
        cong.add_edge(q2, 'a', q3, Void);
        cong.add_edge(q2, 'b', q2, Void);
        cong.add_edge(q3, 'a', q3, Void);
        cong.add_edge(q3, 'b', q2, Void);
        cong
    }

    #[test]
    fn tarjan_scc_decomposition() {
        let cong = ts();
        let sccs = cong.sccs();

        let scc1 = Scc::new(&cong, vec![0]);
        let scc2 = Scc::new(&cong, vec![1]);
        let scc3 = Scc::new(&cong, vec![2, 3]);

        assert_eq!(
            sccs,
            SccDecomposition::new(&cong, vec![scc1.clone(), scc2.clone(), scc3.clone()])
        );

        assert_eq!(
            scc2.interior_transitions(),
            &Set::from_iter([(1, 'a', Void, 1), (1, 'b', Void, 1)])
        );
        assert_eq!(
            scc3.interior_transitions(),
            &Set::from_iter([
                (2, 'a', Void, 3),
                (2, 'b', Void, 2),
                (3, 'a', Void, 3),
                (3, 'b', Void, 2)
            ])
        );

        assert_eq!(scc1.maximal_word(), None);
        assert_eq!(scc2.maximal_word(), Some(vec!['a', 'b']));
        assert_eq!(scc3.maximal_word(), Some(vec!['b', 'a', 'a', 'b']))
    }
}
