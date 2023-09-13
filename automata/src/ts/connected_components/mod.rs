use std::{
    collections::{BTreeSet, VecDeque},
    fmt::Debug,
};

use itertools::Itertools;

use crate::{
    alphabet::{HasAlphabet, SymbolOf},
    ts::{finite::SeenColors, CanInduce, FiniteState, IndexType},
    Alphabet, Map, Set, TransitionSystem,
};

use super::IsTransition;

mod scc;
pub use scc::Scc;

mod tarjan;
pub(crate) use tarjan::tarjan_scc;

mod tarjan_dag;
pub use tarjan_dag::TarjanDAG;

/// Represents a decomposition of a transition system into strongly connected components.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SccDecomposition<'a, Ts: TransitionSystem + FiniteState>(&'a Ts, Vec<Scc<'a, Ts>>);

impl<'a, Ts: TransitionSystem + FiniteState> std::ops::Deref for SccDecomposition<'a, Ts> {
    type Target = Vec<Scc<'a, Ts>>;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<'a, Ts: TransitionSystem + FiniteState> SccDecomposition<'a, Ts> {
    /// Creates a new SCC decomposition from a transition system and a vector of SCCs.
    pub fn new(ts: &'a Ts, sccs: Vec<Scc<'a, Ts>>) -> Self {
        Self(ts, sccs)
    }

    /// Attepmts to find the index of a the SCC containing the given `state`. Returns this index if
    /// it exists, otherwise returns `None`.
    pub fn scc_of(&self, state: Ts::StateIndex) -> Option<usize> {
        self.1
            .iter()
            .enumerate()
            .find_map(|(i, scc)| if scc.contains(&state) { Some(i) } else { None })
    }
}

impl<'a, Ts: TransitionSystem + FiniteState + Debug> std::fmt::Debug for SccDecomposition<'a, Ts> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "SCC decomposition:  {{{}}} in\n{:?}",
            self.1
                .iter()
                .map(|scc| format!("[{}]", scc.iter().map(|q| format!("{:?}", q)).join(", ")))
                .join(", "),
            self.0
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{
        alphabet::Simple,
        simple,
        ts::{
            connected_components::{Scc, SccDecomposition},
            Sproutable,
        },
        Pointed, RightCongruence, TransitionSystem,
    };

    pub(super) fn ts() -> RightCongruence<Simple> {
        let mut cong = RightCongruence::new(simple!('a', 'b'));
        let q0 = cong.initial();
        let q1 = cong.add_state(vec!['a']);
        let q2 = cong.add_state(vec!['b']);
        let q3 = cong.add_state(vec!['b', 'b']);
        cong.add_edge(q0, 'a', q1, ());
        cong.add_edge(q0, 'b', q2, ());
        cong.add_edge(q1, 'a', q1, ());
        cong.add_edge(q1, 'b', q1, ());
        cong.add_edge(q2, 'a', q3, ());
        cong.add_edge(q2, 'b', q2, ());
        cong.add_edge(q3, 'a', q3, ());
        cong.add_edge(q3, 'b', q2, ());
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

        assert_eq!(scc1.maximal_word(), None);
        assert_eq!(scc2.maximal_word(), Some(vec!['a', 'b']));
        assert_eq!(scc3.maximal_word(), Some(vec!['a', 'a', 'b', 'b']))
    }
}
