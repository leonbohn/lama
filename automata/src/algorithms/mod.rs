use std::fmt::Debug;

use crate::{prelude::*, ts::Quotient};

mod partition_refinement;
pub use partition_refinement::mealy_partition_refinement;
use partition_refinement::partition_refinement;

pub fn minimize_dfa<D: DFALike>(dfa: D) -> Quotient<D> {
    let partition = partition_refinement(&dfa);
    dfa.quotient(partition)
}

#[cfg(test)]
mod tests {
    use crate::{prelude::DFALike, tests::wiki_dfa, TransitionSystem};

    #[test]
    fn dfa_minimization() {
        let dfa = wiki_dfa();
        assert_eq!(dfa.size(), 6);
        let a = dfa.minimize();
        assert_eq!(a.size(), 3);
    }
}
