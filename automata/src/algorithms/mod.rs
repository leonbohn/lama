use crate::{prelude::*, ts::Quotient};

mod partition_refinement;
use partition_refinement::partition_refinement;

pub fn minimize_dfa<D: IsDfa>(dfa: D) -> Quotient<D> {
    let partition = partition_refinement(&dfa);
    dfa.quotient(partition)
}

#[cfg(test)]
mod tests {
    use crate::{prelude::IsDfa, tests::wiki_dfa, ts::FiniteState};

    #[test]
    fn dfa_minimization() {
        let dfa = wiki_dfa();
        assert_eq!(dfa.size(), 6);
        let a = dfa.minimize();
        assert_eq!(a.size(), 3);
    }
}
