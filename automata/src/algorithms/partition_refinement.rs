use std::hash::Hash;

use crate::{prelude::*, ts::transition_system::IsTransition, Alphabet, Map, Partition, Set};

pub fn partition_refinement<D: DFALike>(dfa: D) -> Partition<D::StateIndex> {
    let accepting = dfa.accepting_states().collect::<Set<_>>();
    let rejecting = dfa.rejecting_states().collect::<Set<_>>();
    let mut p: Vec<_> = [rejecting, accepting]
        .into_iter()
        .filter(|o| !o.is_empty())
        .collect();
    let mut w = p.clone();

    while let Some(a) = w.pop() {
        for sym in dfa.alphabet().universe() {
            let x = dfa
                .state_indices()
                .filter(|q| {
                    dfa.transition(*q, *sym)
                        .map(|t| a.contains(&t.target()))
                        .unwrap_or(false)
                })
                .collect::<Set<_>>();

            let mut new_p = vec![];
            for y in &p {
                if x.intersection(y).next().is_none() || y.difference(&x).next().is_none() {
                    new_p.push(y.clone());
                    continue;
                }
                let int = x.intersection(y).cloned().collect::<Set<_>>();
                let diff = y.difference(&x).cloned().collect::<Set<_>>();

                if let Some(pos) = w.iter().position(|o| o == y) {
                    w.remove(pos);
                    w.extend([int.clone(), diff.clone()]);
                } else {
                    w.push(if int.len() <= diff.len() {
                        int.clone()
                    } else {
                        diff.clone()
                    });
                }

                new_p.extend([int, diff]);
            }
            p = new_p;
        }
    }
    Partition(p)
}

#[cfg(test)]
mod tests {
    use crate::{alphabet::Fixed, prelude::*, tests::wiki_dfa, Partition};

    use super::partition_refinement;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn partition_refinement_wiki() {
        let dfa = wiki_dfa();

        let p = partition_refinement(&dfa);
        assert_eq!(p, Partition::new([vec![0, 1], vec![5], vec![2, 3, 4]]))
    }
}
