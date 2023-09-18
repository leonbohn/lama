use std::hash::Hash;

use crate::{
    prelude::{IsDfa, DFA},
    ts::transition_system::IsTransition,
    Alphabet, Map, Partition, Set,
};

pub fn partition_refinement<D: IsDfa>(dfa: D) -> Partition<D::StateIndex> {
    let accepting = dfa.accepting_states().into_iter().collect::<Set<_>>();
    let rejecting = dfa.rejecting_states().into_iter().collect::<Set<_>>();
    let mut p = vec![rejecting, accepting];
    let mut w = p.clone();

    while let Some(a) = w.pop() {
        println!("P = {:?} \t a = {:?}", p, a);
        for sym in dfa.alphabet().universe() {
            let x = dfa
                .state_indices()
                .filter(|q| {
                    dfa.transition(*q, *sym)
                        .map(|t| a.contains(&t.target()))
                        .unwrap_or(false)
                })
                .collect::<Set<_>>();
            println!("sym = {:?} \t x = {:?}", sym, x);

            let mut new_p = vec![];
            for y in &p {
                if x.intersection(y).next().is_none() || y.difference(&x).next().is_none() {
                    new_p.push(y.clone());
                    continue;
                }
                let int = x.intersection(y).cloned().collect::<Set<_>>();
                let diff = y.difference(&x).cloned().collect::<Set<_>>();
                println!("intersection {:?} \t difference {:?}", int, diff);

                if let Some(pos) = w.iter().position(|o| o == y) {
                    println!("remove {:?} from w", y);
                    w.remove(pos);
                    println!("add {:?} and {:?} to w", int, diff);
                    w.extend([int.clone(), diff.clone()]);
                } else {
                    w.push(if int.len() <= diff.len() {
                        println!("add {:?} to w", int);
                        int.clone()
                    } else {
                        println!("add {:?} to w", diff);
                        diff.clone()
                    });
                }

                println!("add {:?} and {:?} to p", int, diff);
                new_p.extend([int, diff]);
            }
            p = new_p;
        }
    }
    Partition(p)
}

#[cfg(test)]
mod tests {
    use crate::{alphabet::Fixed, prelude::*, tests::wiki_dfa};

    use super::partition_refinement;

    #[test]
    fn partition_refinement_wiki() {
        let dfa = wiki_dfa();

        let p = partition_refinement(&dfa);
        println!("{:?}", p);
    }
}
