use std::hash::Hash;

use crate::{
    prelude::{IsDfa, DFA},
    ts::transition_system::IsTransition,
    Alphabet, Map, Set,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Partition<I: Hash + Eq>(Vec<Set<I>>);

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
    use crate::{alphabet::Fixed, prelude::*};

    use super::partition_refinement;

    pub fn wiki_dfa() -> DFA {
        let mut dfa = DFA::new(alphabet!(simple 'a', 'b'));
        let a = dfa.initial();
        dfa.set_initial_color(false);
        let b = dfa.add_state(false);
        let c = dfa.add_state(true);
        let d = dfa.add_state(true);
        let e = dfa.add_state(true);
        let f = dfa.add_state(false);

        dfa.add_edge(a, 'a', b, ());
        dfa.add_edge(a, 'b', c, ());
        dfa.add_edge(b, 'a', a, ());
        dfa.add_edge(b, 'b', d, ());
        dfa.add_edge(c, 'a', e, ());
        dfa.add_edge(c, 'b', f, ());
        dfa.add_edge(d, 'a', e, ());
        dfa.add_edge(d, 'b', f, ());
        dfa.add_edge(e, 'a', e, ());
        dfa.add_edge(e, 'b', f, ());
        dfa.add_edge(f, 'a', f, ());
        dfa.add_edge(f, 'b', f, ());

        dfa
    }

    #[test]
    fn partition_refinement_wiki() {
        let dfa = wiki_dfa();

        let p = partition_refinement(&dfa);
        println!("{:?}", p);
    }
}
