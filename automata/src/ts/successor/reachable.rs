use std::collections::VecDeque;

use crate::{alphabet::SymbolOf, ts::StateColor, Alphabet, Set, Successor};

#[derive(Debug, Clone)]
pub struct MinimalRepresentatives<Ts: Successor> {
    ts: Ts,
    origin: Ts::StateIndex,
    seen: Set<Ts::StateIndex>,
    queue: VecDeque<(Vec<SymbolOf<Ts>>, Ts::StateIndex)>,
}

impl<Ts> MinimalRepresentatives<Ts>
where
    Ts: Successor,
{
    pub fn new(ts: Ts, origin: Ts::StateIndex) -> Self {
        let mut seen = Set::from_iter([origin]);
        let queue = [(vec![], origin)].into_iter().collect();
        Self {
            ts,
            origin,
            seen,
            queue,
        }
    }
}

impl<Ts> Iterator for MinimalRepresentatives<Ts>
where
    Ts: Successor,
{
    type Item = (Vec<SymbolOf<Ts>>, Ts::StateIndex);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((access, q)) = self.queue.pop_front() {
            for &a in self.ts.alphabet().universe() {
                if let Some(p) = self.ts.successor_index(q, a) {
                    if self.seen.insert(p) {
                        let mut new_access = access.clone();
                        new_access.push(a);
                        self.queue.push_back((new_access, p));
                    }
                }
            }
            return Some((access, q));
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct ReachableStates<Ts: Successor>(MinimalRepresentatives<Ts>);

impl<Ts> ReachableStates<Ts>
where
    Ts: Successor,
{
    pub fn new(ts: Ts, origin: Ts::StateIndex) -> Self {
        Self(MinimalRepresentatives::new(ts, origin))
    }
}

impl<Ts> Iterator for ReachableStates<Ts>
where
    Ts: Successor,
{
    type Item = (Ts::StateIndex, StateColor<Ts>);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(_, q)| (q, self.0.ts.state_color(q)))
    }
}

#[derive(Debug, Clone)]
pub struct ReachableStateIndices<Ts: Successor>(MinimalRepresentatives<Ts>);

impl<Ts> Iterator for ReachableStateIndices<Ts>
where
    Ts: Successor,
{
    type Item = Ts::StateIndex;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(_, q)| q)
    }
}

impl<Ts> ReachableStateIndices<Ts>
where
    Ts: Successor,
{
    pub fn new(ts: Ts, origin: Ts::StateIndex) -> Self {
        Self(MinimalRepresentatives::new(ts, origin))
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{alphabet::Simple, ts::Sproutable, Pointed, Successor};

    #[test]
    fn reachable_states() {
        let mut dfa = crate::DFA::new(Simple::from_iter("ab".chars()));
        let q0 = dfa.initial();
        let q1 = dfa.add_state(false);
        let q2 = dfa.add_state(true);
        dfa.add_edge(q0, 'a', q1, ());
        dfa.add_edge(q0, 'b', q0, ());
        dfa.add_edge(q1, 'a', q2, ());
        dfa.add_edge(q1, 'b', q0, ());
        dfa.add_edge(q2, 'a', q2, ());
        dfa.add_edge(q2, 'b', q2, ());

        assert_eq!(
            dfa.minimal_representatives_from(q0).collect::<Vec<_>>(),
            vec![(vec![], q0), (vec!['a'], q1), (vec!['a', 'a'], q2)]
        );
        assert_eq!(
            dfa.reachable_state_indices().collect_vec(),
            vec![q0, q1, q2]
        );
        assert_eq!(
            dfa.reachable_states().collect_vec(),
            vec![(q0, false), (q1, false), (q2, true)]
        );
    }
}
