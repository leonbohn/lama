use std::collections::VecDeque;

use crate::{prelude::Expression, ts::StateColor, Alphabet, Set, TransitionSystem};

use super::{transition_system::IsEdge, Deterministic, SymbolOf};

/// Type alias for a minimal representative of a state which is its length-lexicographically minimal
/// access sequence and its state index.
pub type MinimalRepresentative<Ts> = (Vec<SymbolOf<Ts>>, <Ts as TransitionSystem>::StateIndex);

/// Struct that can return the minimal representatives of a transition system. A minimal representative
/// for a state `q` of some transition system is the length-lexicographically minimal string with which
/// `q` can be reached from a given state.
#[derive(Debug, Clone)]
pub struct MinimalRepresentatives<Ts: TransitionSystem> {
    ts: Ts,
    origin: Ts::StateIndex,
    seen: Set<Ts::StateIndex>,
    queue: VecDeque<MinimalRepresentative<Ts>>,
}

#[allow(missing_docs)]
impl<Ts> MinimalRepresentatives<Ts>
where
    Ts: TransitionSystem,
{
    pub fn new(ts: Ts, origin: Ts::StateIndex) -> Self {
        let seen = Set::from_iter([origin]);
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
    Ts: TransitionSystem,
{
    type Item = MinimalRepresentative<Ts>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((access, q)) = self.queue.pop_front() {
            if let Some(mut it) = self.ts.edges_from(q) {
                for edge in it {
                    let p = edge.target();
                    if self.seen.insert(p) {
                        for sym in edge.expression().symbols() {
                            let mut new_access = access.clone();
                            new_access.push(sym);
                            self.queue.push_back((new_access, p))
                        }
                    }
                }
            }
            return Some((access, q));
        }
        None
    }
}

/// Allows iterating over the reachable states of a transition system.
#[derive(Debug, Clone)]
pub struct ReachableStates<Ts: TransitionSystem>(MinimalRepresentatives<Ts>);

#[allow(missing_docs)]
impl<Ts> ReachableStates<Ts>
where
    Ts: TransitionSystem,
{
    pub fn new(ts: Ts, origin: Ts::StateIndex) -> Self {
        Self(MinimalRepresentatives::new(ts, origin))
    }
}

impl<Ts> Iterator for ReachableStates<Ts>
where
    Ts: TransitionSystem,
{
    type Item = (Ts::StateIndex, StateColor<Ts>);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(_, q)| {
            (
                q,
                self.0.ts.state_color(q).expect(
                    "Something went wrong, every state should have a color but this one does not",
                ),
            )
        })
    }
}

/// Allows iterating over the indices of all reachable states in a [`TransitionSystem`].
#[derive(Debug, Clone)]
pub struct ReachableStateIndices<Ts: TransitionSystem>(MinimalRepresentatives<Ts>);

impl<Ts> Iterator for ReachableStateIndices<Ts>
where
    Ts: TransitionSystem,
{
    type Item = Ts::StateIndex;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(_, q)| q)
    }
}

#[allow(missing_docs)]
impl<Ts> ReachableStateIndices<Ts>
where
    Ts: TransitionSystem,
{
    pub fn new(ts: Ts, origin: Ts::StateIndex) -> Self {
        Self(MinimalRepresentatives::new(ts, origin))
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{alphabet::CharAlphabet, ts::Sproutable, Pointed, TransitionSystem, Void};

    #[test]
    fn reachable_states() {
        let mut dfa = crate::DFA::new_for_alphabet(CharAlphabet::from_iter("ab".chars()));
        let q0 = dfa.add_state(false);
        let q1 = dfa.add_state(false);
        let q2 = dfa.add_state(true);
        dfa.add_edge(q0, 'a', q1, Void);
        dfa.add_edge(q0, 'b', q0, Void);
        dfa.add_edge(q1, 'a', q2, Void);
        dfa.add_edge(q1, 'b', q0, Void);
        dfa.add_edge(q2, 'a', q2, Void);
        dfa.add_edge(q2, 'b', q2, Void);

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
