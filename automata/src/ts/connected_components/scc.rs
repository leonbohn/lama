use std::{cell::OnceCell, collections::BTreeSet, fmt::Debug};

use itertools::Itertools;

use crate::{prelude::*, Map, Set};

/// Represents a strongly connected component of a transition system.
#[derive(Clone)]
pub struct Scc<'a, Ts: TransitionSystem> {
    ts: &'a Ts,
    states: BTreeSet<Ts::StateIndex>,
    edges: OnceCell<Set<(Ts::StateIndex, SymbolOf<Ts>, Ts::EdgeColor, Ts::StateIndex)>>,
    edge_colors: OnceCell<Set<Ts::EdgeColor>>,
    minimal_representative: OnceCell<Option<(Ts::StateIndex, Vec<SymbolOf<Ts>>)>>,
}

impl<'a, Ts: TransitionSystem> IntoIterator for Scc<'a, Ts> {
    type IntoIter = std::collections::btree_set::IntoIter<Ts::StateIndex>;
    type Item = Ts::StateIndex;
    fn into_iter(self) -> Self::IntoIter {
        self.states.into_iter()
    }
}

impl<'a, Ts: TransitionSystem> std::ops::Deref for Scc<'a, Ts> {
    type Target = BTreeSet<Ts::StateIndex>;

    fn deref(&self) -> &Self::Target {
        &self.states
    }
}

impl<'a, Ts: TransitionSystem> PartialEq for Scc<'a, Ts> {
    fn eq(&self, other: &Self) -> bool {
        self.states == other.states
    }
}
impl<'a, Ts: TransitionSystem> Eq for Scc<'a, Ts> {}
impl<'a, Ts: TransitionSystem> PartialOrd for Scc<'a, Ts> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<'a, Ts: TransitionSystem> Ord for Scc<'a, Ts> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.first().cmp(&other.first())
    }
}
impl<'a, Ts: TransitionSystem> std::hash::Hash for Scc<'a, Ts> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.states.hash(state);
    }
}

impl<'a, Ts: TransitionSystem> Scc<'a, Ts> {
    /// Creates a new strongly connected component from a transition system and a vector of state indices.
    pub fn new<I: IntoIterator<Item = Ts::StateIndex>>(ts: &'a Ts, indices: I) -> Self {
        let states: BTreeSet<_> = indices.into_iter().collect();
        assert!(!states.is_empty(), "Cannot have empty SCC!");
        let edges = OnceCell::new();
        let edge_colors = OnceCell::new();
        let minimal_representative = OnceCell::new();
        Self {
            ts,
            edges,
            states,
            minimal_representative,
            edge_colors,
        }
    }

    /// Returns a reference to the underlying transition system.
    pub fn ts(&self) -> &'a Ts {
        self.ts
    }

    /// Returns an iterator over the state colors of the states in the SCC.
    pub fn state_colors(&self) -> impl Iterator<Item = Ts::StateColor> + '_ {
        self.iter().map(|q| {
            self.ts
                .state_color(*q)
                .expect("State is in SCC but not in Ts")
        })
    }

    pub fn interior_transitions(
        &self,
    ) -> &Set<(Ts::StateIndex, SymbolOf<Ts>, Ts::EdgeColor, Ts::StateIndex)> {
        self.edges.get_or_init(|| {
            let mut edges = Set::default();
            for q in &self.states {
                let mut it = self.ts.edges_from(*q).expect("State must exist");
                for edge in it {
                    let p = edge.target();
                    for a in edge.expression().symbols() {
                        if self.states.contains(&p) {
                            edges.insert((*q, a, edge.color().clone(), p));
                        }
                    }
                }
            }
            edges
        })
    }

    /// Computes the minimal word on which a state of this SCC may be reached.
    pub fn minimal_representative(&self) -> &Option<(Ts::StateIndex, Vec<SymbolOf<Ts>>)>
    where
        Ts: Pointed,
    {
        self.minimal_representative.get_or_init(|| {
            self.ts.minimal_representatives().find_map(|(access, q)| {
                if self.states.contains(&q) {
                    Some((q, access))
                } else {
                    None
                }
            })
        })
    }

    /// Returns an iterator yielding the colors of edges whose source and target states are
    /// in the SCC.
    pub fn interior_edge_colors(&self) -> &Set<Ts::EdgeColor> {
        self.edge_colors.get_or_init(|| {
            self.interior_transitions()
                .iter()
                .map(|(_, _, c, _)| c.clone())
                .collect()
        })
    }

    /// Returns a vector of the colors of the states in the SCC.
    pub fn colors(&self) -> Option<Vec<Ts::StateColor>> {
        debug_assert!(!self.is_empty());
        Some(
            self.states
                .iter()
                .filter_map(|q| self.ts.state_color(*q))
                .collect(),
        )
    }

    pub fn maximal_word(&self) -> Option<Vec<SymbolOf<Ts>>> {
        self.maximal_loop_from(*self.states.first()?)
    }

    /// Attempts to compute a maximal word (i.e. a word visiting all states in the scc). If such a
    /// word exists, it is returned, otherwise the function returns `None`.
    /// This ensures that the word ends back in the state that it started from.
    pub fn maximal_loop_from(&self, from: Ts::StateIndex) -> Option<Vec<SymbolOf<Ts>>> {
        assert!(self.contains(&from));
        let ts = self.ts;
        debug_assert!(!self.is_empty());

        let mut should_continue = false;
        let mut queue = Map::default();
        for (p, a, _, q) in self.interior_transitions() {
            queue
                .entry(*p)
                .or_insert_with(|| Set::default())
                .insert((*a, *q));
        }
        if queue.is_empty() {
            return None;
        }
        assert!(queue.contains_key(&from));

        let mut current = from;
        let mut word = Vec::new();

        while !queue.is_empty() {
            if queue.contains_key(&current) {
                if queue.get(&current).unwrap().is_empty() {
                    queue.remove(&current);
                    continue;
                } else {
                    let (symbol, target) = *queue
                        .get(&current)
                        .unwrap()
                        .iter()
                        .find_or_first(|(_, p)| *p == current)
                        .expect("We know this is non-empty");
                    debug_assert!(ts.has_transition(current, symbol, target));

                    queue.get_mut(&current).unwrap().remove(&(symbol, target));
                    word.push(symbol);
                    current = target;
                }
            } else {
                let q = self
                    .ts
                    .edges_from(current)
                    .and_then(|mut x| {
                        x.find_map(|e| {
                            if queue.contains_key(&e.target()) {
                                Some(e.target())
                            } else {
                                None
                            }
                        })
                    })
                    .unwrap_or_else(|| *queue.keys().next().unwrap());
                debug_assert!(queue.contains_key(&q));
                if queue.get(&q).unwrap().is_empty() {
                    queue.remove(&q);
                    continue;
                }

                word.extend(
                    ts.word_from_to(current, q)
                        .expect("Such a word must exist as both states are in the same SCC"),
                );
                current = q;
            }
        }

        if current != from {
            word.extend(
                ts.word_from_to(current, from)
                    .expect("they are in the same scc!"),
            );
        }

        Some(word)
    }

    /// Returns the number of states in the SCC.
    pub fn len(&self) -> usize {
        self.states.len()
    }

    /// Returns `true` if and only if the SCC is empty.
    pub fn is_empty(&self) -> bool {
        if self.len() == 0 {
            panic!("SCCs can never be empty!");
        }
        false
    }

    /// Returns `true` iff the SCC consists of a single state.
    pub fn is_singleton(&self) -> bool {
        self.states.len() == 1
    }

    /// Returns `true` iff the SCC is left on every symbol of the alphabet.
    pub fn is_transient(&self) -> bool {
        self.interior_transitions().is_empty()
    }

    /// Returns `true` iff there is a transition from a state in the SCC to another state in the SCC,
    /// i.e. if there is a way of reading a non-empty word and staying in the SCC.
    pub fn is_nontransient(&self) -> bool {
        !self.interior_transitions().is_empty()
    }
}

impl<'a, Ts: TransitionSystem> Debug for Scc<'a, Ts> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.states.iter().map(|q| q.to_string()).join(", ")
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use pretty_assertions::assert_eq;

    use crate::{
        ts::{Deterministic, NTS},
        Set, TransitionSystem,
    };

    #[test]
    fn interior_transitions() {
        let transitions = [
            (0, 'a', 0, 0),
            (0, 'b', 1, 1),
            (1, 'a', 2, 1),
            (1, 'b', 0, 0),
        ]
        .into_iter()
        .collect::<Set<_>>();
        let ts = NTS::builder()
            .default_color(())
            .with_transitions(&transitions)
            .deterministic()
            .with_initial(0);
        let sccs = ts.sccs();
        let first = sccs.first();
        println!("{:?}", first);
        assert_eq!(&transitions, first.interior_transitions());
        assert_eq!(first.interior_edge_colors(), &Set::from_iter([0, 1, 2]));

        let color_restricted = (&ts).edge_color_restricted(1, 2);
        let sccs = color_restricted.sccs();
        assert_eq!(sccs[0].interior_transitions(), &Set::default());
        assert_eq!(
            sccs[1].interior_transitions(),
            &Set::from_iter([(1, 'a', 2, 1)])
        );
    }
}
