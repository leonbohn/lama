use std::{cell::OnceCell, collections::BTreeSet};

use crate::{
    alphabet::SymbolOf,
    ts::{finite::SeenColors, transition_system::IsTransition, CanInduce},
    Alphabet, Map, Set, TransitionSystem,
};

/// Represents a strongly connected component of a transition system.
#[derive(Debug, Clone)]
pub struct Scc<'a, Ts: TransitionSystem> {
    ts: &'a Ts,
    states: BTreeSet<Ts::StateIndex>,
    edges: OnceCell<Set<(Ts::StateIndex, SymbolOf<Ts>, Ts::EdgeColor, Ts::StateIndex)>>,
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
        Self { ts, edges, states }
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

    fn interior_transitions(
        &self,
    ) -> &Set<(Ts::StateIndex, SymbolOf<Ts>, Ts::EdgeColor, Ts::StateIndex)> {
        self.edges.get_or_init(|| {
            let mut edges = Set::default();
            for q in &self.states {
                let mut it = self.ts.transitions_from(*q);
                for (q, a, c, p) in it {
                    if self.states.contains(&p) {
                        edges.insert((q, a, c, p));
                    }
                }
            }
            edges
        })
    }

    /// Returns an iterator yielding the colors of edges whose source and target states are
    /// in the SCC.
    pub fn edge_colors(&self) -> impl Iterator<Item = Ts::EdgeColor> + '_ {
        self.interior_transitions()
            .iter()
            .map(|(_, _, c, _)| c.clone())
    }

    /// Returns a vector of the colors of the states in the SCC.
    pub fn colors(&self) -> Option<Vec<Ts::StateColor>> {
        debug_assert!(!self.is_empty());
        let maximal_word = self.maximal_word()?;
        todo!()
        // let SeenColors(colors) = self
        //     .ts()
        //     .finite_run_from(*self.first().unwrap(), &maximal_word)
        //     .ok()?
        //     .induce();
        // Some(colors)
    }

    /// Attempts to compute a maximal word (i.e. a word visiting all states in the scc). If such a
    /// word exists, it is returned, otherwise the function returns `None`.
    pub fn maximal_word(&self) -> Option<Vec<SymbolOf<Ts>>> {
        let ts = self.ts;
        debug_assert!(!self.is_empty());

        let mut should_continue = false;
        let mut queue = Map::default();
        for state in self.iter() {
            for (p, a, c, q) in ts.transitions_from(*state) {
                if self.contains(&q) {
                    should_continue = true;
                    queue.entry(p).or_insert_with(BTreeSet::new).insert((a, q));
                }
            }
        }

        // This guards against the case where no transitions are available
        if !should_continue {
            return None;
        }

        let mut current = *self.first()?;
        let mut word = Vec::new();

        while !queue.is_empty() {
            if queue.contains_key(&current) {
                if queue.get(&current).unwrap().is_empty() {
                    queue.remove(&current);
                    continue;
                } else {
                    let (sym, target) = *queue.get(&current).unwrap().iter().next().unwrap();
                    assert!(ts.has_transition(current, sym, target));

                    queue.get_mut(&current).unwrap().remove(&(sym, target));
                    word.push(sym);
                    current = target;
                }
            } else {
                let q = *queue.keys().next().unwrap();
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
        !self.is_nontransient()
    }

    /// Returns `true` iff there is a transition from a state in the SCC to another state in the SCC,
    /// i.e. if there is a way of reading a non-empty word and staying in the SCC.
    pub fn is_nontransient(&self) -> bool {
        self.states.iter().any(|&q| {
            self.ts()
                .transitions_from(q)
                .any(|(_, _, _, q)| self.contains(&q))
        })
    }
}
