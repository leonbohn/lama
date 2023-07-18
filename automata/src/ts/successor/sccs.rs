use std::collections::{BTreeSet, VecDeque};

use itertools::Itertools;

use crate::{
    alphabet::SymbolOf,
    ts::{finite::SeenColors, CanInduce, FiniteState, IndexType},
    Alphabet, Map, Set, Successor,
};

#[derive(Debug, Clone)]
struct TarjanData {
    rootindex: Option<usize>,
}

/// Struct that is used to compute the strongly connected components of a transition system.
/// Many thanks go out to the authors of the petgraph crate <3.
#[derive(Debug, Clone)]
pub struct Tarjan<Idx> {
    index: usize,
    component_count: usize,
    stack: Vec<Idx>,
    data: Map<Idx, TarjanData>,
}

impl<Idx: IndexType> Tarjan<Idx> {
    pub fn new() -> Self {
        Self {
            index: 0,
            component_count: usize::MAX,
            stack: Vec::new(),
            data: Map::new(),
        }
    }

    pub fn visit<Ts, F>(&mut self, ts: &Ts, v: Idx, f: &mut F)
    where
        Ts: Successor<StateIndex = Idx>,
        F: FnMut(&[Idx]),
    {
        let mut node_v_is_root = true;
        let node_v_index = self.index;
        self.data.insert(
            v,
            TarjanData {
                rootindex: Some(node_v_index),
            },
        );
        self.index += 1;

        for &sym in ts.alphabet().universe() {
            if let Some(w) = ts.successor_index(v, sym) {
                if self.data.get(&w).and_then(|data| data.rootindex).is_none() {
                    self.visit(ts, w, f);
                }
                let w_index = self.data.get(&w).unwrap().rootindex;
                let mut v_mut = self.data.get_mut(&v).unwrap();
                if w_index < v_mut.rootindex {
                    v_mut.rootindex = w_index;
                    node_v_is_root = false;
                }
            }
        }

        if node_v_is_root {
            let mut adjust = 1;
            let c = self.component_count;
            let node_data = &mut self.data;

            let start = self
                .stack
                .iter()
                .rposition(|w| {
                    if node_data.get(&v).unwrap().rootindex > node_data.get(w).unwrap().rootindex {
                        true
                    } else {
                        node_data.get_mut(w).unwrap().rootindex = Some(c);
                        adjust += 1;
                        false
                    }
                })
                .map(|x| x + 1)
                .unwrap_or_default();

            node_data.get_mut(&v).unwrap().rootindex = Some(c);
            self.stack.push(v);
            tracing::trace!(
                "Calling f on {}.. and with adjust {}, queue {}",
                start,
                adjust,
                self.stack.iter().map(|x| x.to_string()).join(", ")
            );
            f(&self.stack[start..]);
            self.stack.truncate(start);
            self.index -= adjust;
            self.component_count -= 1;
        } else {
            self.stack.push(v);
        }
    }

    pub fn execute<Ts, F>(&mut self, ts: &Ts, mut f: F)
    where
        Ts: Successor<StateIndex = Idx> + FiniteState,
        F: FnMut(&[Idx]),
    {
        self.data.clear();

        for q in ts.state_indices() {
            if let (Some(TarjanData { rootindex: None }) | None) = self.data.get(&q) {
                self.visit(ts, q, &mut f);
            }
        }
        debug_assert!(self.stack.is_empty())
    }
}

#[derive(Clone, Debug)]
pub struct Scc<'a, Ts: Successor>(&'a Ts, Vec<Ts::StateIndex>);

impl<'a, Ts: Successor> IntoIterator for Scc<'a, Ts> {
    type IntoIter = std::vec::IntoIter<Ts::StateIndex>;
    type Item = Ts::StateIndex;
    fn into_iter(self) -> Self::IntoIter {
        self.1.into_iter()
    }
}

impl<'a, Ts: Successor> std::ops::Deref for Scc<'a, Ts> {
    type Target = Vec<Ts::StateIndex>;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<'a, Ts: Successor> PartialEq for Scc<'a, Ts> {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl<'a, Ts: Successor> Eq for Scc<'a, Ts> {}
impl<'a, Ts: Successor> PartialOrd for Scc<'a, Ts> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<'a, Ts: Successor> Ord for Scc<'a, Ts> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.1[0].cmp(&other.1[0])
    }
}
impl<'a, Ts: Successor> std::hash::Hash for Scc<'a, Ts> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.1.hash(state);
    }
}

impl<'a, Ts: Successor> Scc<'a, Ts> {
    pub fn new(ts: &'a Ts, indices: Vec<Ts::StateIndex>) -> Self {
        assert!(!indices.is_empty(), "Cannot have empty SCC!");
        let mut indices = indices;
        indices.sort();
        debug_assert!(
            indices
                .iter()
                .collect::<std::collections::HashSet<_>>()
                .len()
                == indices.len()
        );
        Self(ts, indices)
    }

    pub fn ts(&self) -> &'a Ts {
        self.0
    }

    pub fn colors(&self) -> Option<Vec<Ts::Color>> {
        debug_assert!(!self.is_empty());
        let maximal_word = self.maximal_word()?;
        let SeenColors(colors) = self.ts().run(&maximal_word, self.1[0]).ok()?.induce();
        Some(colors)
    }

    pub fn maximal_word(&self) -> Option<Vec<SymbolOf<Ts>>> {
        let ts = self.0;
        debug_assert!(self.len() > 0);

        let mut should_continue = false;
        let mut queue = Map::new();
        for state in self.iter() {
            for &sym in ts.alphabet().universe() {
                if let Some(succ) = ts.successor_index(*state, sym) {
                    if self.contains(&succ) {
                        should_continue = true;
                        queue
                            .entry(*state)
                            .or_insert_with(BTreeSet::new)
                            .insert(sym);
                    }
                }
            }
        }

        // This guards against the case where no transitions are available
        if !should_continue {
            return None;
        }

        let mut current = self[0];
        let mut word = Vec::new();

        while !queue.is_empty() {
            if queue.contains_key(&current) {
                if queue.get(&current).unwrap().is_empty() {
                    queue.remove(&current);
                    continue;
                } else {
                    let sym = *queue.get(&current).unwrap().iter().next().unwrap();
                    queue.get_mut(&current).unwrap().remove(&sym);
                    word.push(sym);
                    current = ts.successor_index(current, sym).unwrap();
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

    pub fn len(&self) -> usize {
        self.1.len()
    }

    pub fn is_singleton(&self) -> bool {
        self.1.len() == 1
    }

    pub fn is_trivial(&self) -> bool {
        self.maximal_word().is_none()
    }
}

pub fn tarjan_scc<Ts>(ts: &Ts) -> Vec<Scc<Ts>>
where
    Ts: Successor + FiniteState,
{
    let mut sccs = Vec::new();
    {
        let mut tj = Tarjan::new();
        tj.execute(ts, |scc| sccs.push(Scc::new(ts, scc.to_vec())));
    }
    debug_assert!(
        sccs.iter().collect::<std::collections::HashSet<_>>().len() == sccs.len(),
        "All SCCs must be unique!"
    );
    sccs.sort();
    sccs
}

#[cfg(test)]
mod tests {
    use crate::{
        simple,
        ts::{successor::sccs::Scc, Sproutable},
        Pointed, RightCongruence,
    };

    #[test]
    fn tarjan_scc_decomposition() {
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

        let sccs = super::tarjan_scc(&cong);

        let scc1 = Scc::new(&cong, vec![0]);
        let scc2 = Scc::new(&cong, vec![1]);
        let scc3 = Scc::new(&cong, vec![2, 3]);

        assert_eq!(sccs, vec![scc1.clone(), scc2.clone(), scc3.clone()]);

        assert_eq!(scc1.maximal_word(), None);
        assert_eq!(scc2.maximal_word(), Some(vec!['a', 'b']));
        assert_eq!(scc3.maximal_word(), Some(vec!['a', 'a', 'b', 'b']))
    }
}
