use itertools::Itertools;

use crate::{
    alphabet::HasUniverse,
    ts::{HasStateIndices, IndexType},
    Map, Successor,
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
        Ts::Alphabet: HasUniverse,
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
        Ts: Successor<StateIndex = Idx> + HasStateIndices,
        Ts::Alphabet: HasUniverse,
        F: FnMut(&[Idx]),
    {
        self.data.clear();

        for q in ts.state_indices() {
            if let (Some(TarjanData { rootindex: None }) | None) = self.data.get(q) {
                self.visit(ts, *q, &mut f);
            }
        }
        debug_assert!(self.stack.is_empty())
    }
}

#[derive(Clone, Debug)]
pub struct Scc<Ts: Successor>(Vec<Ts::StateIndex>);

impl<Ts: Successor> PartialEq for Scc<Ts> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<Ts: Successor> Eq for Scc<Ts> {}
impl<Ts: Successor> PartialOrd for Scc<Ts> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<Ts: Successor> Ord for Scc<Ts> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0[0].cmp(&other.0[0])
    }
}
impl<Ts: Successor> std::hash::Hash for Scc<Ts> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<Ts: Successor> Scc<Ts> {
    pub fn new(indices: Vec<Ts::StateIndex>) -> Self {
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
        Self(indices)
    }
}

pub fn tarjan_scc<Ts>(ts: &Ts) -> Vec<Scc<Ts>>
where
    Ts: Successor + HasStateIndices,
    Ts::Alphabet: HasUniverse,
{
    let mut sccs = Vec::new();
    {
        let mut tj = Tarjan::new();
        tj.execute(ts, |scc| sccs.push(Scc::new(scc.to_vec())));
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
        assert_eq!(
            sccs,
            vec![Scc::new(vec![0]), Scc::new(vec![1]), Scc::new(vec![2, 3])]
        )
    }
}
