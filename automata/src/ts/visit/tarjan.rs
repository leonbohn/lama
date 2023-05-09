use itertools::Itertools;
use std::collections::BTreeSet;

use tracing::trace;

use crate::{
    ts::{InputOf, IntoStates, StateOf, StateReference},
    Map, Set, State, Successor,
};

#[derive(Debug, Clone)]
struct TarjanData {
    rootindex: Option<usize>,
}

/// Struct that is used to compute the strongly connected components of a transition system.
/// Many thanks go out to the authors of the petgraph crate <3.
#[derive(Debug, Clone)]
pub struct Tarjan<Q: State> {
    index: usize,
    component_count: usize,
    stack: Vec<Q>,
    data: Map<Q, TarjanData>,
}

impl<Q: State> Tarjan<Q> {
    pub fn new() -> Self {
        Self {
            index: 0,
            component_count: usize::MAX,
            stack: Vec::new(),
            data: Map::new(),
        }
    }

    pub fn visit<TS, F>(&mut self, ts: TS, v: Q, f: &mut F)
    where
        TS: IntoStates<Q = Q>,
        F: FnMut(&[Q]),
    {
        let mut node_v_is_root = true;
        let node_v_index = self.index;
        self.data.insert(
            v.clone(),
            TarjanData {
                rootindex: Some(node_v_index),
            },
        );
        self.index += 1;

        for sym in ts.input_alphabet() {
            if let Some(w) = ts.successor(&v, sym) {
                if self.data.get(&w).and_then(|data| data.rootindex).is_none() {
                    self.visit(ts, w.clone(), f);
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
            trace!(
                "Calling f on {}.. and with adjust {}, queue {}",
                start,
                adjust,
                self.stack.iter().map(|x| format!("{:?}", x)).join(", ")
            );
            f(&self.stack[start..]);
            self.stack.truncate(start);
            self.index -= adjust;
            self.component_count -= 1;
        } else {
            self.stack.push(v);
        }
    }

    pub fn execute<TS, F>(&mut self, ts: TS, mut f: F)
    where
        TS: IntoStates<Q = Q>,
        F: FnMut(&[Q]),
    {
        self.data.clear();

        for q in ts.into_states() {
            if let (Some(TarjanData { rootindex: None }) | None) = self.data.get(&q.state()) {
                self.visit(ts, q.state(), &mut f);
            }
        }
        debug_assert!(self.stack.is_empty())
    }
}

pub fn tarjan_scc<T>(ts: T) -> Vec<Vec<StateOf<T>>>
where
    T: IntoStates,
{
    let mut sccs = Vec::new();
    {
        let mut tj = Tarjan::new();
        tj.execute(ts, |scc| sccs.push(scc.to_vec()));
    }
    sccs
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::{Set, TransitionSystem};

    use super::tarjan_scc;

    #[test]
    #[traced_test]
    fn simple_scc_decomposition() {
        let ts = TransitionSystem::from_iter([
            (0, 0, 0),
            (0, 1, 1),
            (1, 0, 0),
            (1, 1, 2),
            (2, 1, 2),
            (2, 0, 3),
            (3, 0, 4),
            (3, 1, 4),
            (4, 0, 4),
            (4, 1, 3),
        ]);
        let sccs = tarjan_scc(&ts);
        let expected: Vec<Set<_>> = vec![
            [0, 1].into_iter().collect(),
            [2].into_iter().collect(),
            [3, 4].into_iter().collect(),
        ];

        assert_eq!(sccs.len(), expected.len());
        for scc in sccs {
            assert!(expected.contains(&scc.into_iter().collect()));
        }
    }
}
