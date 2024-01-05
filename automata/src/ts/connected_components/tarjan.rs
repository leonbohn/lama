use std::collections::{hash_map::RandomState, VecDeque};

use fxhash::FxBuildHasher;
use tracing::trace;

use crate::{
    prelude::Expression,
    ts::{
        connected_components::Scc, predecessors::PredecessorIterable, transition_system::IsEdge,
        IndexType,
    },
    Alphabet, Map, Set, Show, TransitionSystem,
};

use super::SccDecomposition;

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

impl<Idx: IndexType> Default for Tarjan<Idx> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Idx: IndexType> Tarjan<Idx> {
    /// Creates a new Tarjan SCC decomposition instance.
    pub fn new() -> Self {
        Self {
            index: 0,
            component_count: usize::MAX,
            stack: Vec::new(),
            data: Map::default(),
        }
    }

    pub(crate) fn visit<Ts, F>(&mut self, ts: &Ts, v: Idx, f: &mut F)
    where
        Ts: TransitionSystem<StateIndex = Idx>,
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

        if let Some(it) = ts.edges_from(v) {
            for edge in it {
                let q = edge.target();
                for a in edge.expression().symbols() {
                    if self.data.get(&q).and_then(|data| data.rootindex).is_none() {
                        self.visit(ts, q, f);
                    }
                    let w_index = self.data.get(&q).unwrap().rootindex;
                    let v_mut = self.data.get_mut(&v).unwrap();
                    if w_index < v_mut.rootindex {
                        v_mut.rootindex = w_index;
                        node_v_is_root = false;
                    }
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
            f(&self.stack[start..]);
            self.stack.truncate(start);
            self.index -= adjust;
            self.component_count -= 1;
        } else {
            self.stack.push(v);
        }
    }

    pub(crate) fn execute<Ts, F>(&mut self, ts: &Ts, mut f: F)
    where
        Ts: TransitionSystem<StateIndex = Idx>,
        F: FnMut(&[Idx]),
    {
        self.data.clear();

        for q in ts.state_indices() {
            if let Some(TarjanData { rootindex: None }) | None = self.data.get(&q) {
                self.visit(ts, q, &mut f);
            }
        }
        debug_assert!(self.stack.is_empty())
    }
}

pub(crate) fn tarjan_scc_recursive<Ts>(ts: &Ts) -> SccDecomposition<'_, Ts>
where
    Ts: TransitionSystem,
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
    SccDecomposition::new(ts, sccs)
}

pub(crate) fn kosaraju<Ts>(ts: &Ts, start: Ts::StateIndex) -> SccDecomposition<'_, Ts>
where
    Ts: TransitionSystem + PredecessorIterable,
{
    let mut visited = Set::from_iter([start]);
    let mut l = VecDeque::with_capacity(ts.size());
    let mut queue = Vec::with_capacity(ts.size());

    queue.push((start, ts.edges_from(start).unwrap()));

    'outer: while let Some((q, mut edges)) = queue.pop() {
        if let Some(edge) = edges.next() {
            // we proceed dfs-like on the edge
            let target = edge.target();
            if visited.insert(target) {
                queue.extend([(q, edges), (target, ts.edges_from(target).unwrap())]);
            } else {
                queue.push((q, edges));
            }
            continue 'outer;
        }
        // no more edges left, push q to front of l
        l.push_front(q);
    }

    trace!("Computation for L gave {:?}", l);

    // now we do backwards dfs starting from the first element of l
    let reversed = (&ts).reversed();
    let mut sccs = vec![];

    while let Some(next) = l.pop_front() {
        let mut scc = vec![next];
        for v in reversed.reachable_state_indices_from(next) {
            trace!("{} is backward reachable from {}", v.show(), next.show());
            if let Some(pos) = l.iter().position(|x| x == &v) {
                l.remove(pos);
                scc.push(v);
            }
        }
        assert!(scc.contains(&next));
        sccs.push(Scc::new(ts, scc.into_iter()));
    }

    SccDecomposition::new(ts, sccs)
}

pub(crate) fn tarjan_scc_broken<Ts>(ts: &Ts) -> SccDecomposition<'_, Ts>
where
    Ts: TransitionSystem,
{
    let mut pre_count = 0;
    let mut sccs = Vec::new();
    let mut low = Map::default();
    let mut visited = Set::default();
    let mut stack = VecDeque::new();

    let mut min_stack = VecDeque::new();
    let mut iterator_stack: VecDeque<VecDeque<_>> = VecDeque::new();
    let mut it = ts.state_indices().collect::<VecDeque<_>>();

    'outer: loop {
        if let Some(v) = it.pop_front() {
            if visited.insert(v) {
                low.insert(v, pre_count);
                stack.push_back(v);
                min_stack.push_back(pre_count);
                iterator_stack.push_back(it);
                it = ts.edges_from(v).unwrap().map(|e| e.target()).collect();
                pre_count += 1;
            } else if !min_stack.is_empty() {
                let mut min = min_stack.pop_back().unwrap();
                if low.get(&v).unwrap() < &min {
                    min = *low.get(&v).unwrap();
                }
                min_stack.push_back(min);
            }
            continue 'outer;
        }

        'inner: loop {
            let Some(mut it) = iterator_stack.pop_back() else {
                break 'outer;
            };
            let Some(v) = it.pop_front() else {
                continue 'inner;
            };
            let Some(min) = min_stack.pop_back() else {
                panic!("Stacks should have matching height!");
            };

            if min < *low.get(&v).unwrap_or(&usize::MAX) {
                low.insert(v, min);
            } else {
                let mut scc = Set::default();
                'innermost: loop {
                    let w = stack.pop_back().unwrap();
                    scc.insert(w);
                    low.insert(w, ts.size());
                    if w == v {
                        break 'innermost;
                    }
                }
                sccs.push(Scc::new(ts, scc.into_iter()));
            }

            if !min_stack.is_empty() {
                let mut min = min_stack.pop_back().unwrap();
                if low.get(&v).unwrap() < &min {
                    min = *low.get(&v).unwrap();
                }
                min_stack.push_back(min);
            }
        }
    }

    sccs.sort();
    SccDecomposition::new(ts, sccs)
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::{
        ts::{connected_components::tarjan::kosaraju, predecessors::PredecessorIterable, NTS},
        Pointed, TransitionSystem,
    };

    use super::tarjan_scc_broken;

    #[test]
    fn tarjan_iterative() {
        let ts = NTS::builder()
            .with_transitions([
                (0, 'a', 0, 0),
                (0, 'b', 1, 1),
                (0, 'c', 2, 2),
                (1, 'a', 0, 1),
                (2, 'a', 3, 3),
                (2, 'b', 2, 2),
                (3, 'a', 2, 2),
            ])
            .into_dpa(0);

        let rev = (&ts).reversed();
        println!("{:?}", rev.edges_from(2usize).unwrap().collect::<Vec<_>>());
        println!("{:?}", ts.edges_from(2usize).unwrap().collect::<Vec<_>>());
        println!(
            "{:?}",
            rev.predecessors(2usize).unwrap().collect::<Vec<_>>()
        );
        println!("{:?}", ts.predecessors(2usize).unwrap().collect::<Vec<_>>());
        let reachable = rev
            .reachable_state_indices_from(3usize)
            .collect::<HashSet<_>>();
        assert!(reachable.contains(&3));
        assert!(reachable.contains(&2));

        let sccs = kosaraju(&ts, ts.initial());
        println!("{sccs:?}");
    }
}
