use std::collections::{hash_map::RandomState, BTreeSet, VecDeque};

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

pub(crate) fn tarjan_scc_iterative<Ts>(ts: &Ts) -> SccDecomposition<'_, Ts>
where
    Ts: TransitionSystem,
{
    let mut current = 0;
    let mut sccs = vec![];

    let mut indices = Map::default();
    let mut low = Map::default();
    let mut stack = vec![];
    let mut on_stack = Set::default();
    let mut unvisited = ts.state_indices().collect::<BTreeSet<_>>();

    let Some(start) = unvisited.pop_first() else {
        return SccDecomposition::new(ts, sccs);
    };
    let mut queue = vec![(start, ts.edges_from(start).unwrap())];

    'outer: while let Some((q, mut edges)) = queue.pop() {
        trace!(
            "considering state {}\tstack: {:?}\ton_stack: {:?}",
            q.show(),
            stack,
            on_stack
        );
        unvisited.remove(&q);

        if on_stack.insert(q) {
            stack.push(q);
        }

        if !indices.contains_key(&q) {
            trace!("assigning index {current} to state {}", q.show());
            indices.insert(q, current);
            low.insert(q, current);
            current += 1;
        }

        'inner: while let Some(edge) = edges.next() {
            trace!(
                "considering edge {} --{}--> {}",
                edge.source().show(),
                edge.expression().show(),
                edge.target().show()
            );
            let target = edge.target();
            if unvisited.contains(&target) {
                trace!(
                    "successor {} on {} has not been visited, descending",
                    target.show(),
                    edge.expression().show()
                );
                queue.push((q, edges));
                queue.push((target, ts.edges_from(target).unwrap()));
                continue 'outer;
            }
            if on_stack.contains(&target) {
                let new_low = std::cmp::min(*low.get(&q).unwrap(), *low.get(&target).unwrap());
                *low.get_mut(&q).unwrap() = new_low;
                trace!(
                    "successor {} on {} was alread seen, assigning new minimum {new_low}",
                    target.show(),
                    edge.expression().show()
                );
            }
        }

        // reached when all edges have been explored
        let low_q = *low.get(&q).unwrap();
        if low_q == *indices.get(&q).unwrap() {
            trace!(
                "{} has matching index and low {low_q}, extracting scc",
                q.show()
            );
            let mut scc = vec![];
            while on_stack.contains(&q) {
                let top = stack.pop().unwrap();
                low.insert(top, low_q);
                on_stack.remove(&top);
                scc.push(top);
            }
            let scc = Scc::new(ts, scc.into_iter());
            trace!("identified scc {:?}", scc);
            sccs.push(scc);
        }
    }

    sccs.sort();
    SccDecomposition::new(ts, sccs)
}

#[cfg(test)]
mod tests {
    use std::{collections::HashSet, time::Instant};

    use crate::{
        ts::{
            connected_components::{tarjan::kosaraju, tarjan_scc_recursive},
            predecessors::PredecessorIterable,
            NTS,
        },
        Pointed, TransitionSystem,
    };

    use super::tarjan_scc_iterative;

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
        let reachable = rev
            .reachable_state_indices_from(3usize)
            .collect::<HashSet<_>>();
        assert!(reachable.contains(&3));
        assert!(reachable.contains(&2));

        let start = Instant::now();
        let sccs = kosaraju(&ts, ts.initial());
        println!("Kosaraju took {} microseconds", start.elapsed().as_micros());

        let start = Instant::now();
        let sccs = tarjan_scc_recursive(&ts);
        println!(
            "Tarjan recursive took {} microseconds",
            start.elapsed().as_micros()
        );

        let start = Instant::now();
        let sccs = tarjan_scc_iterative(&ts);
        println!(
            "Tarjan iterative took {} microseconds",
            start.elapsed().as_micros()
        );

        println!("{sccs:?}");
    }
}
