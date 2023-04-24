use std::collections::{BTreeSet, VecDeque};

use crate::{Pointed, Set, TransitionSystem};

use super::{TransitionOf, TriggerOf};

pub trait Visitor {
    type Place;

    fn visit_next(&mut self) -> Option<Self::Place>;

    fn iter(self) -> VisitorIter<Self>
    where
        Self: Sized,
    {
        VisitorIter { visitor: self }
    }
}

#[derive(Debug, Clone)]
pub struct VisitorIter<V> {
    visitor: V,
}

impl<V> Iterator for VisitorIter<V>
where
    V: Visitor,
{
    type Item = V::Place;

    fn next(&mut self) -> Option<Self::Item> {
        self.visitor.visit_next()
    }
}

#[derive(Debug, Clone)]
pub struct LengthLexicographic<TS: TransitionSystem> {
    ts: TS,
    alphabet: BTreeSet<TS::Input>,
    queue: VecDeque<TS::State>,
    seen: Set<TS::State>,
}

pub struct LengthLexicographicEdges<TS: TransitionSystem> {
    ts: TS,
    alphabet: BTreeSet<TS::Input>,
    queue: VecDeque<(TS::State, TS::Input)>,
    seen: Set<TS::State>,
}

impl<TS> LengthLexicographic<TS>
where
    TS: TransitionSystem,
{
    /// Creates a new `LengthLexicographic` visitor from a given state.
    pub fn new_from(ts: TS, start: TS::State) -> Self {
        let alphabet: BTreeSet<_> = ts.set_alphabet().into_iter().collect();
        let queue = VecDeque::from([start.clone()]);
        let seen = Set::from([start]);

        Self {
            ts,
            alphabet,
            queue,
            seen,
        }
    }

    /// Creates a new `LengthLexicographic` visitor from the initial state of `TS`.
    pub fn new(ts: TS) -> Self
    where
        TS: Pointed,
    {
        let start = ts.initial();
        Self::new_from(ts, start)
    }
}

impl<TS> LengthLexicographicEdges<TS>
where
    TS: TransitionSystem,
{
    /// Creates a new `LengthLexicographicEdges` visitor from a given state.
    pub fn new_from(ts: TS, start: TS::State) -> Self {
        let alphabet: BTreeSet<_> = ts.set_alphabet().into_iter().collect();
        let queue = alphabet
            .iter()
            .map(|sym| (start.clone(), sym.clone()))
            .collect();
        let seen = Set::from([start]);

        Self {
            ts,
            alphabet,
            queue,
            seen,
        }
    }

    /// Creates a new `LengthLexicographicEdges` visitor from the initial state of `TS`.
    pub fn new(ts: TS) -> Self
    where
        TS: Pointed,
    {
        let start = ts.initial();
        Self::new_from(ts, start)
    }
}

impl<TS> Visitor for LengthLexicographic<TS>
where
    TS: TransitionSystem,
{
    type Place = TS::State;

    fn visit_next(&mut self) -> Option<Self::Place> {
        if let Some(q) = self.queue.pop_front() {
            for sym in &self.alphabet {
                if let Some(successor) = self.ts.succ(&q, sym) {
                    if self.seen.insert(successor.clone()) {
                        self.queue.push_back(successor);
                    }
                }
            }
            Some(q)
        } else {
            None
        }
    }
}

impl<TS> Visitor for LengthLexicographicEdges<TS>
where
    TS: TransitionSystem,
{
    type Place = TransitionOf<TS>;

    fn visit_next(&mut self) -> Option<Self::Place> {
        if let Some((q, sym)) = self.queue.pop_front() {
            if let Some(successor) = self.ts.succ(&q, &sym) {
                if self.seen.insert(successor.clone()) {
                    self.queue.extend(
                        self.alphabet
                            .iter()
                            .map(|sym| (successor.clone(), sym.clone())),
                    );
                }
                Some((q, sym, successor))
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::{ts::Visitor, TransitionSystem};

    #[test]
    fn bfs_search() {
        let ts = vec![(0, 'a', 1), (0, 'b', 0), (1, 'a', 0), (1, 'b', 1)];
        assert_eq!(ts.bfs_from(0).iter().collect::<Vec<_>>(), vec![0, 1]);
        assert_eq!(ts.bfs_edges_from(0).iter().collect::<Vec<_>>(), ts);
    }
}
