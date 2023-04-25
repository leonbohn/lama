use std::collections::{BTreeSet, VecDeque};

use crate::{Pointed, Set, TransitionSystem};

use super::{StateOf, TransitionOf};

/// Trait that encapsulates the ability to visit `Places` of a transition system.
/// A `place` could be a state or a transition, in the future, we might also allow
/// loops and SCCs as places.
/// The main imlementors of this trait will be `LengthLexicographic` and
/// `LengthLexicographicEdges`, which are objects that visit states/transitions of a
/// transition system in length-lexicographic order (which essentially corresponds to
/// visitation in breadth-first order from the initial state).
pub trait Visitor {
    /// The type of thing that is visited, could be a state or a transition.
    type Place;

    /// Visits the next place.
    fn visit_next(&mut self) -> Option<Self::Place>;

    /// Returns an iterator over the places visited by this visitor.
    fn iter(self) -> VisitorIter<Self>
    where
        Self: Sized,
    {
        VisitorIter { visitor: self }
    }
}

/// Stores a visitor and allows iteration over the places visited by it.
#[derive(Debug, Clone)]
pub struct VisitorIter<V> {
    pub(crate) visitor: V,
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

/// A [`Visitor`] that visits states of a transition system in length-lexicographic order.
#[derive(Debug, Clone)]
pub struct LengthLexicographic<TS: TransitionSystem> {
    ts: TS,
    alphabet: BTreeSet<TS::Sigma>,
    queue: VecDeque<StateOf<TS>>,
    seen: Set<StateOf<TS>>,
}
/// A [`Visitor`] that visits edges (i.e. state-symbol-state triples) of a transition
/// system in length-lexicographic order.
#[derive(Debug, Clone)]
pub struct LengthLexicographicEdges<TS: TransitionSystem> {
    ts: TS,
    alphabet: BTreeSet<TS::Sigma>,
    queue: VecDeque<(StateOf<TS>, TS::Sigma)>,
    seen: Set<StateOf<TS>>,
}

impl<TS> LengthLexicographic<TS>
where
    TS: TransitionSystem,
{
    /// Creates a new `LengthLexicographic` visitor from a given state.
    pub fn new_from(ts: TS, start: StateOf<TS>) -> Self {
        let alphabet: BTreeSet<_> = ts.input_alphabet().cloned().collect();
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
    pub fn new_from(ts: TS, start: StateOf<TS>) -> Self {
        let alphabet: BTreeSet<_> = ts.input_alphabet().cloned().collect();
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
    type Place = StateOf<TS>;

    fn visit_next(&mut self) -> Option<Self::Place> {
        if let Some(q) = self.queue.pop_front() {
            for sym in &self.alphabet {
                if let Some(successor) = self.ts.successor(&q, sym) {
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
            if let Some(successor) = self.ts.successor(&q, &sym) {
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
    use crate::{ts::Visitor, Deterministic, TransitionSystem};

    #[test]
    fn bfs_search() {
        let ts = Deterministic::from_iter([(0, 'a', 1), (0, 'b', 0), (1, 'a', 0), (1, 'b', 1)]);
        assert_eq!(ts.bfs_from(0).iter().collect::<Vec<_>>(), vec![0, 1]);
        assert_eq!(
            ts.bfs_edges_from(0).iter().collect::<Deterministic<_>>(),
            ts
        );
    }
}
