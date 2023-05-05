use std::collections::{BTreeSet, VecDeque};

use tracing::trace;

use crate::{
    ts::{StateOf, TransitionOf, TriggerOf},
    Pointed, Set, Successor,
};

use super::{Place, Visitor};

/// A [`Visitor`] that visits states of a transition system in length-lexicographic order.
#[derive(Debug, Clone)]
pub struct Dfs<TS: Successor> {
    ts: TS,
    alphabet: BTreeSet<TS::Sigma>,
    queue: VecDeque<TriggerOf<TS>>,
    seen: Set<StateOf<TS>>,
}

impl<TS> Dfs<TS>
where
    TS: Successor,
{
    /// Creates a new `Dfs` visitor from a given state.
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

    /// Creates a new `Dfs` visitor from the initial state of `TS`.
    pub fn new(ts: TS) -> Self
    where
        TS: Pointed,
    {
        let start = ts.initial();
        Self::new_from(ts, start)
    }
}

impl<TS> Visitor for Dfs<TS>
where
    TS: Successor,
{
    type Place = TransitionOf<TS>;
    fn visit_next(&mut self) -> Option<TransitionOf<TS>> {
        while let Some((q, sym)) = self.queue.pop_back() {
            trace!("Examining {:?} on {:?}", q, sym);
            if let Some(successor) = self.ts.successor(&q, &sym) {
                trace!("Found successor {:?}", successor);
                if self.seen.insert(successor.clone()) {
                    trace!("Which is new, so we add it to the seen set");
                    self.queue.extend(
                        self.alphabet
                            .iter()
                            .rev()
                            .map(|sym| (successor.clone(), sym.clone())),
                    );
                    trace!("Which is new, so we add all its outgoing edges to the queue");
                } else {
                    trace!("Which was already seen");
                }
                return Some((q, sym, successor));
            } else {
                trace!("Which has no successor");
                continue;
            }
        }
        trace!("Queue is empty");
        None
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{ts::Visitor, Successor, TransitionSystem};

    #[test]
    fn dfs_search() {
        let ts = TransitionSystem::from_iter([(0, 'a', 1), (0, 'b', 0), (1, 'a', 0), (1, 'b', 1)]);
        assert_eq!(ts.dfs_from(0).iter().collect::<TransitionSystem<_>>(), ts);
    }

    #[test]
    fn dfs_order() {
        let ts = TransitionSystem::from_iter([
            (0, 'a', 1),
            (0, 'b', 2),
            (1, 'a', 2),
            (2, 'a', 1),
            (2, 'b', 2),
        ]);
    }
}
