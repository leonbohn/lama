use crate::{
    ts::{IntoTransitions, Trimmable, Visitor},
    Combined, Pointed, Set, StateIndex, Successor, Symbol, TransitionSystem,
};

impl<Q: StateIndex, S: Symbol, Acc> Combined<TransitionSystem<Q, S>, Acc> {
    /// Trims self, mutably. See [`Trimmable::trim`] for more information on the trim
    /// operation itself.
    pub fn trim_in_place(&mut self) {
        let mut bfs = self.bfs().iter().collect::<Set<_>>();
        self.ts_mut().states.retain(|s| bfs.contains(s));
        self.ts_mut()
            .edges
            .retain(|(q, a), p| bfs.contains(q) && bfs.contains(p));
    }

    /// Returns a trim version of the automaton, meaning all unreachable states and transitions
    /// originating/ending in them are removed.
    pub fn trim(&self) -> Self
    where
        Self: Clone,
    {
        let mut out = self.clone();
        out.trim_in_place();
        out
    }
}

#[cfg(test)]
mod tests {
    use crate::DFA;

    #[test]
    fn trimming_test() {
        let dfa = DFA::from_parts_iters(
            [
                (0, 'a', 1),
                (1, 'a', 1),
                (0, 'b', 0),
                (1, 'b', 1),
                (2, 'a', 0),
                (2, 'b', 1),
            ],
            [0],
            0,
        );

        assert_eq!(dfa.size(), 3);
        assert_eq!(dfa.trim().size(), 2);
    }
}
