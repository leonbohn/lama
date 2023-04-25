use crate::{
    ts::IntoTransitions, Growable, HasAlphabet, Pair, StateIndex, Successor, Symbol,
    TransitionSystem,
};

impl<Q: StateIndex, S: Symbol> TransitionSystem<Q, S> {
    /// Constructs the product transition system of `self` and `other`.
    pub fn product_ts<T: IntoTransitions<Sigma = S>>(
        &self,
        other: T,
    ) -> TransitionSystem<Pair<Q, T::Q>, S> {
        let other: TransitionSystem<_, _> = other.into_transitions().collect();
        let mut ts = TransitionSystem::new();
        for p in self.states() {
            for q in other.states() {
                ts.add_state(&Pair {
                    left: p.clone(),
                    right: q.clone(),
                });
            }
        }

        for p in self.states() {
            for q in other.states() {
                for a in self.alphabet() {
                    if let (Some(succ_p), Some(succ_q)) =
                        (self.successor(p, &a), other.successor(q, &a))
                    {
                        ts.add_transition(
                            Pair {
                                left: p.clone(),
                                right: q.clone(),
                            },
                            a,
                            Pair {
                                left: succ_p,
                                right: succ_q,
                            },
                        );
                    }
                }
            }
        }

        ts
    }
}

#[cfg(test)]
mod tests {
    use crate::{ts::IntoTransitions, TransitionSystem};

    #[test]
    fn simple_direct_product() {
        let left =
            TransitionSystem::from_iter([(0, 'a', 0), (0, 'b', 1), (1, 'a', 0), (1, 'b', 1)]);
        let right =
            TransitionSystem::from_iter([(0, 'a', 1), (0, 'b', 0), (1, 'a', 0), (1, 'b', 0)]);

        let prod = left.product_ts(&right);
        assert_eq!(prod.size(), 4);

        let edges = prod.into_transitions();
        assert_eq!(edges.count(), 8);
    }
}
