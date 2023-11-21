use crate::{
    prelude::{ExpressionOf, HasAlphabet, SymbolOf},
    Alphabet, Partition, Pointed, Set, TransitionSystem,
};

use super::{transition_system::IsTransition, Deterministic};

/// A quotient takes a transition system and merges states which are in the same
/// congruence class of some [`Partition`]. We assume that the [`Partition`] is
/// a congruence, meaning if we have two classes `X, Y`, then for all `p`, `q` in X
/// and all symbols `a` in the alphabet, there is an edge from `p` on `a` to some
/// state in `Y` if and only if the same is true for `q`. Thus, there is an edge
/// between states (which are congruence classes) of the [`Quotient`] for some symbol
/// `a`, if there is an edge between the states contained in the class.
///
/// In the implementation of [`TransitionSystem`] for [`Quotient`], the edge and
/// state colors are [`Vec`]s of the respective colors of the underlying transition
/// system, where we simply collect all colors.
#[derive(Debug, Clone)]
pub struct Quotient<Ts: TransitionSystem> {
    ts: Ts,
    partition: Partition<Ts::StateIndex>,
}

impl<Ts: Deterministic + Pointed> Pointed for Quotient<Ts> {
    fn initial(&self) -> Self::StateIndex {
        self.find_id_by_state(self.ts.initial())
            .expect("Initial class must exist")
    }
}

impl<Ts: TransitionSystem> Quotient<Ts> {
    /// Returns an iterator over the indices in the quotient class with the given `id`.
    /// If no such class exists, `None` is returned.
    pub fn class_iter_by_id(&self, id: usize) -> Option<impl Iterator<Item = Ts::StateIndex> + '_> {
        self.partition.get(id).map(|s| s.iter().cloned())
    }

    /// Tries to find the id of the quotient class containing the given state `q`. If
    /// the state is not in the partition, `None` is returned.
    pub fn find_id_by_state(&self, q: Ts::StateIndex) -> Option<usize> {
        self.partition.iter().position(|o| o.contains(&q))
    }

    fn sanity_check(ts: &Ts, partition: &Partition<Ts::StateIndex>) -> bool {
        #[cfg(not(debug_assertions))]
        return true;
        use itertools::Itertools;

        for p in &partition.0 {
            let all_equal = p
                .iter()
                .map(|i| {
                    ts.edges_from(*i)
                        .map(|it| {
                            it.map(|tt| (tt.expression().clone(), tt.target()))
                                .collect::<Set<_>>()
                        })
                        .unwrap_or_default()
                })
                .all_equal();
            if !all_equal {
                return false;
            }
        }
        true
    }

    /// Creates a new quotient of the given transition system by the give [`Partition`].
    pub fn new(ts: Ts, partition: Partition<Ts::StateIndex>) -> Self {
        Self { ts, partition }
    }
}

pub struct QuotientTransition<Idx, E, C> {
    expression: E,
    colors: Vec<C>,
    target: Idx,
}

impl<Idx: Copy, E, C: Clone> IsTransition<E, Idx, Vec<C>> for QuotientTransition<Idx, E, C> {
    fn target(&self) -> Idx {
        self.target
    }

    fn color(&self) -> Vec<C> {
        self.colors.clone()
    }

    fn expression(&self) -> &E {
        &self.expression
    }
}

impl<Idx, E, C> QuotientTransition<Idx, E, C> {
    pub fn new(expression: E, colors: Vec<C>, target: Idx) -> Self {
        Self {
            expression,
            colors,
            target,
        }
    }
}

#[derive(Clone)]
pub struct QuotientEdgesFrom<'a, Ts: TransitionSystem, I> {
    it: I,
    quot: &'a Quotient<Ts>,
    class: usize,
}

impl<'a, Ts, I> Iterator for QuotientEdgesFrom<'a, Ts, I>
where
    Ts: Deterministic,
    I: Iterator<Item = &'a SymbolOf<Ts>>,
{
    type Item = QuotientTransition<usize, ExpressionOf<Ts>, Ts::EdgeColor>;

    fn next(&mut self) -> Option<Self::Item> {
        let sym = self.it.next()?;
        self.quot.transition(self.class, *sym)
    }
}

impl<'a, Ts: TransitionSystem, I> QuotientEdgesFrom<'a, Ts, I> {
    pub fn new(ts: &'a Quotient<Ts>, it: I, class: usize) -> Self {
        Self {
            it,
            quot: ts,
            class,
        }
    }
}

impl<Ts: Deterministic> TransitionSystem for Quotient<Ts> {
    type StateIndex = usize;

    type StateColor = Vec<Ts::StateColor>;

    type EdgeColor = Vec<Ts::EdgeColor>;

    type TransitionRef<'this> = QuotientTransition<usize, ExpressionOf<Self>, Ts::EdgeColor>
    where
        Self: 'this;

    type EdgesFromIter<'this> = QuotientEdgesFrom<'this, Ts, <Self::Alphabet as Alphabet>::Universe<'this>>
    where
        Self: 'this;
    type StateIndices<'this> = std::ops::Range<usize> where Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        0..self.partition.len()
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        let mut it = self.class_iter_by_id(state)?;
        it.map(|o| self.ts.state_color(o)).collect()
    }

    fn edges_from<Idx: super::transition_system::Indexes<Self>>(
        &self,
        state: Idx,
    ) -> Option<Self::EdgesFromIter<'_>> {
        if self.partition.len() <= state.to_index(self)? {
            None
        } else {
            Some(QuotientEdgesFrom::new(
                self,
                self.alphabet().universe(),
                state.to_index(self)?,
            ))
        }
    }
}
impl<D: Deterministic> Deterministic for Quotient<D> {
    fn transition<Idx: super::transition_system::Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        let (states, colors): (Set<_>, Vec<_>) = self
            .class_iter_by_id(state.to_index(self)?)?
            .filter_map(|q| {
                self.ts.transition(q, symbol).map(|tt| {
                    (
                        self.find_id_by_state(tt.target()).expect("Unknown state"),
                        tt.color(),
                    )
                })
            })
            .unzip();
        assert_eq!(
            states.len(),
            1,
            "More than one quotient class reached, partition was faulty"
        );
        Some(QuotientTransition {
            expression: <D::Alphabet as Alphabet>::expression(symbol),
            colors,
            target: states.into_iter().next().unwrap(),
        })
    }
}

impl<Ts: TransitionSystem> HasAlphabet for Quotient<Ts> {
    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        Ts::alphabet(&self.ts)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        tests::wiki_dfa,
        ts::{Deterministic, ToDot},
        Partition, RightCongruence, TransitionSystem,
    };

    #[test]
    fn quotient_test() {
        let dfa = wiki_dfa();
        let p = Partition::new([vec![0, 1], vec![5], vec![2, 3, 4]]);
        let a = dfa.quotient(p);

        for (i, p) in [0, 2, 1, 1, 2, 1].into_iter().enumerate() {
            let q = i / 2;
            let sym = ['a', 'b'][i % 2];
            assert_eq!(a.successor_index(q, sym), Some(p));
        }
    }
}
