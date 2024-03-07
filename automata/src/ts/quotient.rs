use std::collections::HashMap;

use itertools::Itertools;

use crate::{Alphabet, Partition, Pointed, RightCongruence, Set, Show, TransitionSystem};

use super::{
    transition_system::{Indexes, IsEdge},
    Deterministic, ExpressionOf, SymbolOf,
};

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
    expressions: crate::Map<SymbolOf<Ts>, ExpressionOf<Ts>>,
    partition: Partition<Ts::StateIndex>,
}

impl<Ts: Deterministic + Pointed> Pointed for Quotient<Ts> {
    fn initial(&self) -> Self::StateIndex {
        self.find_id_by_state(self.ts.initial())
            .expect("Initial class must exist")
    }
}

impl<Ts: TransitionSystem> Quotient<Ts> {
    /// Returns a reference to the [`Partition`] underlying the quotient.
    pub fn partition(&self) -> &Partition<Ts::StateIndex> {
        &self.partition
    }

    /// Gives a reference to the underlying transition system.
    pub fn ts(&self) -> &Ts {
        &self.ts
    }

    /// Indexes into the underlying partition and tries to get a representative for the indexed class.
    /// Panics if the class does not exist or is empty.
    pub fn unwrap_class_representative(&self, id: usize) -> Ts::StateIndex {
        *self
            .partition
            .get(id)
            .expect("Class must exist")
            .iter()
            .next()
            .expect("Class must not be empty")
    }

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
                println!("SANITY CHECK FAILED:\n{:?}", partition);
                return false;
            }
        }
        true
    }

    /// Extracts the underlying right congruence by erasing the state and edge colors and then collecting
    /// into a [`RightCongruence`].
    pub fn underlying_right_congruence(self, ts: &Ts) -> RightCongruence<Ts::Alphabet>
    where
        Ts: Deterministic + Pointed,
    {
        self.erase_edge_colors()
            .erase_state_colors()
            .collect_right_congruence_bare()
    }

    /// Creates a new quotient of the given transition system by the give [`Partition`].
    pub fn new(ts: Ts, partition: Partition<Ts::StateIndex>) -> Self {
        Self {
            expressions: ts
                .alphabet()
                .universe()
                .map(|sym| (sym, <Ts::Alphabet as Alphabet>::expression(sym)))
                .collect(),
            ts,
            partition,
        }
    }
}

pub struct QuotientTransition<'a, Idx, E, C> {
    source: Idx,
    expression: &'a E,
    colors: Vec<C>,
    target: Idx,
}

impl<'a, Idx: Copy, E, C: Clone> IsEdge<'a, E, Idx, Vec<C>> for QuotientTransition<'a, Idx, E, C> {
    fn target(&self) -> Idx {
        self.target
    }

    fn color(&self) -> Vec<C> {
        self.colors.clone()
    }

    fn expression(&self) -> &'a E {
        self.expression
    }

    fn source(&self) -> Idx {
        self.source
    }
}

impl<'a, Idx, E, C> QuotientTransition<'a, Idx, E, C> {
    pub fn new(source: Idx, expression: &'a E, colors: Vec<C>, target: Idx) -> Self {
        Self {
            source,
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
    I: Iterator<Item = SymbolOf<Ts>>,
{
    type Item = QuotientTransition<'a, usize, ExpressionOf<Ts>, Ts::EdgeColor>;

    fn next(&mut self) -> Option<Self::Item> {
        let sym = self.it.next()?;
        self.quot.transition(self.class, sym)
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

    type EdgeRef<'this> = QuotientTransition<'this, usize, ExpressionOf<Self>, Ts::EdgeColor>
    where
        Self: 'this;

    type EdgesFromIter<'this> = QuotientEdgesFrom<'this, Ts, <Self::Alphabet as Alphabet>::Universe<'this>>
    where
        Self: 'this;
    type StateIndices<'this> = std::ops::Range<usize> where Self: 'this;

    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
    fn state_indices(&self) -> Self::StateIndices<'_> {
        0..self.partition.len()
    }

    fn state_color<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::StateColor> {
        let state = state.to_index(self)?;
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
    ) -> Option<Self::EdgeRef<'_>> {
        let origin = state.to_index(self)?;
        let (states, colors): (Set<_>, Vec<_>) = self
            .class_iter_by_id(origin)?
            .filter_map(|q| {
                self.ts.transition(q, symbol).map(|tt| {
                    (
                        self.find_id_by_state(tt.target()).expect("Unknown state"),
                        tt.color().clone(),
                    )
                })
            })
            .unzip();

        match states.len() {
            0 => None,
            1 => {
                let expression = self.expressions.get(&symbol).unwrap();
                Some(QuotientTransition {
                    source: origin,
                    expression,
                    colors,
                    target: states.into_iter().next().unwrap(),
                })
            }
            _ => {
                let string = format!(
                    "{{{}}}",
                    states.iter().map(|idx| idx.to_string()).join(", ")
                );
                panic!("From {origin}|{} on symbol {}, we reach {} while precisely one state should be reached!", self.class_iter_by_id(origin).unwrap().map(|c| c.to_string()).join(", "), symbol.show(), string)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        tests::wiki_dfa,
        ts::{Deterministic, Dottable},
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
