use crate::{
    prelude::{ExpressionOf, HasAlphabet, SymbolOf},
    Alphabet, Partition, Set, TransitionSystem,
};

use super::{transition_system::IsTransition, FiniteState};

#[derive(Debug, Clone)]
pub struct Quotient<Ts: FiniteState + TransitionSystem> {
    ts: Ts,
    partition: Partition<Ts::StateIndex>,
}

impl<Ts: FiniteState + TransitionSystem> Quotient<Ts> {
    pub fn class_by_id_iter(&self, id: usize) -> Option<impl Iterator<Item = Ts::StateIndex> + '_> {
        self.partition.get(id).map(|s| s.iter().cloned())
    }

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

    pub fn new(ts: Ts, partition: Partition<Ts::StateIndex>) -> Self {
        // assert!(Self::sanity_check(&ts, &partition));
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
pub struct QuotientEdgesFrom<'a, Ts: TransitionSystem + FiniteState, I> {
    it: I,
    quot: &'a Quotient<Ts>,
    class: usize,
}

impl<'a, Ts, I> Iterator for QuotientEdgesFrom<'a, Ts, I>
where
    Ts: TransitionSystem + FiniteState,
    I: Iterator<Item = &'a SymbolOf<Ts>>,
{
    type Item = QuotientTransition<usize, ExpressionOf<Ts>, Ts::EdgeColor>;

    fn next(&mut self) -> Option<Self::Item> {
        let sym = self.it.next()?;
        self.quot.transition(self.class, *sym)
    }
}

impl<'a, Ts: TransitionSystem + FiniteState, I> QuotientEdgesFrom<'a, Ts, I> {
    pub fn new(ts: &'a Quotient<Ts>, it: I, class: usize) -> Self {
        Self {
            it,
            quot: ts,
            class,
        }
    }
}

impl<Ts: FiniteState + TransitionSystem> TransitionSystem for Quotient<Ts> {
    type StateIndex = usize;

    type StateColor = Vec<Ts::StateColor>;

    type EdgeColor = Vec<Ts::EdgeColor>;

    type TransitionRef<'this> = QuotientTransition<usize, ExpressionOf<Self>, Ts::EdgeColor>
    where
        Self: 'this;

    type EdgesFromIter<'this> = QuotientEdgesFrom<'this, Ts, <Self::Alphabet as Alphabet>::Universe<'this>>
    where
        Self: 'this;

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: crate::prelude::SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        let (states, colors): (Set<_>, Vec<_>) = self
            .class_by_id_iter(state)?
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
            expression: <Ts::Alphabet as Alphabet>::expression(symbol),
            colors,
            target: states.into_iter().next().unwrap(),
        })
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &crate::prelude::ExpressionOf<Self>,
    ) -> Option<super::EdgeColor<Self>> {
        todo!()
    }

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
        if self.partition.len() <= state {
            None
        } else {
            Some(QuotientEdgesFrom::new(
                self,
                self.alphabet().universe(),
                state,
            ))
        }
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        todo!()
    }
}

impl<Ts: FiniteState + TransitionSystem> HasAlphabet for Quotient<Ts> {
    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        Ts::alphabet(&self.ts)
    }
}

#[cfg(test)]
mod tests {
    use crate::{tests::wiki_dfa, ts::ToDot, Partition, RightCongruence, TransitionSystem};

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
