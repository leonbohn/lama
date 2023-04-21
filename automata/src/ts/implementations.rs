use crate::{Set, StateIndex, Symbol, TransitionSystem};

use super::TransitionIterable;

pub type VecTS<Q, S> = Vec<(Q, S, Q)>;
pub type SetTS<Q, S> = Set<(Q, S, Q)>;

impl<Q: StateIndex, S: Symbol> TransitionSystem for VecTS<Q, S> {
    type State = Q;

    type Input = S;

    fn succ(&self, from: &Self::State, on: &Self::Input) -> Option<Self::State> {
        self.iter().find_map(|(p, a, q)| {
            if p == from && a == on {
                Some(q.clone())
            } else {
                None
            }
        })
    }

    fn vec_alphabet(&self) -> Vec<Self::Input> {
        self.iter().map(|(_, a, _)| a.clone()).collect()
    }

    fn vec_states(&self) -> Vec<Self::State> {
        self.iter()
            .flat_map(|(p, _, q)| vec![p.clone(), q.clone()])
            .collect()
    }
}
impl<Q: StateIndex, S: Symbol> TransitionSystem for SetTS<Q, S> {
    type State = Q;

    type Input = S;

    fn succ(&self, from: &Self::State, on: &Self::Input) -> Option<Self::State> {
        self.iter().find_map(|(p, a, q)| {
            if p == from && a == on {
                Some(q.clone())
            } else {
                None
            }
        })
    }

    fn vec_alphabet(&self) -> Vec<Self::Input> {
        self.iter().map(|(_, a, _)| a.clone()).collect()
    }

    fn vec_states(&self) -> Vec<Self::State> {
        self.iter()
            .flat_map(|(p, _, q)| vec![p.clone(), q.clone()])
            .collect()
    }
}

impl<Q: StateIndex, S: Symbol> TransitionIterable for VecTS<Q, S> {
    type TransitionIter<'me> = std::slice::Iter<'me, (Q, S, Q)>
    where
        Self: 'me,
        Self::State: 'me,
        Self::Input: 'me;

    fn transitions_iter(&self) -> Self::TransitionIter<'_> {
        self.iter()
    }
}

impl<Q: StateIndex, S: Symbol> TransitionIterable for SetTS<Q, S> {
    type TransitionIter<'me> = std::collections::hash_set::Iter<'me, (Q, S, Q)>
    where
        Self: 'me,
        Self::State: 'me,
        Self::Input: 'me;

    fn transitions_iter(&self) -> Self::TransitionIter<'_> {
        self.iter()
    }
}
