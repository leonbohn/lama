use super::transition::DeterministicTransition;
use super::{FiniteState, Growable, IntoStateReferences, Transition, TransitionSystem};
use itertools::Itertools;
use std::{collections::BTreeSet, fmt::Debug, hash::Hash};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Deterministic<S = char, Q = u32> {
    states: Vec<Q>,
    edges: Vec<DeterministicTransition<S, Q>>,
}

impl Deterministic {
    pub fn new() -> Self {
        Self {
            edges: Vec::new(),
            states: Vec::new(),
        }
    }
}

impl<S: Clone + PartialEq, Q: Clone + PartialEq> Deterministic<S, Q> {
    fn clear_targets(&mut self, from: &Q, on: &S) {
        self.edges.retain(|t| t.0 != *from || t.1 != *on);
    }
}

impl Default for Deterministic {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: IntoIterator<Item = (u32, char, u32)>> From<I> for Deterministic {
    fn from(iter: I) -> Self {
        let edges: Vec<DeterministicTransition<char, u32>> = iter
            .into_iter()
            .map(|(p, a, q)| DeterministicTransition(p, a, q))
            .collect();

        Self {
            states: edges
                .iter()
                .flat_map(|DeterministicTransition(from, _, to)| vec![from.clone(), to.clone()])
                .unique()
                .collect(),
            edges,
        }
    }
}

impl<S, Q> TransitionSystem for Deterministic<S, Q>
where
    S: Clone + Eq + Hash + Debug,
    Q: Debug + Hash + Eq + Clone,
{
    type Q = Q;
    type S = S;
    type Transition = DeterministicTransition<S, Q>;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.edges
            .iter()
            .find(|DeterministicTransition(f, s, _)| f == from && s == on)
            .map(|DeterministicTransition(_, _, t)| t.clone())
    }
}

impl<Q, S> FiniteState for Deterministic<S, Q>
where
    Q: Debug + Hash + Eq + Clone,
    S: Clone + Eq + Hash + Debug,
{
    fn states(&self) -> Vec<Self::Q> {
        self.states.clone()
    }

    fn size(&self) -> usize {
        self.states().len()
    }
}

impl<Q, S> Growable for Deterministic<S, Q>
where
    Q: Debug + Hash + Eq + Clone + Ord,
    S: Clone + Eq + Hash + Debug + Ord,
{
    fn add_state(&mut self) -> Self::Q {
        todo!()
    }

    fn add_transition<X: AsRef<Self::S>>(
        &mut self,
        from: Self::Q,
        on: Self::S,
        to: Self::Q,
    ) -> std::option::Option<Q> {
        let old_target = self
            .edges
            .iter()
            .find_map(|DeterministicTransition(f, s, t)| {
                if f == &from && s == &on {
                    Some(t.clone())
                } else {
                    None
                }
            });
        self.clear_targets(&from, &on);
        self.edges.push(DeterministicTransition(from, on, to));
        old_target
    }
}
