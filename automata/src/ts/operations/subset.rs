use std::{cell::RefCell, f32::consts::E, fmt::Debug};

use itertools::Itertools;

use crate::{prelude::*, ts::reachable::ReachableStateIndices, Set};

#[derive(Clone, Eq)]
pub struct StateSet<Ts: TransitionSystem>(Set<Ts::StateIndex>);

impl<Ts: TransitionSystem> Extend<Ts::StateIndex> for StateSet<Ts> {
    fn extend<T: IntoIterator<Item = Ts::StateIndex>>(&mut self, iter: T) {
        self.0.extend(iter)
    }
}

impl<Ts: TransitionSystem> PartialEq for StateSet<Ts> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<Ts: TransitionSystem> Default for StateSet<Ts> {
    fn default() -> Self {
        Self(Set::default())
    }
}

impl<Ts: TransitionSystem> IntoIterator for StateSet<Ts> {
    type IntoIter = std::collections::hash_set::IntoIter<Ts::StateIndex>;
    type Item = Ts::StateIndex;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<Ts: TransitionSystem> FromIterator<Ts::StateIndex> for StateSet<Ts> {
    fn from_iter<T: IntoIterator<Item = Ts::StateIndex>>(iter: T) -> Self {
        Self(Set::from_iter(iter))
    }
}

impl<Ts: TransitionSystem> StateSet<Ts> {
    pub fn singleton(q: Ts::StateIndex) -> Self {
        Self(Set::from_iter([q]))
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ Ts::StateIndex> + '_ {
        self.0.iter()
    }
}

pub struct SubsetConstruction<Ts: TransitionSystem> {
    ts: Ts,
    states: RefCell<Vec<StateSet<Ts>>>,
}

impl<Ts: TransitionSystem> HasAlphabet for SubsetConstruction<Ts> {
    type Alphabet = Ts::Alphabet;
    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}

impl<Ts: TransitionSystem> Deterministic for SubsetConstruction<Ts> {
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        let (colorset, stateset): (Vec<Ts::EdgeColor>, StateSet<Ts>) = self
            .states
            .borrow()
            .get(state.to_index(self)?)?
            .iter()
            .flat_map(|q| {
                self.ts.transitions_from(*q).filter_map(|(q, a, c, p)| {
                    if a == symbol {
                        Some((c, p))
                    } else {
                        None
                    }
                })
            })
            .unzip();
        if let Some(pos) = self.states.borrow().iter().position(|s| stateset.eq(s)) {
            return Some((
                <Ts::Alphabet as Alphabet>::expression(symbol),
                pos,
                colorset,
            ));
        }

        self.states.borrow_mut().push(stateset);
        Some((
            <Ts::Alphabet as Alphabet>::expression(symbol),
            self.states.borrow().len(),
            colorset,
        ))
    }
}

impl<Ts: TransitionSystem> Pointed for SubsetConstruction<Ts> {
    fn initial(&self) -> Self::StateIndex {
        assert!(!self.states.borrow().is_empty());
        0
    }
}

impl<Ts: TransitionSystem> TransitionSystem for SubsetConstruction<Ts> {
    type StateIndex = usize;

    type StateColor = Vec<Ts::StateColor>;

    type EdgeColor = Vec<Ts::EdgeColor>;

    type TransitionRef<'this> = (ExpressionOf<Ts>, usize, Self::EdgeColor)
    where
        Self: 'this;

    type EdgesFromIter<'this> = DeterministicEdgesFrom<'this, Self>
    where
        Self: 'this;

    type StateIndices<'this> = ReachableStateIndices<&'this Self>
    where
        Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.reachable_state_indices_from(self.initial())
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        Some(DeterministicEdgesFrom::new(self, state.to_index(self)?))
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        let Some(color) = self.states.borrow().get(state).map(|q| {
            q.iter()
                .map(|idx| {
                    let Some(color) = self.ts.state_color(*idx) else {
                        panic!("Could not find state {} in ts {:?}", state, self);
                    };
                    color
                })
                .collect()
        }) else {
            panic!("Could not find state {} in ts {:?}", state, self);
        };
        Some(color)
    }
}

impl<Ts: TransitionSystem> Debug for SubsetConstruction<Ts> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Subset construction\n{}",
            self.build_transition_table(|idx, _| format!(
                "{{{}}}",
                self.states
                    .borrow()
                    .get(idx)
                    .unwrap()
                    .iter()
                    .map(|q| q.to_string())
                    .join(", ")
            ))
        )
    }
}

impl<Ts: TransitionSystem> SubsetConstruction<Ts> {
    pub fn new_from(ts: Ts, idx: Ts::StateIndex) -> Self {
        Self {
            states: vec![StateSet::singleton(idx)].into(),
            ts,
        }
    }

    pub fn new<I: IntoIterator<Item = Ts::StateIndex>>(ts: Ts, iter: I) -> Self {
        Self {
            states: vec![StateSet::from_iter(iter)].into(),
            ts,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        prelude::WithInitial,
        ts::{Deterministic, BTS, NTS},
        TransitionSystem,
    };

    #[test]
    fn subset_construction() {
        let nts = NTS::builder()
            .default_color(false)
            .extend([
                (0, 'a', (), 0),
                (0, 'a', (), 1),
                (0, 'b', (), 1),
                (1, 'b', (), 1),
                (1, 'a', (), 0),
            ])
            .collect()
            .with_initial(0);

        let dts = nts.subset_construction();

        assert_eq!(dts.reachable_state_indices().count(), 3);
        assert_eq!(dts.state_indices().count(), 3);
        assert_eq!(dts.trim_collect().size(), 3);
    }
}