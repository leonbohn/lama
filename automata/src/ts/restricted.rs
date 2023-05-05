use crate::{
    output::IntoAssignments, Growable, Mapping, Pointed, Set, Successor, Transition,
    TransitionSystem, Trigger,
};

use super::{HasInput, HasStates, InputOf, IntoTransitions, StateOf};

/// Allows the restriction of a transition system to a subset of its states.
#[derive(Debug, Clone)]
pub struct Restricted<TS, F> {
    pub(crate) on: TS,
    pub(crate) mask: F,
}

impl<TS, F> Restricted<TS, F> {
    /// Restrict the transition system `self` to states which satisfy the given predicate.
    pub fn new(on: TS, mask: F) -> Self {
        Self { on, mask }
    }
}

impl<TS: Successor, F: Fn(&TS::Q) -> bool> HasStates for Restricted<TS, F> {
    type Q = StateOf<TS>;

    fn contains_state<X: std::borrow::Borrow<Self::Q>>(&self, state: X) -> bool {
        (self.mask)(state.borrow()) && self.on.contains_state(state)
    }
}
impl<TS: Successor, F> HasInput for Restricted<TS, F> {
    type Sigma = InputOf<TS>;

    type Input<'me> = TS::Input<'me>
    where
        Self: 'me;

    fn raw_input_alphabet_iter(&self) -> Self::Input<'_> {
        self.on.raw_input_alphabet_iter()
    }
}
impl<TS: Successor, F: Fn(&TS::Q) -> bool> Successor for Restricted<TS, F> {
    fn successor<X: std::borrow::Borrow<Self::Q>, Y: std::borrow::Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q> {
        if !(self.mask)(from.borrow()) {
            return None;
        }
        self.on
            .successor(from, on)
            .filter(|successor| (self.mask)(successor))
    }
}
impl<TS: Pointed, F: Fn(&TS::Q) -> bool> Pointed for Restricted<TS, F> {
    fn initial(&self) -> Self::Q {
        let initial = self.on.initial();
        assert!(
            (self.mask)(&initial),
            "The initial state may not be masked out!"
        );
        initial
    }
}

pub struct RestrictedTransitions<I, F>
where
    I: Iterator,
{
    iter: I,
    mask: F,
}

impl<I, F> Iterator for RestrictedTransitions<I, F>
where
    I: Iterator,
    I::Item: Transition,
    F: Fn(&<I::Item as Trigger>::Q) -> bool,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .find(|item| (self.mask)(item.source()) && (self.mask)(item.target()))
    }
}

impl<'a, TS, F> IntoTransitions for &'a Restricted<TS, F>
where
    TS: IntoTransitions,
    F: Fn(&TS::Q) -> bool,
{
    type TransitionRef = TS::TransitionRef;

    type IntoTransitions = RestrictedTransitions<TS::IntoTransitions, &'a F>;

    fn into_transitions(self) -> Self::IntoTransitions {
        RestrictedTransitions {
            iter: self.on.into_transitions(),
            mask: &self.mask,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{ts::IntoTransitions, Successor, TransitionSystem};

    #[test]
    fn restrict_ts() {
        let original = TransitionSystem::from_iter([
            (0, 'a', 1),
            (0, 'b', 2),
            (1, 'a', 1),
            (1, 'b', 2),
            (2, 'a', 1),
            (2, 'b', 0),
        ]);
        let smaller = original.restrict(|&state| state != 2).into_ts();

        assert_eq!(smaller.size(), 2);
    }
}
