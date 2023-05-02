use crate::{Growable, Mapping, Pointed, Set, Successor, TransitionSystem};

use super::{HasInput, HasStates, StateOf, SymbolOf};
pub struct Restricted<TS, F> {
    pub(crate) on: TS,
    pub(crate) mask: F,
}

impl<TS, F> Restricted<TS, F> {
    pub fn new(on: TS, mask: F) -> Self {
        Self { on, mask }
    }
}

impl<TS: Successor, F> HasStates for Restricted<TS, F> {
    type Q = StateOf<TS>;
}
impl<TS: Successor, F> HasInput for Restricted<TS, F> {
    type Sigma = SymbolOf<TS>;

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
