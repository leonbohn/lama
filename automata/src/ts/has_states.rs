use crate::Color;

use super::{State, StateIndex};

pub trait StateColor {
    type StateColor: Color;
    fn color(&self, index: StateIndex) -> Option<&Self::StateColor>;
}

pub trait HasStates: StateColor {
    type States<'this>: Iterator<Item = (StateIndex, &'this Self::StateColor)>
    where
        Self: 'this;

    fn states(&self) -> Self::States<'_>;
}
