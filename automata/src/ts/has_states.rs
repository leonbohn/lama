use crate::Color;

use super::{Indexes, State, StateIndex};

pub trait StateColored {
    type StateColor: Color;
    fn state_color(&self, index: StateIndex) -> &Self::StateColor;
}

pub trait HasStates: StateColored + Sized {
    type States<'this>: Iterator<Item = (StateIndex, &'this Self::StateColor)>
    where
        Self: 'this;

    fn states_iter(&self) -> Self::States<'_>;
}
