use itertools::Itertools;

use crate::{Alphabet, Pointed, TransitionSystem};

use super::{EdgeColor, FiniteState, StateColor};

pub trait Sproutable: TransitionSystem {
    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self;

    fn add_state(&mut self, color: StateColor<Self>) -> Self::StateIndex;

    fn set_state_color(&mut self, index: Self::StateIndex, color: StateColor<Self>);

    fn set_initial_color(&mut self, color: StateColor<Self>)
    where
        Self: Pointed,
    {
        self.set_state_color(self.initial(), color);
    }

    fn add_edge<X, Y>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: EdgeColor<Self>,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Into<Self::StateIndex>,
        Y: Into<Self::StateIndex>;

    fn remove_edge(
        &mut self,
        from: Self::StateIndex,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool;

    /// Turns the automaton into a complete one, by adding a sink state and adding transitions
    /// to it from all states that do not have a transition for a given symbol.
    fn complete_with_sink(&mut self, sink_color: Self::StateColor) -> Self::StateIndex
    where
        Self: FiniteState,
    {
        let sink = self.add_state(sink_color.clone());

        let universe = self.alphabet().universe().cloned().collect_vec();

        todo!()
    }
}
