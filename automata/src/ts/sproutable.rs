use itertools::Itertools;

use crate::{Alphabet, Pointed, TransitionSystem};

use super::{EdgeColor, FiniteState, StateColor};

/// Trait for transition systems that allow insertion of states and transitions.
pub trait Sproutable: TransitionSystem {
    /// Creates a new instance of `Self` for the given alphabet.
    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self;

    /// Adds a new state with the given color, returning the index of the newly created state.
    fn add_state(&mut self, color: StateColor<Self>) -> Self::StateIndex;

    /// Removes the state with the given index. Note, that this should also remove all transitions
    /// that start or end in the given state. If the no state with the given `index` exists, the
    /// method is a no-op.
    fn set_state_color(&mut self, index: Self::StateIndex, color: StateColor<Self>);

    /// Sets the state color of the initial state.
    fn set_initial_color(&mut self, color: StateColor<Self>)
    where
        Self: Pointed,
    {
        self.set_state_color(self.initial(), color);
    }

    /// Adds a new transition from the state `from` to the state `to` on the given expression. If
    /// a transition already exists, the method returns the index of the original target and the
    /// color of the original edge. Otherwise, the method returns `None`.
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

    /// Removes the transition from the state `from` to the state `to` on the given expression.
    /// Returns `true` if the transition existed and was removed, `false` otherwise.
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
        let _sink = self.add_state(sink_color.clone());

        let _universe = self.alphabet().universe().cloned().collect_vec();

        todo!()
    }
}
