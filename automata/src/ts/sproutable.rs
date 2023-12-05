use bit_set::BitSet;
use itertools::Itertools;

use crate::{prelude::Simple, Alphabet, Pointed, TransitionSystem};

use super::{transition_system::IsTransition, EdgeColor, StateColor};

pub trait IndexedAlphabet: Alphabet {
    fn symbol_to_index(&self, sym: Self::Symbol) -> usize;
    fn expression_to_index(&self, sym: &Self::Expression) -> usize;
    fn symbol_from_index(&self, index: usize) -> Self::Symbol;
    fn expression_from_index(&self, index: usize) -> Self::Expression;
    fn expression_to_symbol(&self, expression: &Self::Expression) -> Self::Symbol {
        self.symbol_from_index(self.expression_to_index(expression))
    }
    fn symbol_to_expression(&self, symbol: Self::Symbol) -> Self::Expression {
        self.expression_from_index(self.symbol_to_index(symbol))
    }
}

impl IndexedAlphabet for Simple {
    fn symbol_to_index(&self, sym: Self::Symbol) -> usize {
        self.expression_to_index(&sym)
    }

    fn expression_to_index(&self, sym: &Self::Expression) -> usize {
        self.0
            .iter()
            .position(|x| x == sym)
            .expect("Must be present in alphabet")
    }

    fn symbol_from_index(&self, index: usize) -> Self::Symbol {
        self.expression_from_index(index)
    }

    fn expression_from_index(&self, index: usize) -> Self::Expression {
        assert!(index < self.0.len());
        self.0[index]
    }
}

/// Trait for transition systems that allow insertion of states and transitions.
pub trait Sproutable: TransitionSystem {
    /// Creates a new instance of `Self` for the given alphabet.
    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self;

    fn complete_with_colors(&mut self, sink_color: Self::StateColor, edge_color: Self::EdgeColor)
    where
        Self::Alphabet: IndexedAlphabet,
    {
        let sink = self.add_state(sink_color);
        for sym in self.alphabet().universe().collect_vec() {
            self.add_edge(
                sink,
                self.alphabet().symbol_to_expression(sym),
                sink,
                edge_color.clone(),
            );
        }
        let mut seen = BitSet::with_capacity(self.alphabet().size());
        for state in self.state_indices().collect_vec() {
            seen.clear();
            for edge in self.edges_from(state).unwrap() {
                seen.insert(self.alphabet().expression_to_index(edge.expression()));
            }
            for missing in (0..self.alphabet().size()).filter(|i| !seen.contains(*i)) {
                assert!(self
                    .add_edge(
                        state,
                        self.alphabet().expression_from_index(missing),
                        sink,
                        edge_color.clone()
                    )
                    .is_none());
            }
        }
    }

    /// Adds a new state with the given color, returning the index of the newly created state.
    ///
    fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex;
    /// The type of iterator that is returned when calling [`Self::extend_states()`].
    ///
    type ExtendStateIndexIter: IntoIterator<Item = Self::StateIndex>;
    /// For each element that `iter` provides, a new state with the corresponding color is added.
    /// The function returns something which can be turned into an iterator over the indices of
    /// the newly created states.
    ///
    fn extend_states<I: IntoIterator<Item = StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter;
    /// Removes the state with the given index. Note, that this should also remove all transitions
    /// that start or end in the given state. If the no state with the given `index` exists, the
    /// method is a no-op.
    ///
    fn set_state_color<X: Into<StateColor<Self>>>(&mut self, index: Self::StateIndex, color: X);
    /// Sets the state color of the initial state.
    ///
    fn set_initial_color<X: Into<StateColor<Self>>>(&mut self, color: X)
    where
        Self: Pointed,
    {
        self.set_state_color(self.initial(), color);
    }
    /// Adds a new transition from the state `from` to the state `to` on the given expression. If
    /// a transition already exists, the method returns the index of the original target and the
    /// color of the original edge. Otherwise, the method returns `None`.
    ///
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
    fn complete_with_sink(&mut self, sink_color: Self::StateColor) -> Self::StateIndex {
        let _sink = self.add_state(sink_color.clone());

        let _universe = self.alphabet().universe().collect_vec();

        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::ts::{Deterministic, Sproutable, NTS};

    #[test]
    fn complete_ts() {
        let mut partial = NTS::builder()
            .default_color(())
            .extend([
                (0, 'a', 0, 0),
                (0, 'b', 0, 0),
                (0, 'c', 0, 1),
                (1, 'a', 0, 0),
            ])
            .deterministic();
        assert_eq!(partial.reached_from("aaacb", 0), None);
        partial.complete_with_colors((), 2);
        println!(
            "{:?}",
            partial.build_transition_table(|q, c| format!("{q}"))
        );
        for w in ["abbacababcab", "bbcca", "cc", "aababbabbabbccbabba"] {
            assert!(partial.reached_state_index_from(w, 0).unwrap() > 1);
        }
    }
}
