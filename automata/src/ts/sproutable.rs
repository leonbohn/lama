use bit_set::BitSet;
use itertools::Itertools;

use crate::{prelude::CharAlphabet, Alphabet, Bijection, Pointed, TransitionSystem};

use super::{
    transition_system::{Indexes, IsEdge},
    EdgeColor, StateColor,
};

/// Marker trait for [`Alphabet`]s that can be indexed, i.e. where we can associate each
/// [`Alphabet::Symbol`] and [`Alphabet::Expression`] with a unique index (a `usize`).
pub trait IndexedAlphabet: Alphabet {
    /// Turns the given symbol into an index.
    fn symbol_to_index(&self, sym: Self::Symbol) -> usize;
    /// Turns the given expression into an index.
    fn expression_to_index(&self, sym: &Self::Expression) -> usize;
    /// Returns the symbol that corresponds to the given index.
    fn symbol_from_index(&self, index: usize) -> Self::Symbol;
    /// Returns the expression that corresponds to the given index.
    fn expression_from_index(&self, index: usize) -> Self::Expression;
    /// Turns the given expression into a symbol.
    fn expression_to_symbol(&self, expression: &Self::Expression) -> Self::Symbol {
        self.symbol_from_index(self.expression_to_index(expression))
    }
    /// Turns the given symbol into an expression.
    fn symbol_to_expression(&self, symbol: Self::Symbol) -> Self::Expression {
        self.expression_from_index(self.symbol_to_index(symbol))
    }
}

impl IndexedAlphabet for CharAlphabet {
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

    /// Creates a new transition system, by collecting all states and transitions present in `ts`.
    /// This is done by using a naive approach, which simply iterates through all states and adds
    /// them one by one. At the same time, a [bijective mapping](`Bijection`) between old and
    /// new state indices is created. Subequently, the transitions are inserted one by one. Finally,
    /// the newly created transition system is returned together with the bijective state index
    /// mapping.
    ///
    /// Note, that this procedure allows a form of 'downcasting' of edge and state colors. If the
    /// transition system that we want to collect into does not use any edge colors (i.e. the edges
    /// are colored with type [`crate::Void`]) then we simply 'forget' the current colors.
    ///
    /// # Example
    /// ```
    /// use automata::prelude::*;
    ///
    /// let source = TSBuilder::default()
    ///     .with_transitions([(0, 'a', 0, 0), (0, 'b', 0, 0)])
    ///     .with_colors([0])
    ///     .deterministic();
    ///
    /// let (without_edge_colors, _): (DTS<CharAlphabet, usize, Void>, _) = DTS::collect_from(&source);
    /// let (without_state_colors, _): (DTS<CharAlphabet, Void, usize>, _) = DTS::collect_from(&source);
    /// ```
    fn collect_from<Ts>(ts: Ts) -> (Self, Bijection<Ts::StateIndex, Self::StateIndex>)
    where
        Ts: TransitionSystem<Alphabet = Self::Alphabet>,
        StateColor<Ts>: Into<StateColor<Self>>,
        EdgeColor<Ts>: Into<EdgeColor<Self>>,
    {
        let mut out = Self::new_for_alphabet(ts.alphabet().clone());
        let mut map = Bijection::new();
        for index in ts.state_indices() {
            map.insert(
                index,
                out.add_state(
                    ts.state_color(index)
                        .expect("We assume each state to be colored!")
                        .into(),
                ),
            );
        }
        for index in ts.state_indices() {
            let source = *map.get_by_left(&index).unwrap();
            for edge in ts.edges_from(index).expect("State exists") {
                out.add_edge(
                    source,
                    edge.expression().clone(),
                    *map.get_by_left(&edge.target()).unwrap(),
                    edge.color().into(),
                );
            }
        }
        (out, map)
    }

    /// Turns the automaton into a complete one, by adding a sink state and adding transitions
    /// to it from all states that do not have a transition for a given symbol.
    ///
    /// The sink state will be colored with `sink_color` and each newly introduced edge will
    /// be colored with `edge_color`.
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
    /// method may panic or simply print an error!
    /// TODO: Decide on the behavior of this method for states that do not exist.
    fn set_state_color<Idx: Indexes<Self>, X: Into<StateColor<Self>>>(
        &mut self,
        index: Idx,
        color: X,
    );
    /// Sets the state color of the initial state.
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
    fn add_edge<X, Y, CI>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: CI,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Indexes<Self>,
        Y: Indexes<Self>,
        CI: Into<EdgeColor<Self>>;

    /// Removes the transition from the state `from` to the state `to` on the given expression.
    /// Returns `true` if the transition existed and was removed, `false` otherwise.
    fn remove_edges<X: Indexes<Self>>(
        &mut self,
        from: X,
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
            .with_transitions([
                (0, 'a', 0, 0),
                (0, 'b', 0, 0),
                (0, 'c', 0, 1),
                (1, 'a', 0, 0),
            ])
            .deterministic();
        assert_eq!(partial.reached_state_index_from("aaacb", 0), None);
        partial.complete_with_colors((), 2);
        for w in ["abbaccababcab", "bbcca", "cc", "aababbabbabbccbabba"] {
            if partial.reached_state_index_from(w, 0).unwrap() < 1 {
                panic!("Word {} misclassified", w);
            }
        }
    }
}
