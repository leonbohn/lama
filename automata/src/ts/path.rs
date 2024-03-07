use std::fmt::Debug;

use itertools::{Either, Itertools};

use crate::{alphabet::Alphabet, Color, Set, Show};

use super::{
    transition_system::{Indexes, IsEdge},
    Deterministic, ExpressionOf, IndexType, SymbolOf, TransitionSystem,
};

/// Represents a path through a transition system. Note, that the path itself is decoupled from the
/// transition system, which allows to use it for multiple transition systems. In particular, it is possible
/// to create a path through some transition system, modify the transition system and then extend the previously
/// created path in the modified transiton system.
///
/// We represent a path as a sequence of transitions, where each transition is a tuple of the source state,
/// the symbol that is taken and the color of the transition. The path also stores the colors of all states
/// that are visited by the path. Finally, we store the index of the state that is reached by the path separately.
/// This is done to allow for efficient extension of the path. Also we know that there can be no empty paths.
#[derive(Clone, PartialEq, Hash)]
pub struct Path<A: Alphabet, Idx, Q, C> {
    end: Idx,
    state_colors: Vec<Q>,
    transitions: Vec<(Idx, A::Symbol, C)>,
}

/// Type alias that given a [`TransitionSystem`] `D` creates a [`Path`] in `D`.
pub type PathIn<D> = Path<
    <D as TransitionSystem>::Alphabet,
    <D as TransitionSystem>::StateIndex,
    <D as TransitionSystem>::StateColor,
    <D as TransitionSystem>::EdgeColor,
>;

impl<A: Alphabet, Idx: IndexType, Q: Clone, C: Clone> Path<A, Idx, Q, C> {
    /// Builds a new path from its parts, which are the index of the state it ends in, the sequence of
    /// state colors and the setquence of transitions.
    pub fn from_parts(
        end: Idx,
        state_colors: Vec<Q>,
        transitions: Vec<(Idx, A::Symbol, C)>,
    ) -> Self {
        Self {
            end,
            state_colors,
            transitions,
        }
    }

    /// Returns the index of the state that is reached by the path.
    pub fn reached(&self) -> Idx {
        self.end
    }

    /// Gives the origin of the path, which is the index of the state the path starts in.
    pub fn origin(&self) -> Idx {
        if !self.transitions.is_empty() {
            self.transitions[0].0
        } else {
            self.end
        }
    }

    /// Returns true if the path is empty/trivial, meaning it consists of only one state.
    pub fn is_empty(&self) -> bool {
        self.transitions.is_empty()
    }

    /// Returns the length of the path.
    pub fn len(&self) -> usize {
        self.transitions.len()
    }

    /// Returns the color of the state that is reached by the path.
    pub fn reached_state_color(&self) -> Q {
        assert!(!self.state_colors.is_empty());
        self.state_colors
            .last()
            .cloned()
            .expect("At least one color must exist")
    }

    /// Returns the color of the last transition if `self` is viewed as a path in the given `ts`, if it exists.
    /// If the path is empty or not contiguous in `ts`, `None` is returned.
    pub fn last_transition_color(&self) -> Option<&C> {
        self.transitions.last().map(|t| &t.2)
    }

    /// Gives an iterator over all colors of the states visited by the path.
    pub fn state_colors(&self) -> impl Iterator<Item = &Q> + '_ {
        self.state_colors.iter()
    }

    /// Consumes `self` and returns an iterator over all colors of the states visited by the path. May contain ducplicates.
    pub fn into_state_colors(self) -> impl Iterator<Item = Q> {
        self.state_colors.into_iter()
    }

    /// Returns true if the path is empty/trivial, meaning it consists of only one state.
    pub fn empty_in<
        D: Deterministic<StateColor = Q, EdgeColor = C, StateIndex = Idx, Alphabet = A>,
    >(
        ts: D,
        state: Idx,
    ) -> Self {
        let origin = state
            .to_index(&ts)
            .expect("Path must start in state that exists");
        Self {
            state_colors: vec![ts
                .state_color(origin)
                .expect("Origin state must be colored")],
            end: origin,
            transitions: Vec::new(),
        }
    }

    /// Creates a new empty path that starts in the given `state` and has the given `capacity`.
    pub fn empty_in_with_capacity<D>(ts: D, state: Idx, capacity: usize) -> Self
    where
        D: Deterministic<StateColor = Q, EdgeColor = C, StateIndex = Idx, Alphabet = A>,
    {
        let origin = state
            .to_index(&ts)
            .expect("Path must start in state that exists");
        Self {
            state_colors: vec![ts
                .state_color(origin)
                .expect("Origin state must be colored")],
            end: origin,
            transitions: Vec::with_capacity(capacity),
        }
    }

    /// Attempts to extend the path in the given `ts` by the given `symbol`. If the path can be extended,
    /// the transition is returned. Otherwise, `None` is returned.
    pub fn extend_in<'a, D>(&mut self, ts: &'a D, symbol: SymbolOf<D>) -> Option<D::EdgeRef<'a>>
    where
        D: Deterministic<StateColor = Q, EdgeColor = C, StateIndex = Idx, Alphabet = A>,
    {
        let transition = ts.transition(self.end, symbol)?;
        self.transitions
            .push((self.end, symbol, transition.color().clone()));
        self.end = transition.target();
        self.state_colors
            .push(ts.state_color(self.end).expect("The state must be colored"));
        Some(transition)
    }

    /// Extends self with the given `other` path.
    pub fn extend_with(&mut self, other: Path<A, Idx, Q, C>) {
        assert_eq!(self.reached(), other.origin(), "Start and end must match!");
        self.transitions.extend(other.transitions);
        self.state_colors
            .extend(other.state_colors.into_iter().skip(1));

        assert_eq!(self.state_colors.len() - 1, self.transitions.len());
        self.end = other.end;
    }

    /// Returns an iterator over the indices of the states visited by the path.
    pub fn state_sequence(&self) -> impl Iterator<Item = Idx> + '_ {
        self.transitions
            .iter()
            .map(|(source, _, _)| *source)
            .chain(std::iter::once(self.end))
    }

    /// Consumes `self` and returns an iterator over the indices of the states visited by the path. May contain duplicates.
    pub fn into_state_sequence(self) -> impl Iterator<Item = Idx> {
        self.transitions
            .into_iter()
            .map(|(source, _, _)| source)
            .chain(std::iter::once(self.end))
    }

    /// Returns an iterator over all colors which appear on an edge taken by the path.
    pub fn edge_colors(&self) -> impl Iterator<Item = &C> + '_ {
        self.transitions.iter().map(|(source, sym, c)| c)
    }

    /// Consumes `self` and returns an iterator over all colors which appear on an edge taken by the path. May contain duplicates.
    pub fn into_edge_colors(self) -> impl Iterator<Item = C> {
        self.transitions.into_iter().map(|(p, a, c)| c)
    }

    /// Creates a looping path by pointing the last transition to the given `position`.
    pub fn loop_back_to(self, position: usize) -> Lasso<A, Idx, Q, C> {
        debug_assert!(position < self.len());
        debug_assert!(self.end == self.transitions[position].0);

        Lasso::new(
            Path::from_parts(
                self.transitions[position].0,
                self.state_colors[..=position].to_vec(),
                self.transitions[..position].to_vec(),
            ),
            Path::from_parts(
                self.end,
                self.state_colors[position..].to_vec(),
                self.transitions[position..].to_vec(),
            ),
        )
    }

    /// Returns an iterator over the transitions in the form (src, symbol, target, color)
    pub fn transitions(&self) -> impl Iterator<Item = Edge<A::Symbol, Idx, C>> + '_ {
        if let Some(last) = self.transitions.last() {
            Either::Left(
                self.transitions
                    .iter()
                    .tuple_windows::<(_, _)>()
                    .map(|((q0, a0, c0), (q1, _, _))| Edge::new(*q0, *a0, c0.clone(), *q1))
                    .chain(std::iter::once(Edge::new(
                        last.0,
                        last.1,
                        last.2.clone(),
                        self.end,
                    ))),
            )
        } else {
            Either::Right(std::iter::empty())
        }
    }

    /// Consumes `self` and returns an iterator over the transitions in the form (src, symbol, target, color)
    pub fn into_transitions(self) -> impl Iterator<Item = Edge<A::Symbol, Idx, C>> {
        if let Some(last) = self.transitions.last().cloned() {
            Either::Left(
                self.transitions
                    .into_iter()
                    .tuple_windows::<(_, _)>()
                    .map(|((q0, a0, c0), (q1, _, _))| Edge::new(q0, a0, c0, q1))
                    .chain(std::iter::once(Edge::new(last.0, last.1, last.2, self.end))),
            )
        } else {
            Either::Right(std::iter::empty())
        }
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Show, C: Show> std::fmt::Debug for Path<A, Idx, Q, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.show())
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Show, C: Show> Show for Path<A, Idx, Q, C> {
    fn show(&self) -> String {
        format!(
            "{}{}",
            self.transitions
                .iter()
                .map(|(p, a, c)| format!("{} -{}|{}-> ", p.show(), a.show(), c.show()))
                .join(""),
            self.end.show()
        )
    }
}

/// A lasso represents an infinite path, which after it ends loops back to some previous position.
#[derive(Clone)]
pub struct Lasso<A: Alphabet, Idx, Q, C> {
    base: Path<A, Idx, Q, C>,
    cycle: Path<A, Idx, Q, C>,
}

/// Type alias that given a [`TransitionSystem`] `D` creates a [`Lasso`] in `D`.
pub type LassoIn<D> = Lasso<
    <D as TransitionSystem>::Alphabet,
    <D as TransitionSystem>::StateIndex,
    <D as TransitionSystem>::StateColor,
    <D as TransitionSystem>::EdgeColor,
>;

impl<A: Alphabet, Idx: IndexType, Q: Clone, C: Clone> Lasso<A, Idx, Q, C> {
    /// Creates a new [`Lasso`] from the given base/spoke and cycle/recurring [`Path`].
    pub fn new(base: Path<A, Idx, Q, C>, cycle: Path<A, Idx, Q, C>) -> Self {
        Self { base, cycle }
    }

    /// Gives an iterator over the state indices that appear in the cycle of the lasso. These
    /// are precisely the states that appear infinitely often. May contain duplicates.
    pub fn recurrent_state_indices(&self) -> impl Iterator<Item = Idx> + '_ {
        self.cycle.state_sequence()
    }

    /// Gives the colors of states along the cycle, so colors that appear infinitely often. May contain duplicates.
    pub fn recurrent_state_colors(&self) -> impl Iterator<Item = &Q> {
        self.cycle.state_colors()
    }

    /// Gives the colors of edges along the loop, so colors that appear infinitely often. May contain duplicates.
    pub fn recurrent_edge_colors(&self) -> impl Iterator<Item = &C> {
        self.cycle.edge_colors()
    }

    /// Gives the transitions along the loop, so transitions that appear infinitely often
    /// as tuples of the form (src, smbol, target, color)
    pub fn recurrent_transitions(&self) -> impl Iterator<Item = Edge<A::Symbol, Idx, C>> + '_ {
        self.cycle.transitions()
    }

    /// Consumes `self` and gives an iterator over the state indices that appear in the cycle of the lasso. These
    /// are precisely the states that appear infinitely often. May contain duplicates.
    pub fn into_recurrent_state_indices(self) -> impl Iterator<Item = Idx> {
        self.cycle.into_state_sequence()
    }

    /// Consumes `self` and gives the colors of states along the cycle, so colors that appear infinitely often. May contain duplicates.
    pub fn into_recurrent_state_colors(self) -> impl Iterator<Item = Q> {
        self.cycle.into_state_colors()
    }

    /// Consumes `self` and gives the colors of edges along the loop, so colors that appear infinitely often. May contain duplicates.
    pub fn into_recurrent_edge_colors(self) -> impl Iterator<Item = C> {
        self.cycle.into_edge_colors()
    }

    /// Consumes `self` and gives the transitions along the loop, so transitions that appear infinitely often
    /// as tuples of the form (src, smbol, target, color)
    pub fn into_recurrent_transitions(self) -> impl Iterator<Item = Edge<A::Symbol, Idx, C>> {
        self.cycle.into_transitions()
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> Show for Lasso<A, Idx, Q, C> {
    fn show(&self) -> String {
        format!("{}({})", self.base.show(), self.cycle.show())
    }
}

/// Represents an edge that is not associated to a transition system. It stores a color, an
/// expression, as well as a source and target state index.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Edge<E, Idx, C> {
    source: Idx,
    target: Idx,
    color: C,
    expression: E,
}

impl<'a, E, Idx: Copy, C: Clone> IsEdge<'a, E, Idx, C> for &'a Edge<E, Idx, C> {
    fn target(&self) -> Idx {
        self.target
    }

    fn color(&self) -> C {
        self.color.clone()
    }

    fn expression(&self) -> &'a E {
        &self.expression
    }

    fn source(&self) -> Idx {
        self.source
    }
}

impl<E, Idx, C> Edge<E, Idx, C> {
    /// Creates a new edge with the given source, expression, color and target.
    pub fn new(source: Idx, expression: E, color: C, target: Idx) -> Self {
        Self {
            source,
            target,
            color,
            expression,
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::alphabet::CharAlphabet;
    use crate::ts::{path::Edge, Path};

    #[test]
    fn path_transitions() {
        // make path
        let path: Path<CharAlphabet, usize, bool, bool> = Path::from_parts(
            0,
            vec![true, false, true],
            vec![(0, 'a', false), (1, 'b', true), (2, 'c', false)],
        );
        let transitions = vec![
            Edge::new(0, 'a', false, 1),
            Edge::new(1, 'b', true, 2),
            Edge::new(2, 'c', false, 0),
        ];

        assert!(path.transitions().eq(transitions));
    }
}
