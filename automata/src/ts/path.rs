use itertools::Itertools;

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
/// A path consists of an `origin`, which is simply the index of the state where the path starts. It stores
/// a sequence of transitions and the colors of the states it visits.
#[derive(Clone, PartialEq, Hash)]
pub struct Path<A: Alphabet, Idx, Q, C> {
    end: Idx,
    state_colors: Vec<Q>,
    transitions: Vec<(Idx, A::Symbol, C)>,
}

pub type PathIn<D> = Path<
    <D as TransitionSystem>::Alphabet,
    <D as TransitionSystem>::StateIndex,
    <D as TransitionSystem>::StateColor,
    <D as TransitionSystem>::EdgeColor,
>;

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> Path<A, Idx, Q, C> {
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
    pub fn extend_in<'a, D>(
        &mut self,
        ts: &'a D,
        symbol: SymbolOf<D>,
    ) -> Option<D::TransitionRef<'a>>
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
}

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> Show for Path<A, Idx, Q, C> {
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

pub type LassoIn<D> = Lasso<
    <D as TransitionSystem>::Alphabet,
    <D as TransitionSystem>::StateIndex,
    <D as TransitionSystem>::StateColor,
    <D as TransitionSystem>::EdgeColor,
>;

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> Lasso<A, Idx, Q, C> {
    /// Creates a new [`Lasso`] from the given base/spoke and cycle/recurring [`Path`].
    pub fn new(base: Path<A, Idx, Q, C>, cycle: Path<A, Idx, Q, C>) -> Self {
        Self { base, cycle }
    }

    pub fn recurrent_state_indices(&self) -> impl Iterator<Item = Idx> + '_ {
        self.cycle.state_sequence()
    }

    pub fn recurrent_state_colors(&self) -> impl Iterator<Item = &Q> {
        self.cycle.state_colors()
    }

    pub fn recurrent_edge_colors(&self) -> impl Iterator<Item = &C> {
        self.cycle.edge_colors()
    }

    pub fn into_recurrent_state_indices(self) -> impl Iterator<Item = Idx> {
        self.cycle.into_state_sequence()
    }

    pub fn into_recurrent_state_colors(self) -> impl Iterator<Item = Q> {
        self.cycle.into_state_colors()
    }

    pub fn into_recurrent_edge_colors(self) -> impl Iterator<Item = C> {
        self.cycle.into_edge_colors()
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> Show for Lasso<A, Idx, Q, C> {
    fn show(&self) -> String {
        format!("{}({})", self.base.show(), self.cycle.show())
    }
}
