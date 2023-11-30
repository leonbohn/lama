use crate::{alphabet::Alphabet, Set};

use super::{transition_system::IsTransition, Deterministic, IndexType, TransitionSystem};

/// Represents a path through a transition system. Note, that the path itself is decoupled from the
/// transition system, which allows to use it for multiple transition systems. In particular, it is possible
/// to create a path through some transition system, modify the transition system and then extend the previously
/// created path in the modified transiton system.
///
/// A path consists of an `origin`, which is simply the index of the state where the path starts. It stores
/// a sequence of transitions and the colors of the states it visits.
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Path<A: Alphabet, Idx> {
    end: Idx,
    transitions: Vec<(Idx, A::Expression)>,
}

impl<A: Alphabet, Idx: IndexType> Path<A, Idx> {
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

    /// Creates a looping path by pointing the last transition to the given `position`.
    pub fn loop_back_to(self, position: usize) -> Lasso<A, Idx>
    where
        Idx: IndexType,
    {
        debug_assert!(position < self.len());
        debug_assert!(self.end == self.transitions[position].0);

        Lasso::new(
            Path::new(
                self.transitions[position].0,
                self.transitions[..position].to_vec(),
            ),
            Path::new(self.end, self.transitions[position..].to_vec()),
        )
    }

    /// Returns an iterator over all colors which appear on an edge taken by the path.
    pub fn edge_colors<'a, TS>(&'a self, ts: &'a TS) -> impl Iterator<Item = TS::EdgeColor> + 'a
    where
        TS: Deterministic<Alphabet = A, StateIndex = Idx>,
        Idx: IndexType,
        TS::EdgeColor: Clone,
    {
        self.transitions
            .iter()
            .map(move |(source, sym)| ts.edge_color(*source, sym).expect("These transitions must exist, otherwise the path cannot have been built with a ts that is consistent with the given one."))
    }

    /// Returns the color of the state that is reached by the path.
    pub fn reached_state_color<'a, TS>(&'a self, ts: &'a TS) -> TS::StateColor
    where
        TS: Deterministic<Alphabet = A, StateIndex = Idx>,
        Idx: IndexType,
        TS::StateColor: Clone,
    {
        ts.state_color(self.reached())
            .expect("We assume every state to be colored")
    }

    /// Returns the color of the last transition if `self` is viewed as a path in the given `ts`, if it exists.
    /// If the path is empty or not contiguous in `ts`, `None` is returned.
    pub fn last_transition_color<'a, TS>(&'a self, ts: &'a TS) -> Option<TS::EdgeColor>
    where
        TS: Deterministic<Alphabet = A, StateIndex = Idx>,
        Idx: IndexType,
        TS::EdgeColor: Clone,
    {
        self.transitions
            .last()
            .and_then(|t| ts.edge_color(t.0, &t.1))
    }

    /// Gives an iterator over all colors of the states visited by the path.
    pub fn state_colors<'a, TS>(&'a self, ts: &'a TS) -> impl Iterator<Item = TS::StateColor> + 'a
    where
        TS: Deterministic<Alphabet = A, StateIndex = Idx>,
        Idx: IndexType,
        TS::StateColor: Clone,
    {
        self.state_sequence().map(move |state| {
            ts.state_color(state)
                .expect("Something must have gone wrong, every state should have a color!")
        })
    }

    /// Returns true if the path is empty/trivial, meaning it consists of only one state.
    pub fn empty(state: Idx) -> Self {
        Self {
            end: state,
            transitions: Vec::new(),
        }
    }

    pub fn with_capacity(state: Idx, capacity: usize) -> Self {
        Self {
            end: state,
            transitions: Vec::with_capacity(capacity),
        }
    }

    /// Creates a new path with the given `state` as origin and the given `transitions`.
    pub fn new(state: Idx, transitions: Vec<(Idx, A::Expression)>) -> Self {
        Self {
            end: state,
            transitions,
        }
    }

    /// Attempts to extend the path in the given `ts` by the given `symbol`. If the path can be extended,
    /// the transition is returned. Otherwise, `None` is returned.
    pub fn extend_in<'a, Ts>(
        &mut self,
        ts: &'a Ts,
        symbol: A::Symbol,
    ) -> Option<Ts::TransitionRef<'a>>
    where
        Idx: IndexType,
        Ts: Deterministic<Alphabet = A, StateIndex = Idx>,
    {
        let transition = ts.transition(self.end, symbol)?;
        self.transitions
            .push((self.end, transition.expression().clone()));
        self.end = transition.target();
        Some(transition)
    }

    /// Extends self with the given `other` path.
    pub fn extend_with(&mut self, other: Path<A, Idx>) {
        assert_eq!(self.reached(), other.origin(), "Start and end must match!");
        self.transitions.extend(other.transitions);
        self.end = other.end;
    }

    /// Returns an iterator over the indices of the states visited by the path.
    pub fn state_sequence(&self) -> impl Iterator<Item = Idx> + '_
    where
        Idx: IndexType,
    {
        self.transitions
            .iter()
            .map(|(source, _)| *source)
            .chain(std::iter::once(self.end))
    }
}

/// A lasso represents an infinite path, which after it ends loops back to some previous position.
#[derive(Debug, Clone)]
pub struct Lasso<A: Alphabet, Idx> {
    base: Path<A, Idx>,
    cycle: Path<A, Idx>,
}

impl<A: Alphabet, Idx> Lasso<A, Idx> {
    /// Creates a new [`Lasso`] from the given base/spoke and cycle/recurring [`Path`].
    pub fn new(base: Path<A, Idx>, cycle: Path<A, Idx>) -> Self {
        Self { base, cycle }
    }

    /// Returns the infinity set of `self` in the given [`TransitionSystem`], i.e. the set of (edge) colors
    /// that is visited infinitely often.
    pub fn infinity_set<Ts>(self, ts: Ts) -> Set<Ts::EdgeColor>
    where
        Ts: Deterministic<Alphabet = A, StateIndex = Idx>,
        Idx: IndexType,
    {
        self.cycle.edge_colors(&ts).collect()
    }

    pub fn recurrent_state_indices(self) -> Set<Idx>
    where
        Idx: IndexType,
    {
        self.cycle.state_sequence().collect()
    }

    pub fn recurrent_state_colors<Ts>(self, ts: &Ts) -> Set<Ts::StateColor>
    where
        Ts: Deterministic<Alphabet = A, StateIndex = Idx>,
        Idx: IndexType,
    {
        self.cycle.state_colors(ts).collect()
    }
}
