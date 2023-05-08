use std::{borrow::Borrow, collections::VecDeque, fmt::Display};

use impl_tools::autoimpl;
use owo_colors::OwoColorize;
use tabled::{builder::Builder, settings::Style};

use crate::{
    output::Mapping,
    run::{Configuration, Run},
    Map, Pointed, Set, TransitionSystem, Trigger, Word,
};

use super::{
    visit::{Dfs, LLexPaths, Place},
    Bfs, HasInput, HasStates, InputOf, IntoStates, IntoTransitions, Restricted, StateOf,
    StateReference, TransitionOf, TriggerOf, Visitor, VisitorIter,
};

/// The base trait implemented by a deterministic transition system. A transition system is a tuple `(Q, S, δ)`, where `Q` is a finite set of states, `S` is a finite set of symbols and `δ: Q × S → Q` is a transition function. Note that the transition function is not necessarily complete and some transitions may be missing.
/// States of a transition system are generic, and can be any type that implements the [`StateIndex`] trait.
/// Also the symbols of a transition system are generic, and can be any type that implements the [`Alphabet`] trait.
/// The [`TransitionTrigger`] trait is used to represent an outgoing transition. Note, that such a trigger is a pair consisting of a state and a symbol, meaning the target state is not included in a trigger.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait Successor: HasStates + HasInput {
    /// Returns the successor state of the given state on the given symbol. The transition function is deterministic, meaning that if a transition exists, it is unique. On the other hand there may not be a transition for a given state and symbol, in which case `succ` returns `None`.
    fn successor<X: Borrow<Self::Q>, Y: Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q>;

    fn run_from<W: Word<S = Self::Sigma>, X: Borrow<Self::Q>>(
        &self,
        origin: X,
        input: W,
    ) -> Run<&Self, W>
    where
        Self: Sized,
    {
        Run::new(self, origin, input)
    }

    fn transition_for<X: Borrow<Self::Q>, Y: Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<TransitionOf<Self>> {
        let (from, on) = (from.borrow(), on.borrow());
        self.successor(from, on)
            .map(|to| (from.clone(), on.clone(), to))
    }

    fn run<W: Word<S = Self::Sigma>>(&self, input: W) -> Run<&Self, W>
    where
        Self: Sized + Pointed,
    {
        Run::new(self, self.initial(), input)
    }

    /// Returns the successor state for the given trigger through calling [`Self::succ`].
    fn apply_trigger(&self, trigger: &(Self::Q, Self::Sigma)) -> Option<Self::Q> {
        self.successor(trigger.source(), trigger.sym())
    }

    /// Computes the product with `other` by creating a [`Product`] object. Note that this
    /// is the direct product, where states are pairs of states. This operation is only
    /// possible if `self` and `other` operate on the same symbol.
    /// Additionally, if `self` and `other` both implement [`IntoAssignments`], then the
    /// resulting product does so as well.
    fn product<O>(&self, other: O) -> crate::operations::Product<&Self, O>
    where
        Self: Sized,
        O: Successor<Sigma = Self::Sigma>,
    {
        crate::operations::Product::new(self, other)
    }

    /// Restrict the transition system to only those transitions that satisfy the given predicate.
    fn restrict<F>(&self, filter: F) -> Restricted<&Self, F>
    where
        Self: Sized,
        F: Fn(&Self::Q) -> bool,
    {
        Restricted::new(self, filter)
    }

    /// Creates a new trigger from the given state and symbol.
    fn make_trigger<X: Borrow<StateOf<Self>>, Y: Borrow<InputOf<Self>>>(
        from: X,
        on: Y,
    ) -> TriggerOf<Self> {
        (from.borrow().clone(), on.borrow().clone())
    }

    /// Creates a new transition from the given input data.
    fn make_transition<X: Borrow<StateOf<Self>>, Y: Borrow<InputOf<Self>>>(
        from: X,
        on: Y,
        to: X,
    ) -> TransitionOf<Self> {
        (
            from.borrow().clone(),
            on.borrow().clone(),
            to.borrow().clone(),
        )
    }

    /// Starts a run from the given state. A run is given by a [`Configuration`] object, which keeps track
    /// of the current state.
    fn run_word_from<W: Word<S = Self::Sigma>, X: Borrow<Self::Q>>(
        &self,
        on: W,
        from: X,
    ) -> Configuration<&Self, W>
    where
        Self: Sized,
    {
        Configuration::from_state(self, from.borrow().clone(), on)
    }

    /// Creates a copy of the current TS which has its initial state set.
    fn start(&self, start: Self::Q) -> (Self, Self::Q)
    where
        Self: Sized + Clone,
    {
        (self.clone(), start)
    }

    /// Builds a string representation of the transition table of the transition system.
    /// For this, the [`tabled`] crate is used.
    fn display_transition_table(&self) -> String
    where
        Self::Q: Display,
        Self: IntoStates,
    {
        let mut builder = Builder::default();
        builder.set_header(
            vec!["Deterministic".to_string()].into_iter().chain(
                self.input_alphabet()
                    .map(|s| s.purple().to_string())
                    .collect::<Vec<String>>(),
            ),
        );
        for state in self.into_states() {
            let mut row = vec![state.state().to_string()];
            for sym in self.input_alphabet() {
                row.push(
                    if let Some(successor) = self.successor(state.state(), sym) {
                        successor.to_string()
                    } else {
                        "-".to_string()
                    },
                );
            }
            builder.push_record(row);
        }
        let mut transition_table = builder.build();
        transition_table.with(Style::psql());
        transition_table.to_string()
    }

    /// Performs a breadth-first search on the transition system, starting from the given state.
    fn bfs_from(&self, start: Self::Q) -> Bfs<&Self>
    where
        Self: Sized,
    {
        Bfs::new_from(self, start)
    }

    /// Performs a breadth-first search on the transition system, starting from the initial state.
    fn bfs(&self) -> Bfs<&Self>
    where
        Self: Sized + Pointed,
    {
        Bfs::new(self)
    }

    /// Performs a depth-first search on the states, starting from the given state.
    fn dfs_from(&self, start: Self::Q) -> Dfs<&Self>
    where
        Self: Sized,
    {
        Dfs::new_from(self, start)
    }

    /// Performs a depth-first search on the states, starting from the initial state.
    fn dfs(&self) -> Dfs<&Self>
    where
        Self: Sized + Pointed,
    {
        Dfs::new(self)
    }

    /// Performs a breadth-first search on the transition system, starting from the given state, emitting each visited [`Path`].
    fn all_paths_from<X>(&self, origin: X) -> LLexPaths<&Self>
    where
        Self: Sized,
        X: Borrow<Self::Q>,
    {
        LLexPaths::new_from(self, origin)
    }

    /// Computes the set of all reachable states starting in `origin`. Note that this always includes `origin` itself.
    fn reachable_states_from<X>(&self, origin: X) -> Set<Self::Q>
    where
        Self: Sized,
        X: Borrow<Self::Q>,
    {
        std::iter::once(origin.borrow().clone())
            .chain(
                self.bfs_from(origin.borrow().clone())
                    .iter()
                    .flat_map(|(p, _, q)| [p, q]),
            )
            .collect()
    }

    /// Computes the set of all reachable states from the initial state.
    fn reachable_states(&self) -> Set<<Self as HasStates>::Q>
    where
        Self: Pointed + Sized,
    {
        self.reachable_states_from(self.initial())
    }

    fn can_reach<X: Borrow<Self::Q>, Y: Borrow<Self::Q>>(&self, source: X, target: Y) -> bool
    where
        Self: Sized,
    {
        self.reachable_states_from(source.borrow())
            .contains(target.borrow())
    }

    /// Checks whether `source` lies on a non-trivial cycle. This is done by calling [`Self::reachable_states_from`] and
    /// checking whether `source` is contained in the result.
    fn is_on_cycle<X: Borrow<Self::Q>>(&self, source: X) -> bool
    where
        Self: Sized,
    {
        self.reachable_states_from(source.borrow())
            .iter()
            .skip(1)
            .any(|state| state == source.borrow())
    }

    /// Computes a mapping that assigns to each state in the transition system, an index representing
    /// the strongly connected component that state belongs to. If two states belong to the same
    /// strongly connected component, they will be assigned the same index.
    fn scc_maping(&self) -> Map<Self::Q, usize> {
        let mut out = Map::new();

        out
    }
}

pub trait Predecessor: Successor {
    fn predecessors<X: Borrow<Self::Q>>(&self, from: X) -> Set<&Self::Q>;

    fn reached_by<'a>(&'a self, from: &'a Self::Q) -> Set<&'a Self::Q> {
        let mut seen = Set::from_iter([from]);
        let mut queue = VecDeque::from_iter([from]);

        while let Some(next) = queue.pop_front() {
            for pred in self.predecessors(next) {
                if !seen.contains(pred) {
                    seen.insert(pred);
                    queue.push_back(pred);
                }
            }
        }

        seen
    }
}
