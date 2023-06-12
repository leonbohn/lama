use std::{borrow::Borrow, collections::VecDeque, fmt::Display};

use impl_tools::autoimpl;
use itertools::Itertools;
use owo_colors::OwoColorize;
use tabled::{builder::Builder, settings::Style};

use crate::{
    output::Mapping,
    run::{Cane, FailedRun, InducedPath},
    words::{Length, WordInduces},
    Map, Pointed, Set, Str, Subword, Transition, TransitionSystem, Trigger, Word,
};

use super::{
    visit::{tarjan_scc, BfsPaths, Dfs, PathFor, Place},
    Bfs, HasInput, HasStates, InputOf, IntoStates, IntoTransitions, Restricted, StateOf,
    StateReference, TransitionOf, TriggerOf, Visitor, VisitorIter,
};

/// The base trait implemented by a deterministic transition system. A transition system is a tuple `(Q, S, δ)`, where `Q` is a finite set of states, `S` is a finite set of symbols and `δ: Q × S → Q` is a transition function. Note that the transition function is not necessarily complete and some transitions may be missing.
/// States of a transition system are generic, and can be any type that implements the [`StateIndex`] trait.
/// Also the symbols of a transition system are generic, and can be any type that implements the [`Alphabet`] trait.
/// The [`TransitionTrigger`] trait is used to represent an outgoing transition. Note, that such a trigger is a pair consisting of a state and a symbol, meaning the target state is not included in a trigger.
pub trait Successor: HasStates + HasInput {
    /// Returns the successor state of the given state on the given symbol. The transition function is deterministic, meaning that if a transition exists, it is unique. On the other hand there may not be a transition for a given state and symbol, in which case `succ` returns `None`.
    fn successor<X: Borrow<Self::Q>, Y: Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q>;

    /// Runs the given `input` on `self` from the given `origin` state. If the run is successful,
    /// the method returns the induced object (the type of which is determined by the
    /// [`Length`] associated with `input`).
    /// If unsuccessful, it returns a [`FailedRun`], giving access to the partial computation
    /// that was successful as well as to the missing symbol and remaining suffix.
    fn run_from<W>(
        &self,
        input: W,
        origin: Self::Q,
    ) -> Result<WordInduces<W, Self>, FailedRun<Self::Q, W>>
    where
        W: Subword<S = Self::Sigma>,
    {
        let mut cane = Cane::new(input, self, origin);
        cane.result().map(|induced_path| induced_path.induces())
    }

    /// Runs the given `input` word in `self` from the initial state, see also [`run_from`].
    fn run<W>(&self, input: W) -> Result<WordInduces<W, Self>, FailedRun<Self::Q, W>>
    where
        W: Subword<S = Self::Sigma>,
        Self: Sized + Pointed,
    {
        self.run_from(input, self.initial())
    }

    /// Returns the [`Transition`] corresponding to taking the `on`-transition starting in `from`.
    fn transition_for<X: Borrow<Self::Q>, Y: Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<TransitionOf<Self>> {
        let (from, on) = (from.borrow(), on.borrow());
        self.successor(from, on)
            .map(|to| (from.clone(), on.clone(), to))
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
    /// Creates a copy of the current TS which has its initial state set.
    fn start(self, start: Self::Q) -> (Self, Self::Q)
    where
        Self: Sized + Clone,
    {
        (self, start)
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
    fn paths_from<X>(&self, origin: X) -> BfsPaths<&Self>
    where
        Self: Sized,
        X: Borrow<Self::Q>,
    {
        BfsPaths::new_from(self, origin.borrow())
    }

    /// Performs a breadth-first search on the transition system, starting from the initial state, emitting each visited [`Path`].
    fn paths(&self) -> BfsPaths<&Self>
    where
        Self: Pointed + Sized,
    {
        BfsPaths::new_from(self, &self.initial())
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

    /// Returns true if and only if `target` is reachable from `source`.
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

    /// Returns an iterator over all transitions originating from the given state.
    fn transitions_from<'a>(
        &'a self,
        origin: &'a StateOf<Self>,
    ) -> TransitionsForState<'a, Self, itertools::Unique<Self::Input<'a>>>
    where
        Self: Sized,
    {
        TransitionsForState {
            ts: self,
            state: origin,
            alphabet_iter: self.input_alphabet().unique(),
        }
    }
}

impl<TS: Successor + ?Sized> Successor for &TS {
    fn successor<X: Borrow<Self::Q>, Y: Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q> {
        TS::successor(self, from, on)
    }
}

/// Trait that encapsulates the exact opposite to the [`Successor`] trait; it provides access to the predecessors of a state.
/// For now, these are returned just in the form of a [`Set`], but this might change in the future.
pub trait Predecessor: Successor {
    /// Returns the set of all states that have a transition to `from`.
    fn predecessors<X: Borrow<Self::Q>>(&self, from: X) -> Set<&Self::Q>;

    /// Returns the set of all states that can reach `from` via a path of arbitrary length.
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

pub struct TransitionsForState<'a, T: Successor, I> {
    ts: &'a T,
    state: &'a T::Q,
    alphabet_iter: I,
}

impl<'a, T: Successor, I: Iterator<Item = &'a T::Sigma>> Iterator
    for TransitionsForState<'a, T, I>
{
    type Item = TransitionOf<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.alphabet_iter.find_map(|sym| {
            if let Some(successor) = self.ts.successor(self.state, sym) {
                Some(T::make_transition(self.state, sym, &successor))
            } else {
                None
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use tracing_test::traced_test;

    use crate::{ts::IntoStates, Successor, Transition, TransitionSystem, Trigger};

    #[test]
    #[traced_test]
    fn scc_witnesses() {
        let ts = TransitionSystem::from_iter([
            (0u32, 'a', 0),
            (0, 'b', 1),
            (1, 'a', 0),
            (1, 'b', 2),
            (2, 'b', 2),
            (2, 'a', 3),
            (3, 'a', 4),
            (3, 'b', 4),
            (4, 'a', 4),
            (4, 'b', 3),
        ])
        .start(0);

        let scc_transitions = ts.scc_transitions();
        assert_eq!(scc_transitions.len(), 3);

        let scc_for_four = ts.scc_transitions_of(&4);
        assert_eq!(scc_for_four.len(), 4);

        let witness = ts.scc_transitions_witness(&scc_for_four);
        assert!(witness.is_some());

        let (base, recur) = witness.unwrap();
        println!("{}, {}", base, recur);
    }
}
