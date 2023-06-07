//! Library for working with finite automata in Rust.
//!
//! # Overview
//! This library provides a generic interface for working with finite automata. The overall design is that of a trait hierarchy, where each trait represents a different kind of functionality. The hierarchy is as follows:
//! [`TransitionSystem`] is the root trait, and represents a transition system. A transition system is a set of states, and a set of transitions between states. The states and transitions are generic, and can be any type that implements the [`StateIndex`] and [`TransitionTrigger`] traits, respectively. The [`TransitionSystem`] trait provides methods for querying the transition system, such as [`TransitionSystem::succ`], which returns the successor state of a given state and symbol.
//! A [`StateIndex`] is a type that can be used to uniquely identify a state in a transition system, it defaults to `u32` but can be any other type implementing the required traits.
//! A [`TransitionTrigger`] is a type that can be used to uniquely identify a transition in a transition system, it consists of a [`StateIndex`], the source of the transition, and a symbol, the symbol on which the transition is triggered.
//! Symbols of a transition system are generic, and can be any type that implements the [`Alphabet`] trait. The [`Alphabet`] trait is a marker trait, and is used to indicate that a type can be used as a symbol in a transition system. The [`Alphabet`] trait is implemented for all types that implement `Clone + Eq + std::fmt::Debug + PartialEq`, which is the minimum required for a type to be used as a symbol in a transition system.
//!
//! # Functionality
//! In the following, we give a (not necessarily complete) list of traits that are provided by this library, and describe the functionality they provide.
//! - [`Pointed`]: indicates that a transition system has a single initial state.
//! - [`Growable`]: for growing a transition system. This includes adding states and transitions to the transition system.
//! - [`Shrinkable`]: for shrinking a transition system. This includes removing states and transitions from the transition system.
//! - [`FiniteState`]: for querying the size of a transition system, and enumerating all states in the transition system. This trait should be implemented for all transition systems that are finite.
//! - [`IntoStateReferences`]: for converting a transition system into an iterator over references to the states in the transition system.
// - [`Trimable`]: for trimming a transition system. This corresponds to removing all states which cannot be reached from the initial state. Additionally, every transition that does not have a source or target state in the transition system is removed.

#![warn(missing_docs)]
#![allow(unused)]

/// Module in which traits for working with transition systems are defined. See [`ts::TransitionSystem`] and the crate level documentation for an overview of the trait hierarchy.
/// This module also contains a concrete implementation of a transition system, [`ts::Deterministic`], which stores the transition system as a vector of states, and a vector of transitions. Is only available when the `det` feature is enabled.
#[macro_use]
pub mod ts;
use itertools::Itertools;
use output::{IntoAssignments, Mapping};
pub use ts::{
    AnonymousGrowable, Growable, IntoStateReferences, Pointed, Predecessor, Shrinkable, State,
    Successor, Transition, TransitionIterable, TransitionSystem, Trigger, TriggerIterable,
};
use ts::{InputOf, IntoTransitions, TriggerOf};

pub mod convert;
#[cfg(feature = "hoa")]
pub use convert::{parse_dba, parse_dpa, parse_hoa};

/// Module in which traits for working with words are defined, see [`crate::Word`] for more details.
pub mod words;
pub use words::{Append, PeriodicWord, Prepend, Str, Subword, UltimatelyPeriodicWord, Word};

/// Here, (right) congruence relations and constructs building upon them are introduced based on [`ts::TransitionSystem`].
pub mod congruence;
pub use congruence::{Class, ProgressRightCongruence, RightCongruence, FORC};

/// Module in which acceptance conditions of automata are defined. This includes the [`AcceptanceCondition`] trait, which is implemented by all acceptance conditions, and provides a common interface for working with acceptance conditions.
mod acceptance;
pub use acceptance::{
    AcceptanceCondition, Acceptor, BuchiCondition, OmegaCondition, ParityCondition,
    ReachabilityCondition,
};

use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

mod combined;
pub use combined::Combined;
#[cfg(feature = "det")]
pub use combined::{CongruenceDba, CongruenceDpa, Dba, Dfa, Dpa};
#[cfg(feature = "hoa")]
pub use combined::{HoaDba, HoaDpa};

/// Module in which traits for working with runs of transition systems are defined.
/// A run of a transition system on a given word is a sequence of states, where each state is the successor of the previous state, and the transition between the states is triggered by a symbol as given in the input word.
pub mod run;

/// Module in which traits for working with Mealy machines are defined.
pub mod output;
pub use output::{OutputOf, Priority, Transformer, TransitionOutput};

/// Module in which traits for working with operations on transition systems/automata are defined.
pub mod operations;

/// Module in which traits for working with boundedness of inputs for transition systems are defined.
mod boundedness;
pub use boundedness::{Boundedness, FiniteKind, InfiniteKind};

/// Trait for types that are used as values in a [`Mapping`], for example.
pub trait Value: Clone + Eq + Hash + Debug {}

impl<T> Value for T where T: Clone + Eq + Hash + Debug {}

/// A trait for the symbols of a [`Word`] and the trigger of a transition in a [`TransitionSystem`].
pub trait Symbol: Clone + Eq + std::fmt::Debug + PartialEq + Hash + Display + Ord {}

impl Symbol for usize {}
impl Symbol for u32 {}
impl Symbol for char {}
impl Symbol for i32 {}
impl Symbol for bool {}

#[cfg(feature = "ahash")]
/// Abstracts a mapping, assigning to each element of the domain `X` a value from the codomain `Y`.
pub type Map<X, Y> = ahash::AHashMap<X, Y>;
#[cfg(not(feature = "ahash"))]
pub type Map<X, Y> = std::collections::HashMap<X, Y>;

#[cfg(feature = "ahash")]
/// Abstracts a set of elements of type `X`.
pub type Set<X> = ahash::AHashSet<X>;
#[cfg(not(feature = "ahash"))]
pub type Set<X> = std::collections::AHashSet<X>;

/// Abstracts things that are equivalent, meaning they represent the same thing. Useful for comparing represtantions of words.
pub trait Equivalent<T = Self> {
    /// Returns true if `self` and `other` are equivalent.
    fn equivalent(&self, other: &T) -> bool;
}

/// Pairs of elements of type `L` and `R`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Pair<L, R> {
    /// The left part.
    pub left: L,
    /// The right part.
    pub right: R,
}

impl<L, R> Pair<L, R> {
    /// Creates a new pair.
    pub fn new(left: L, right: R) -> Self {
        Self { left, right }
    }

    /// Obtains a reference to the left stored value.
    pub fn left(&self) -> &L {
        &self.left
    }

    /// Obtains a reference to the right stored value.
    pub fn right(&self) -> &R {
        &self.right
    }

    /// Clones both entries, returning a tuple of elements.
    pub fn raw(&self) -> (L, R)
    where
        L: Clone,
        R: Clone,
    {
        (self.left.clone(), self.right.clone())
    }
}

impl<L, R> Display for Pair<L, R>
where
    L: Display,
    R: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}|{})", self.left, self.right)
    }
}

/// Implemented by things that have an alphabet, i.e. a finite set of symbols. For example a transition
/// system has an alphabet, which is the set of symbols that can be used as triggers for transitions.
/// A word has an alphabet, which is the set of symbols that can be used in the word.
pub trait HasAlphabet {
    /// The type of the symbols in the alphabet.
    type Alphabet: Symbol;

    /// Type of the iterator over the alphabet.
    type AlphabetIter: Iterator<Item = Self::Alphabet>;

    /// Returns the alphabet of `self`.
    fn alphabet(&self) -> Set<Self::Alphabet> {
        self.alphabet_iter().collect()
    }

    /// Returns an iterator over the alphabet of `self`.
    fn alphabet_iter(&self) -> Self::AlphabetIter;

    /// Returns a sorted vector of alphabet symbols.
    fn get_sorted(&self) -> Vec<Self::Alphabet> {
        self.alphabet_iter().sorted().collect()
    }
}

/// An iterator over the alphabet of a transition system.
pub struct AlphabetIter<TS: TriggerIterable> {
    alphabet: Vec<InputOf<TS>>,
    pos: usize,
}

impl<TS: TriggerIterable> Iterator for AlphabetIter<TS> {
    type Item = InputOf<TS>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(result) = self.alphabet.get(self.pos) {
            self.pos += 1;
            Some(result.clone())
        } else {
            None
        }
    }
}

impl<TS: TriggerIterable> HasAlphabet for TS {
    type Alphabet = InputOf<TS>;
    type AlphabetIter = AlphabetIter<TS>;

    fn alphabet_iter(&self) -> Self::AlphabetIter {
        AlphabetIter {
            alphabet: self.triggers_iter().map(|t| t.sym().clone()).collect(),
            pos: 0,
        }
    }
}

mod helpers {
    use crate::{
        output::{IntoAssignments, Mapping, TransformerOutput},
        ts::{HasStates, InputOf, IntoTransitions, StateOf, TriggerOf},
        Combined, OmegaCondition, OutputOf, State, Symbol, Transformer, TransitionSystem, Value,
    };

    /// Trait that is implemented by objects which can be turned into the components that
    /// make up a [`MooreMachine`].
    pub trait IntoMooreTransitions:
        IntoTransitions + IntoAssignments<Domain = StateOf<Self>>
    {
        /// The typ of state.
        type State: State;
        /// The input symbol type.
        type Symbol: Symbol;
        /// The output symbol type.
        type Output: Value;
    }

    impl<X> IntoMooreTransitions for X
    where
        X: IntoTransitions + IntoAssignments<Domain = StateOf<X>>,
    {
        type State = StateOf<X>;

        type Symbol = InputOf<X>;

        type Output = TransformerOutput<Self>;
    }

    /// Trait that is implemented by objects which can be turned into the components that
    /// make up a [`MealyMachine`].
    pub trait IntoMealyTransitions:
        IntoTransitions + IntoAssignments<Domain = (StateOf<Self>, InputOf<Self>)>
    {
        /// The typ of state.
        type State: State;
        /// The input symbol type.
        type Symbol: Symbol;
        /// The output symbol type.
        type Output: Value;
    }

    impl<X> IntoMealyTransitions for X
    where
        X: IntoTransitions + IntoAssignments<Domain = (StateOf<X>, InputOf<X>)>,
    {
        type State = StateOf<X>;

        type Symbol = InputOf<X>;

        type Output = TransformerOutput<Self>;
    }

    /// Represents an automaton with an omega acceptance condition.
    pub type OmegaAutomaton<Q = u32, S = char> =
        Combined<TransitionSystem<Q, S>, OmegaCondition<(Q, S)>>;

    /// Type alias for a (deterministic) automaton.
    pub type Automaton<Acc, Q = u32, S = char> = Combined<TransitionSystem<Q, S>, Acc>;

    /// Type alias for a (deterministic) mealy machine.
    pub type MealyMachine<C = usize, Q = u32, S = char> =
        Combined<TransitionSystem<Q, S>, Mapping<TriggerOf<TransitionSystem<Q, S>>, C>>;

    pub type MooreMachine<C = usize, Q = u32, S = char> =
        Combined<TransitionSystem<Q, S>, Mapping<StateOf<TransitionSystem<Q, S>>, C>>;

    /// Represents a Buchi acceptance condition, which is in essence simply a mapping that assigns to each transition
    /// a boolean value. Here `true` means that the transition is accepting, `false` means that it is not.
    pub type BuchiAcceptance<Q, S> = Mapping<(Q, S), bool>;
    /// Represents a Parity acceptance condition, which assigns to each transition a priority.
    pub type ParityAcceptance<Q, S> = Mapping<(Q, S), usize>;
    /// Assigns to each state a boolean value, indicating whether it is accepting or not.
    pub type ReachabilityAcceptance<Q> = Mapping<Q, bool>;

    /// Represents a deterministic finite automaton.
    pub type DFA<Q = u32, S = char> = MooreMachine<bool, Q, S>;
    /// Represents a deterministic Buchi automaton.
    pub type DBA<Q = u32, S = char> = MealyMachine<bool, Q, S>;
    /// Represents a deterministic parity automaton.
    pub type DPA<Q = u32, S = char> = MealyMachine<usize, Q, S>;
}

pub use helpers::{
    BuchiAcceptance, IntoMealyTransitions, IntoMooreTransitions, MealyMachine, OmegaAutomaton,
    ParityAcceptance, ReachabilityAcceptance, DBA, DFA, DPA,
};

#[cfg(test)]
mod tests {
    pub fn one_mod_three_times_a_dfa() -> DFA {
        DFA::from_parts_iters(
            [
                (0, 'a', 1),
                (0, 'b', 0),
                (1, 'a', 2),
                (1, 'b', 1),
                (2, 'a', 0),
                (2, 'b', 2),
            ],
            [1],
            0,
        )
    }

    pub fn inf_aa_dba() -> DBA {
        DBA::from_iter(
            [
                (0, 'a', 1, false),
                (0, 'b', 0, false),
                (1, 'b', 0, false),
                (1, 'a', 0, true),
            ],
            0,
        )
    }

    use crate::{
        operations::{Intersection, IsEmpty, Negation, Union},
        output::MutableTransformer,
        upw, word, Acceptor, AnonymousGrowable, Growable, PeriodicWord, TransitionSystem, DBA, DFA,
    };

    pub fn simple_ts() -> TransitionSystem {
        let mut ts = TransitionSystem::new();
        let q0 = ts.add_new_state();
        let q1 = ts.add_new_state();

        ts.add_transition(q0, 'a', q1);
        ts.add_transition(q0, 'b', q0);
        ts.add_transition(q1, 'a', q0);
        ts.add_transition(q1, 'b', q1);

        ts
    }

    #[test]
    fn acceptor_test() {
        let dfa = DFA::from_parts_iters(
            vec![(0, 'a', 1), (0, 'b', 0), (1, 'a', 0), (1, 'b', 1)],
            [0],
            0,
        );
        println!("{}", &dfa);

        assert!(dfa.accepts(&word!("aa")));
        assert!(!dfa.accepts(&word!("a")));

        let dba = DBA::from_iter(
            vec![
                (0, 'a', 1, true),
                (0, 'b', 0, false),
                (1, 'a', 0, false),
                (1, 'b', 0, false),
            ],
            0,
        );
        println!("{}", dba);

        assert!(dba.accepts(&upw!("a")));
        assert!(dba.accepts(&upw!("ab")));
        assert!(dba.accepts(&upw!("babbabab")));
        assert!(!dba.accepts(&upw!("b")));
    }

    #[test]
    fn boolean_operations() {
        let mut dfa = one_mod_three_times_a_dfa();
        dfa.set_map(1, false);
        dfa.set_map(2, true);
        let right = one_mod_three_times_a_dfa();

        let union = dfa.union(&right);
        let intersection = dfa.intersection(&right);
        let negation = dfa.complement();

        let should_be_empty = dfa.intersection(&negation);
        assert!(should_be_empty.is_empty());

        let should_be_empty = (dfa.union(&negation)).complement();
        assert!(should_be_empty.is_empty());
    }
}
