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

/// Module in which traits for working with transition systems are defined. See [`ts::TransitionSystem`] and the crate level documentation for an overview of the trait hierarchy.
/// This module also contains a concrete implementation of a transition system, [`ts::Deterministic`], which stores the transition system as a vector of states, and a vector of transitions. Is only available when the `det` feature is enabled.
pub mod ts;
use itertools::Itertools;
use ts::SymbolOf;
pub use ts::{
    AnonymousGrowable, Deterministic, Growable, IntoStateReferences, Pointed, Shrinkable,
    StateIndex, Transition, TransitionIterable, TransitionSystem, Trigger, TriggerIterable,
};

mod display;

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

use std::{fmt::Display, hash::Hash};

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
pub use output::{Mapping, OutputOf, Priority, TransitionOutput};

/// Module in which traits for working with operations on transition systems/automata are defined.
pub mod operations;

/// Module in which traits for working with boundedness of inputs for transition systems are defined.
mod boundedness;
pub use boundedness::{Boundedness, FiniteKind, InfiniteKind};

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

#[cfg(feature = "hoa")]
mod hoa;
#[cfg(feature = "hoa")]
pub use hoa::{parse_dba, parse_dpa, parse_hoa};

/// Abstracts things that are equivalent, meaning they represent the same thing. Useful for comparing represtantions of words.
pub trait Equivalent<T = Self> {
    /// Returns true if `self` and `other` are equivalent.
    fn equivalent(&self, other: &T) -> bool;
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
    alphabet: Vec<SymbolOf<TS>>,
    pos: usize,
}

impl<TS: TriggerIterable> Iterator for AlphabetIter<TS> {
    type Item = SymbolOf<TS>;

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
    type Alphabet = SymbolOf<TS>;
    type AlphabetIter = AlphabetIter<TS>;

    fn alphabet_iter(&self) -> Self::AlphabetIter {
        AlphabetIter {
            alphabet: self.triggers_iter().map(|t| t.sym().clone()).collect(),
            pos: 0,
        }
    }
}

/// Represents an automaton with an omega acceptance condition.
pub type OmegaAutomaton<Q = u32, S = char> = Combined<Deterministic<Q, S>, OmegaCondition<(Q, S)>>;

#[cfg(test)]
mod tests {
    use crate::{AnonymousGrowable, Deterministic, Growable};

    pub fn simple_ts() -> Deterministic {
        let mut ts = Deterministic::new();
        let q0 = ts.add_new_state();
        let q1 = ts.add_new_state();

        ts.add_transition(q0, 'a', &q1);
        ts.add_transition(&q0, 'b', &q0);
        ts.add_transition(&q1, 'a', &q0);
        ts.add_transition(&q1, 'b', &q1);

        ts
    }
}
