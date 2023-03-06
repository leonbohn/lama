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
mod ts;
pub use ts::{
    Deterministic, Growable, IntoStateReferences, Pointed, Shrinkable, StateIndex, StateIterable,
    Transition, TransitionIterable, TransitionSystem, Trigger, TriggerIterable,
};

/// Module in which traits for working with words are defined, see [`crate::Word`] for more details.
pub mod words;
pub use words::{Append, FiniteWord, PeriodicWord, Prepend, Subword, UltimatelyPeriodicWord, Word};

/// Module in which acceptance conditions of automata are defined. This includes the [`AcceptanceCondition`] trait, which is implemented by all acceptance conditions, and provides a common interface for working with acceptance conditions.
mod acceptance;
pub use acceptance::{
    AcceptanceCondition, BuchiCondition, Parity, ParityCondition, ReachabilityCondition,
};

mod acceptor;
pub use acceptor::Acceptor;
use std::hash::Hash;

mod combined;
pub use combined::Combined;
#[cfg(feature = "det")]
pub use combined::{Dba, Dfa, Dpa};

/// Module in which traits for working with runs of transition systems are defined.
/// A run of a transition system on a given word is a sequence of states, where each state is the successor of the previous state, and the transition between the states is triggered by a symbol as given in the input word.
pub mod run;

/// Module in which traits for working with boundedness of inputs for transition systems are defined.
mod boundedness;
pub use boundedness::{Boundedness, FiniteKind, InfiniteKind};

/// A trait for the symbols of a [`Word`] and the trigger of a transition in a [`TransitionSystem`].
pub trait Symbol: Clone + Eq + std::fmt::Debug + PartialEq + Hash {}

impl<C: Clone + Eq + std::fmt::Debug + Hash> Symbol for C {}

#[cfg(feature = "ahash")]
/// Abstracts a mapping, assigning to each element of the domain `X` a value from the codomain `Y`.
pub type Mapping<X, Y> = ahash::AHashMap<X, Y>;
#[cfg(not(feature = "ahash"))]
pub type Mapping<X, Y> = std::collections::HashMap<X, Y>;

#[cfg(feature = "ahash")]
/// Abstracts a set of elements of type `X`.
pub type Set<X> = ahash::AHashSet<X>;
#[cfg(not(feature = "ahash"))]
pub type Set<X> = std::collections::AHashSet<X>;

#[cfg(test)]
mod tests {}
