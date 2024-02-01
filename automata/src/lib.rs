//! Library for working with finite automata in Rust.
//!
//! In essence, an automaton consists of a transition system (TS) together with an acceptance component. A TS is simply a finite collection of states (the set of all states is denoted $Q$) which are connected with directed edges. It can have colors on its states (then each state $q \in Q$ is assigned precisely one color) as well as colors on its edges (meaning every edge between two states has a color).
//!
//! The implementation of TS is generic over the alphabet, which can either be simple (i.e. it is just a collection of individual symbols/chars) or propositional (meaning the alphabet consists of a collection of atomic propositions). Similar to other libraries dealing with (omega) automata, we distinguish between edges and transitions in a TS. Specifically, an edge is determined by its origin/target state, the color it emits and a guard, which is of the expression type that the alphabet determines. A transition on the other hand is a concretization of an edge, where the guard is a single symbol (also determined by the alphabet). For simple alphabets, expressions and symbols coincide, whereas for propositional alphabets, expressions are formulas (represented as BDDs) over the atomic propositions and symbols are satisfying valuations of expressions.
//!
//! The most important trait is [`TransitionSystem`], which provides access to the indices of all states and is capable of returning iterators over the outgoing edges of a state. It also provides a lot of combinators, which allow manipulation of the TS. For example `map_state_color` consumes the TS and relabels the colors on the states through applying a given function. Most combinators consume `self`, returning a new TS, which is mainly to avoid unneccessary cloning of the underlying data structures. If the original TS should continue to exist, call `as_ref` or simply use a reference to the TS.
//! As each combinator returns an object which again implements [`TransitionSystem`], these can easily be chained together without too much overhead. While this is convenient, the applied manipulations are computed on-demand, which may lead to considerable overhead. To circumvent this, it can be beneficial to `collect` the resulting TS into a structure, which then explicitly does all the necessary computations and avoids recomputation at a later point. There are also variants `collect_with_initial`/`collect_ts`, which either take the designated ininital state into account or collect into a specific representation of the TS.
//!
//! The crate defines some basic building blocks of TS which can easily be manipulated (see `Sproutable`), these are
//! - [`NTS`]/[`DTS`] (the latter is just a thin wrapper around the former). These store edges in a vector, a state contains a pointer to the first edge in this collection and each edge contains pointers to the previous/next one.
//! - [`BTS`] which stores transitions in an efficient HashMap
//!
//! Further traits that are of importance are
//! - [`Pointed`] which picks one designated initial state, this is important for deterministic automata
//! - [`Deterministic`], a marker trait that disambiguates between nondeterministic and deterministic TS. As [`TransitionSystem`] only provides iterators over the outgoing edges, it can be used to deal with nondeterministic TS, that have multiple overlapping edges. By implementing `Deterministic`, we guarantee, that there is always a single unique outgoing transition for each state.
//! - [`Sproutable`] enables growing a TS state by state and edge/transition by edge/transition. Naturally, this is only implemented for the basic building blocks, i.e. `BTS`, `DTS` and `NTS`.
#![warn(missing_docs)]
#![allow(unused)]
#![allow(clippy::pedantic)]

/// The prelude is supposed to make using this package easier. Including everything, i.e.
/// `use automata::prelude::*;` should be enough to use the package.
pub mod prelude {
    pub use super::{
        alphabet,
        alphabet::{Expression, Simple, Symbol},
        automaton::{
            DBALike, DFALike, DPALike, FiniteWordAcceptor, FiniteWordTransformer, Initialized,
            IntoMealyMachine, IntoMooreMachine, MealyLike, MealyMachine, MooreLike, MooreMachine,
            NoColor, OmegaWordAcceptor, OmegaWordTransformer, StateBasedDBA, StateBasedDPA, DBA,
            DFA, DPA,
        },
        mapping::Morphism,
        ts::{
            dag::Dag,
            dot::Dottable,
            finite::ReachedState,
            operations::{Product, ProductIndex},
            predecessors::PredecessorIterable,
            transition_system::{EdgeColorOf, EdgeReference, Indexes, IsEdge, StateColorOf},
            Congruence, Deterministic, DeterministicEdgesFrom, EdgeColor, ExpressionOf, HasColor,
            HasColorMut, HasMutableStates, HasStates, IndexType, NTSBuilder, Sproutable,
            StateColor, SymbolOf, TransitionSystem, BTS, DTS, NTS,
        },
        upw,
        word::{FiniteWord, LinearWord, OmegaWord, Periodic, Reduced, ReducedParseError},
        Alphabet, Class, Color, FiniteLength, HasLength, InfiniteLength, Length, Pointed,
        RightCongruence, Show,
    };
}

/// Module that contains definitions for dealing with alphabets.
pub mod alphabet;
pub use alphabet::Alphabet;
use impl_tools::autoimpl;
use itertools::Itertools;

/// Defines lengths of finite and infinite words.
pub mod length;
use std::{collections::BTreeSet, hash::Hash};

/// Module that contains definitions for dealing with lengths. This is particularly
/// useful for dealing with infinite words.
pub use length::{FiniteLength, HasLength, InfiniteLength, Length};

/// This module defines transition systems and successor functions and such.
#[macro_use]
pub mod ts;
pub use ts::{Pointed, TransitionSystem};

/// Defines automata and common types of combinations of transition system with acceptance condition.
#[allow(clippy::upper_case_acronyms)]
pub mod automaton;
use automaton::{MealyMachine, MooreMachine, DBA, DFA, DPA};

/// Defines congruence relations and congruence classes.
pub mod congruence;
pub use congruence::{Class, RightCongruence};

/// Module that contains definitions for dealing with words.
#[macro_use]
pub mod word;

/// Module that contains definitions for dealing with mappings.
pub mod mapping;

/// Contains implementations for different algorithms such as minimization.
pub mod algorithms;

#[feature(hoa)]
pub mod hoa;

/// Implements the generation of random transition systems.
#[feature(random)]
pub mod random;

/// A color is simply a type that can be used to color states or transitions.
pub trait Color: Clone + Eq + Ord + Hash + Show {
    /// Reduces a sequence of colors (of type `Self`) to a single color of type `Self`.
    fn reduce<I: IntoIterator<Item = Self>>(iter: I) -> Self
    where
        Self: Sized,
    {
        iter.into_iter().min().unwrap()
    }
}

impl<T: Eq + Ord + Clone + Hash + Show> Color for T {}

/// Type alias for sets, we use this to hide which type of `HashSet` we are actually using.
pub type Set<S> = fxhash::FxHashSet<S>;
/// Type alias for maps, we use this to hide which type of `HashMap` we are actually using.
pub type Map<K, V> = fxhash::FxHashMap<K, V>;

/// Helper trait which can be used to display states, transitions and such.
pub trait Show {
    /// Returns a human readable representation of `self`, for a state index that should be
    /// for example q0, q1, q2, ... and for a transition (q0, a, q1) it should be (q0, a, q1).
    /// Just use something that makes sense. This is mainly used for debugging purposes.
    fn show(&self) -> String;
    /// Show a collection of the thing, for a collection of states this should be {q0, q1, q2, ...}
    /// and for a collection of transitions it should be {(q0, a, q1), (q1, b, q2), ...}.
    /// By default this is unimplemented.
    fn show_collection<'a, I>(iter: I) -> String
    where
        Self: 'a,
        I: IntoIterator<Item = &'a Self>,
        I::IntoIter: DoubleEndedIterator,
    {
        unimplemented!("This operation makes no sense")
    }
}

impl Show for Option<usize> {
    fn show(&self) -> String {
        match self {
            None => "".to_string(),
            Some(x) => x.show(),
        }
    }

    fn show_collection<'a, I>(iter: I) -> String
    where
        Self: 'a,
        I: IntoIterator<Item = &'a Self>,
        I::IntoIter: DoubleEndedIterator,
    {
        usize::show_collection(iter.into_iter().filter_map(|x| x.as_ref()))
    }
}

impl Show for usize {
    fn show(&self) -> String {
        self.to_string()
    }
    fn show_collection<'a, I: IntoIterator<Item = &'a Self>>(iter: I) -> String
    where
        Self: 'a,
        I::IntoIter: DoubleEndedIterator,
    {
        format!(
            "[{}]",
            itertools::Itertools::join(&mut iter.into_iter().map(|x| x.show()), ", ")
        )
    }
}

impl Show for () {
    fn show(&self) -> String {
        "-".into()
    }
    fn show_collection<'a, I: IntoIterator<Item = &'a Self>>(_iter: I) -> String
    where
        Self: 'a,
        I::IntoIter: DoubleEndedIterator,
    {
        "-".to_string()
    }
}

impl<S: Show> Show for [S] {
    fn show(&self) -> String {
        format!(
            "\"{}\"",
            itertools::Itertools::join(&mut self.iter().map(|x| x.show()), "")
        )
    }

    fn show_collection<'a, I: IntoIterator<Item = &'a Self>>(iter: I) -> String
    where
        Self: 'a,
        I::IntoIter: DoubleEndedIterator,
    {
        format!(
            "{{{}}}",
            itertools::Itertools::join(&mut iter.into_iter().map(|x| x.show()), ", ")
        )
    }
}

impl<S: Show> Show for Vec<S> {
    fn show(&self) -> String {
        S::show_collection(self.iter())
    }

    fn show_collection<'a, I: IntoIterator<Item = &'a Self>>(iter: I) -> String
    where
        Self: 'a,
        I::IntoIter: DoubleEndedIterator,
    {
        unimplemented!()
    }
}

impl<S: Show, T: Show> Show for (S, T) {
    fn show(&self) -> String {
        format!("({}, {})", self.0.show(), self.1.show())
    }

    fn show_collection<'a, I: IntoIterator<Item = &'a Self>>(iter: I) -> String
    where
        Self: 'a,
        I::IntoIter: DoubleEndedIterator,
    {
        todo!()
    }
}

impl Show for bool {
    fn show(&self) -> String {
        match self {
            true => "+",
            false => "-",
        }
        .to_string()
    }

    fn show_collection<'a, I: IntoIterator<Item = &'a Self>>(iter: I) -> String
    where
        Self: 'a,
        I::IntoIter: DoubleEndedIterator,
    {
        format!("{{{}}}", iter.into_iter().map(Show::show).join(", "))
    }
}

/// A partition is a different view on a congruence relation, by grouping elements of
/// type `I` into their respective classes under the relation.
#[derive(Debug, Clone)]
#[autoimpl(Deref using self.0)]
pub struct Partition<I: Hash + Eq>(Vec<BTreeSet<I>>);

impl<'a, I: Hash + Eq> IntoIterator for &'a Partition<I> {
    type Item = &'a BTreeSet<I>;
    type IntoIter = std::slice::Iter<'a, BTreeSet<I>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<I: Hash + Eq> PartialEq for Partition<I> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().all(|o| other.contains(o))
    }
}
impl<I: Hash + Eq> Eq for Partition<I> {}

impl<I: Hash + Eq + Ord> Partition<I> {
    /// Returns the size of the partition, i.e. the number of classes.
    pub fn size(&self) -> usize {
        self.0.len()
    }

    /// Builds a new congruence relation from an iterator that yields iterators
    /// which yield elements of type `I`.
    pub fn new<X: IntoIterator<Item = I>, Y: IntoIterator<Item = X>>(iter: Y) -> Self {
        Self(
            iter.into_iter()
                .map(|it| it.into_iter().collect::<BTreeSet<_>>())
                .collect(),
        )
    }
}

impl<I: Hash + Eq + Ord> From<Vec<BTreeSet<I>>> for Partition<I> {
    fn from(value: Vec<BTreeSet<I>>) -> Self {
        Self(value)
    }
}

/// Captures types that have a parity. This is used for example to determine whether a state
/// is even or odd. We extend this notion to boolean values by assuming that `true` is even
/// and `false` is odd.
pub trait HasParity {
    #[allow(missing_docs)]
    fn is_even(&self) -> bool;
    #[allow(missing_docs)]
    fn is_odd(&self) -> bool {
        !self.is_even()
    }
}
impl<P: HasParity> HasParity for &P {
    fn is_even(&self) -> bool {
        P::is_even(self)
    }
}
impl HasParity for usize {
    fn is_even(&self) -> bool {
        self % 2 == 0
    }
}

#[cfg(test)]
mod tests {
    use crate::{alphabet, prelude::*};

    pub fn wiki_dfa() -> DFA<Simple> {
        let mut dfa = DFA::new(alphabet!(simple 'a', 'b'));
        let a = dfa.initial();
        dfa.set_initial_color(false);
        let b = dfa.add_state(false);
        let c = dfa.add_state(true);
        let d = dfa.add_state(true);
        let e = dfa.add_state(true);
        let f = dfa.add_state(false);

        dfa.add_edge(a, 'a', b, ());
        dfa.add_edge(a, 'b', c, ());
        dfa.add_edge(b, 'a', a, ());
        dfa.add_edge(b, 'b', d, ());
        dfa.add_edge(c, 'a', e, ());
        dfa.add_edge(c, 'b', f, ());
        dfa.add_edge(d, 'a', e, ());
        dfa.add_edge(d, 'b', f, ());
        dfa.add_edge(e, 'a', e, ());
        dfa.add_edge(e, 'b', f, ());
        dfa.add_edge(f, 'a', f, ());
        dfa.add_edge(f, 'b', f, ());

        dfa
    }
}
