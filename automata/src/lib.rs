//! Library for working with finite automata in Rust.
//!
#![allow(missing_docs)]
#![allow(unused)]
#![allow(clippy::pedantic)]

/// The prelude is supposed to make using this package easier. Including everything, i.e.
/// `use automata::prelude::*;` should be enough to use the package.
pub mod prelude {
    pub use super::{
        alphabet,
        alphabet::{Expression, Simple, Symbol},
        automata::{
            DBALike, DFALike, DPALike, FiniteWordAcceptor, FiniteWordTransformer, IntoMealyMachine,
            IntoMooreMachine, MealyLike, MealyMachine, MooreLike, MooreMachine, NoColor,
            OmegaWordAcceptor, OmegaWordTransformer, StateBasedDBA, StateBasedDPA, WithInitial,
            DBA, DFA, DPA,
        },
        mapping::Morphism,
        ts::{
            dag::Dag,
            finite::ReachedState,
            operations::{Product, ProductIndex},
            predecessors::{IsPreTransition, PredecessorIterable},
            transition_system::{EdgeColorOf, Indexes, IsTransition, StateColorOf},
            Congruence, Deterministic, DeterministicEdgesFrom, EdgeColor, ExpressionOf, HasColor,
            HasColorMut, HasMutableStates, HasStates, IndexType, NTSBuilder, Sproutable,
            StateColor, SymbolOf, ToDot, TransitionSystem, BTS, DTS, NTS,
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
use std::hash::Hash;

/// Module that contains definitions for dealing with lengths. This is particularly
/// useful for dealing with infinite words.
pub use length::{FiniteLength, HasLength, InfiniteLength, Length};

/// This module defines transition systems and successor functions and such.
#[macro_use]
pub mod ts;
pub use ts::{Pointed, TransitionSystem};

/// Defines automata and common types of combinations of transition system with acceptance condition.
#[allow(clippy::upper_case_acronyms)]
pub mod automata;
use automata::{MealyMachine, MooreMachine, DBA, DFA, DPA};

/// Defines congruence relations and congruence classes.
pub mod congruence;
pub use congruence::{Class, RightCongruence};

/// Module that contains definitions for dealing with words.
#[macro_use]
pub mod word;

/// Module that contains definitions for dealing with mappings.
pub mod mapping;

mod algorithms;

#[feature(hoa)]
pub mod hoa;

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

pub trait Show {
    fn show(&self) -> String;
    fn show_collection<'a, I>(iter: I) -> String
    where
        Self: 'a,
        I: IntoIterator<Item = &'a Self>,
        I::IntoIter: DoubleEndedIterator;
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
pub struct Partition<I: Hash + Eq>(Vec<Set<I>>);

impl<I: Hash + Eq> PartialEq for Partition<I> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().all(|o| other.contains(o))
    }
}
impl<I: Hash + Eq> Eq for Partition<I> {}

impl<I: Hash + Eq> Partition<I> {
    /// Builds a new congruence relation from an iterator that yields iterators
    /// which yield elements of type `I`.
    pub fn new<X: IntoIterator<Item = I>, Y: IntoIterator<Item = X>>(iter: Y) -> Self {
        Self(
            iter.into_iter()
                .map(|it| it.into_iter().collect::<Set<_>>())
                .collect(),
        )
    }
}

pub trait Parity {
    fn is_even(&self) -> bool;
    fn is_odd(&self) -> bool {
        !self.is_even()
    }
}
impl<P: Parity> Parity for &P {
    fn is_even(&self) -> bool {
        P::is_even(self)
    }
}
impl Parity for usize {
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
