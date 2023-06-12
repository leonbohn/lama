/// Contains the code that allows conversion of objects to a [Graphviz DOT representation](https://graphviz.org/doc/info/lang.html).
pub mod dot;
mod fmt;

pub use dot::ToDot;
use std::hash::Hash;
use std::{fmt::Display, ops::Rem};

#[cfg(feature = "hoa")]
mod hoa;
#[cfg(feature = "hoa")]
pub use hoa::{parse_dba, parse_dpa, parse_hoa};
use owo_colors::OwoColorize;

use crate::Transition;
use crate::{
    congruence::CongruenceTrigger, output::Mapping, BuchiCondition, Class, Map, OmegaCondition,
    ParityCondition, Successor, Symbol, Transformer, Value,
};

/// Implementors of this trait possess the ability to annotate a transition, i.e. to endow
/// it with additional information (like an associated priority). This happens at the
/// edge and not at the target level.
pub trait AnnotatesTransition<X, Y> {
    /// Performs the annotation of a transition.
    fn annotate_transition(&self, x: &(X, Y), z: &X) -> String;
}

/// Implementors can annotate transitions, but on the target level not on the edge level.
pub trait AnnotatesTransitionTarget<X, Y> {
    /// Produces the target annotation.
    fn annotate_transition_target<T: Transition<Q = X, S = Y>>(&self, transition: T) -> String;
}

/// Abstracts the ability of annotating a state directly, for example with a flag indicating
/// whether or not it is accepting (in a DFA).
pub trait AnnotatesState<X> {
    /// Produces a state-level annotation for the given state.
    fn annotate_state(&self, x: &X) -> String;
}

/// Implementors have the ability of displaying their states.
pub trait DisplayState: Successor {
    /// Display the given state, i.e. turn it into a string representation.
    fn display_state(&self, state: &Self::Q) -> String;
}

/// Implementors can display [`Symbol`]s, i.e. transition labels.
pub trait DisplaySymbol: Successor {
    /// Display the transition label.
    fn display_symbol(&self, sym: &Self::Sigma) -> String;
}

impl<X, Y> AnnotatesTransition<X, Y> for BuchiCondition<(X, Y)>
where
    X: Eq + Hash + Display,
    Y: Eq + Hash + Display,
{
    fn annotate_transition(&self, x: &(X, Y), z: &X) -> String {
        format!(
            "{}@{}",
            x.1,
            match self.0.contains(x) {
                true => 0,
                false => 1,
            }
        )
    }
}

impl<X, Y> AnnotatesState<X> for BuchiCondition<(X, Y)>
where
    X: Eq + Hash + Display,
    Y: Eq + Hash + Display,
{
    fn annotate_state(&self, x: &X) -> String {
        x.to_string()
    }
}

impl<X, Y> AnnotatesState<X> for ParityCondition<(X, Y)>
where
    X: Eq + Hash + Display,
    Y: Eq + Hash + Display,
{
    fn annotate_state(&self, x: &X) -> String {
        x.to_string()
    }
}

impl<I: Symbol, O> AnnotatesState<Class<I>> for Map<CongruenceTrigger<I>, O> {
    fn annotate_state(&self, x: &Class<I>) -> String {
        x.to_string()
    }
}

impl<X, Y> AnnotatesTransition<X, Y> for ParityCondition<(X, Y)>
where
    X: Eq + Hash + Display,
    Y: Eq + Hash + Display,
{
    fn annotate_transition(&self, x: &(X, Y), z: &X) -> String {
        // if none is assigned, we take the highest value
        let priority = (0..self.0.len())
            .find(|i| self.0[*i].contains(x))
            .unwrap_or(self.0.len());
        format!("{}@{}", z, priority)
    }
}

impl<X, Y> AnnotatesTransitionTarget<X, Y> for BuchiCondition<(X, Y)>
where
    X: Value + Display,
    Y: Value + Display,
{
    fn annotate_transition_target<T: Transition<Q = X, S = Y>>(&self, transition: T) -> String {
        format!(
            "{}@{}",
            transition.target(),
            match self
                .0
                .contains(&(transition.source().clone(), transition.sym().clone()))
            {
                true => 0,
                false => 1,
            }
        )
    }
}

impl<X, Y> AnnotatesTransitionTarget<X, Y> for ParityCondition<(X, Y)>
where
    X: Value + Display,
    Y: Value + Display,
{
    fn annotate_transition_target<T: Transition<Q = X, S = Y>>(&self, transition: T) -> String {
        // if none is assigned, we take the highest value
        let priority = (0..self.0.len())
            .find(|i| self.0[*i].contains(&(transition.source().clone(), transition.sym().clone())))
            .unwrap_or(self.0.len());
        format!("{}@{}", transition.target(), priority)
    }
}

impl<X, Y, O> AnnotatesTransitionTarget<X, Y> for Mapping<(X, Y), O>
where
    (X, Y): Value,
    Y: Display + Clone,
    X: Display + Clone,
    O: Display + Value,
{
    fn annotate_transition_target<T: Transition<Q = X, S = Y>>(&self, transition: T) -> String {
        format!(
            "{}@{}",
            transition.target(),
            self.apply((transition.source().clone(), transition.sym().clone()))
        )
    }
}

impl<X, Y> AnnotatesTransitionTarget<X, Y> for Mapping<X, bool>
where
    X: Value + Display,
    Y: Value,
{
    fn annotate_transition_target<T: Transition<Q = X, S = Y>>(&self, transition: T) -> String {
        format!("{}", transition.target())
    }
}

impl<X, Y, Z> AnnotatesTransitionTarget<X, Y> for Map<(X, Y), Z>
where
    (X, Y): Eq + Hash,
    Y: Display + Value,
    X: Display + Value,
    Z: Display,
{
    fn annotate_transition_target<T: Transition<Q = X, S = Y>>(&self, transition: T) -> String {
        match self.get(&(transition.source().clone(), transition.sym().clone())) {
            Some(v) => format!("{}@{}", transition.target(), v),
            None => transition.target().to_string(),
        }
    }
}

impl<X, Y> AnnotatesTransitionTarget<X, Y> for OmegaCondition<(X, Y)>
where
    X: Value + Display,
    Y: Value + Display,
{
    fn annotate_transition_target<T: Transition<Q = X, S = Y>>(&self, transition: T) -> String {
        match self {
            OmegaCondition::Parity(c) => c.annotate_transition_target(transition),
            OmegaCondition::Buchi(c) => c.annotate_transition_target(transition),
        }
    }
}

impl<X, Y> AnnotatesState<X> for OmegaCondition<(X, Y)>
where
    X: Eq + Hash + Display,
    Y: Eq + Hash + Display,
{
    fn annotate_state(&self, x: &X) -> String {
        x.to_string()
    }
}

impl<X, Y> AnnotatesTransition<X, Y> for OmegaCondition<(X, Y)>
where
    X: Eq + Hash + Display,
    Y: Eq + Hash + Display,
{
    fn annotate_transition(&self, x: &(X, Y), z: &X) -> String {
        match self {
            OmegaCondition::Parity(c) => c.annotate_transition(x, z),
            OmegaCondition::Buchi(c) => c.annotate_transition(x, z),
        }
    }
}

impl<TS> DisplayState for TS
where
    TS: Successor,
    TS::Q: Display,
{
    fn display_state(&self, state: &Self::Q) -> String {
        format!("{}", state)
    }
}

impl<TS: Successor> DisplaySymbol for TS {
    fn display_symbol(&self, sym: &Self::Sigma) -> String {
        format!("{}", sym)
    }
}

impl<X, Y, Z> AnnotatesTransition<X, Y> for Map<(X, Y), Z>
where
    (X, Y): Eq + Hash,
    Y: Display,
    X: Display,
    Z: Display,
{
    fn annotate_transition(&self, x: &(X, Y), z: &X) -> String {
        match self.get(x) {
            Some(v) => format!("{}@{}", z, v),
            None => z.to_string(),
        }
    }
}

impl<X, Y, O> AnnotatesTransition<X, Y> for Mapping<(X, Y), O>
where
    (X, Y): Value,
    Y: Display,
    X: Display,
    O: Display + Value,
{
    fn annotate_transition(&self, x: &(X, Y), z: &X) -> String {
        format!("{}@{}", z, self.apply(x))
    }
}

impl<X, Y, O> AnnotatesState<X> for Mapping<(X, Y), O>
where
    (X, Y): Value,
    Y: Display,
    X: Display,
    O: Display + Value,
{
    fn annotate_state(&self, x: &X) -> String {
        x.to_string()
    }
}

impl<X, Y> AnnotatesTransition<X, Y> for Mapping<X, bool>
where
    X: Value,
    Y: Display,
    X: Display,
{
    fn annotate_transition(&self, _x: &(X, Y), z: &X) -> String {
        format!("{}", z)
    }
}

impl<X> AnnotatesState<X> for Mapping<X, bool>
where
    X: Value + Display,
{
    fn annotate_state(&self, x: &X) -> String {
        // if self.apply(x) {
        //     x.green().to_string()
        // } else {
        x.to_string()
        // }
    }
}
