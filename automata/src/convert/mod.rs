mod dot;
mod fmt;

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

pub trait AnnotatesTransition<X, Y> {
    fn annotate_transition(&self, x: &(X, Y), z: &X) -> String;
}

pub trait AnnotatesTransitionTarget<X, Y> {
    fn annotate_transition_target<T: Transition<Q = X, S = Y>>(&self, transition: T) -> String;
}

pub trait AnnotatesState<X> {
    fn annotate_state(&self, x: &X) -> String;
}

pub trait DisplayState: Successor {
    fn display_state(&self, state: &Self::Q) -> String;
}

pub trait DisplaySymbol: Successor {
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
        x.to_string()
    }
}
