use std::fmt::{Debug, Display};

use crate::{alphabet::Symbol, Color};

use super::StateIndex;

/// A transition is a concrete instantiation of a [`Edge`] in a [`TransitionSystem`].
/// While edges can be labeled with arbitrary expressions, transitions are labeled with
/// concrete symbols from the alphabet.
///
/// As an example, we may have an edge labeled with a boolean expression over some
/// atomic propositions, e.g. `a | b`. This edge can be instantiated with the symbols
/// `a & b`, `a & !b` and `!a & b`, which are all concrete symbols from the alphabet
/// that match the expression.
#[derive(Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Transition<'a, S, C> {
    source: StateIndex,
    target: StateIndex,
    symbol: S,
    color: &'a C,
}

impl<'a, S: Symbol, C: Color> Transition<'a, S, C> {
    /// Creates a new transition with the given source and target state, symbol and color.
    pub fn new(source: StateIndex, symbol: S, target: StateIndex, color: &'a C) -> Self {
        Self {
            source,
            target,
            symbol,
            color,
        }
    }

    /// Returns the source state index.
    pub fn source(&self) -> StateIndex {
        self.source
    }

    /// Returns the symbol of the source state.
    pub fn symbol(&self) -> S {
        self.symbol
    }

    /// Returns the color of the transition.
    pub fn color(&self) -> &C {
        self.color
    }

    /// Returns the target state index.
    pub fn target(&self) -> StateIndex {
        self.target
    }
}

impl<'a, S: Display, C: Display> Display for Transition<'a, S, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} --{}:{}--> {}",
            self.source, self.symbol, self.color, self.target
        )
    }
}

impl<'a, S: Debug, C: Debug> Debug for Transition<'a, S, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} --{:?}:{:?}--> {}",
            self.source, self.symbol, self.color, self.target
        )
    }
}
