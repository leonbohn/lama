use std::fmt::Display;

use crate::{Symbol, TransitionSystem};

/// A product of two transition systems.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Product<L, R> {
    pub left: L,
    pub right: R,
}

/// Represents a pair of objects from two different types, mainly used for the construction of
/// product transition systems.
#[derive(Debug, Clone, Ord, PartialEq, PartialOrd, Eq, Hash)]
pub struct Pair<X, Y>(pub X, pub Y);

impl<X, Y> Display for Pair<X, Y>
where
    X: Display,
    Y: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

impl<X, Y> Symbol for Pair<X, Y>
where
    X: Symbol,
    Y: Symbol,
{
}

impl<L, R> TransitionSystem for Product<L, R>
where
    L: TransitionSystem,
    R: TransitionSystem,
{
    type State = Pair<L::State, R::State>;

    type Input = Pair<L::Input, R::Input>;

    fn succ(&self, _from: &Self::State, _on: &Self::Input) -> Option<Self::State> {
        todo!()
    }

    fn vec_alphabet(&self) -> Vec<Self::Input> {
        todo!()
    }

    fn vec_states(&self) -> Vec<Self::State> {
        todo!()
    }
}
