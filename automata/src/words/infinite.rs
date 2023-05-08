use itertools::Itertools;

use super::{IsInfinite, Str, SymbolIterable, Word, WordTransitions};
use crate::{
    congruence::CongruenceTransition,
    ts::{transitionsystem::States, HasInput, HasStates, IntoStates, IntoTransitions},
    Class, InfiniteKind, Subword, Successor, Symbol,
};

pub trait InfiniteWord {
    type Symbol: Symbol;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A `PeriodicWord` essentially just loops a [`FiniteWord`] over and over again.
pub struct PeriodicWord<S>(pub(crate) Str<S>);

impl<S: Symbol> IsInfinite for PeriodicWord<S> {
    fn base_length(&self) -> usize {
        0
    }

    fn recur_length(&self) -> usize {
        self.0.symbols.len()
    }
}

impl<S: Symbol> Word for PeriodicWord<S> {
    type Kind = InfiniteKind;
    type S = S;

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.0.nth(index % self.0.symbols.len())
    }
}

impl<S, I: Into<Str<S>>> From<I> for PeriodicWord<S> {
    fn from(finite: I) -> Self {
        Self(finite.into())
    }
}

impl<S> TryFrom<UltimatelyPeriodicWord<S>> for PeriodicWord<S> {
    type Error = ();

    fn try_from(upw: UltimatelyPeriodicWord<S>) -> Result<Self, Self::Error> {
        if upw.0.is_empty() {
            Ok(upw.1)
        } else {
            Err(())
        }
    }
}

impl<'a, S: Symbol> SymbolIterable for &'a PeriodicWord<S> {
    type SymbolIter = std::iter::Cycle<std::vec::IntoIter<S>>;

    fn symbol_iter(self) -> Self::SymbolIter {
        self.0.symbol_iter().cycle()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// In an `UltimatelyPeriodicWord`, the first part is a finite prefix, after which a periodic part follows. The prefix can be empty.
pub struct UltimatelyPeriodicWord<S>(pub(crate) Str<S>, pub(crate) PeriodicWord<S>);

impl<S: Symbol> IsInfinite for UltimatelyPeriodicWord<S> {
    fn base_length(&self) -> usize {
        self.0.symbols.len()
    }

    fn recur_length(&self) -> usize {
        self.1 .0.symbols.len()
    }
}

impl<S: Symbol> Word for UltimatelyPeriodicWord<S> {
    type Kind = InfiniteKind;
    type S = S;

    fn nth(&self, index: usize) -> Option<Self::S> {
        let prefix_length = self.0.symbols.len();
        if index < prefix_length {
            self.0.nth(index)
        } else {
            self.1.nth(index - prefix_length)
        }
    }
}

impl<S> From<PeriodicWord<S>> for UltimatelyPeriodicWord<S> {
    fn from(periodic: PeriodicWord<S>) -> Self {
        Self(Str::epsilon(), periodic)
    }
}

impl<S> From<(Str<S>, PeriodicWord<S>)> for UltimatelyPeriodicWord<S> {
    fn from((prefix, periodic): (Str<S>, PeriodicWord<S>)) -> Self {
        Self(prefix, periodic)
    }
}

impl<'a, S: Symbol> SymbolIterable for &'a UltimatelyPeriodicWord<S> {
    type SymbolIter =
        std::iter::Chain<std::vec::IntoIter<S>, std::iter::Cycle<std::vec::IntoIter<S>>>;

    fn symbol_iter(self) -> Self::SymbolIter {
        self.0.symbol_iter().chain(self.1.symbol_iter())
    }
}

impl<S: Symbol> UltimatelyPeriodicWord<S> {
    /// Returns a reference to the base part of the word.
    pub fn base(&self) -> &Str<S> {
        &self.0
    }

    /// Returns a reference to the looping part of the word.
    pub fn recur(&self) -> &PeriodicWord<S> {
        &self.1
    }

    /// Unrolls the word by one step, i.e. it appends the first symbol of the looping part
    /// to the base part and rotates the looping part by one.
    ///
    /// # Example
    /// If we consider a word like (aba,baa), then unrolling it once will result in
    /// the word (abab,aab).
    pub fn unroll_one(&mut self) {
        self.0.symbols.push(self.1 .0.symbols[0].clone());
        self.1 .0.symbols.rotate_left(1);
    }

    /// Returns true if and only if the word has the given prefix.
    pub fn has_prefix(&self, prefix: &Str<S>) -> bool {
        self.symbol_iter()
            .zip(prefix.symbol_iter())
            .all(|(x, y)| x == y)
    }
}

impl<S: Symbol> Successor for UltimatelyPeriodicWord<S> {
    fn successor<X: std::borrow::Borrow<Self::Q>, Y: std::borrow::Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q> {
        todo!()
    }
}
impl<S: Symbol> HasStates for UltimatelyPeriodicWord<S> {
    type Q = Class<S>;

    fn contains_state<X: std::borrow::Borrow<Self::Q>>(&self, state: X) -> bool {
        self.has_prefix(&state.borrow().iter().collect())
    }
}
impl<S: Symbol> HasInput for UltimatelyPeriodicWord<S> {
    type Sigma = S;

    type Input<'me> = itertools::Unique<std::iter::Chain<std::slice::Iter<'me, S>,std::slice::Iter<'me, S>>> where Self:'me;

    fn input_alphabet(&self) -> Self::Input<'_> {
        todo!()
    }
}

impl<'a, S: Symbol> IntoStates for &'a UltimatelyPeriodicWord<S> {
    type StateRef = &'a Class<S>;

    type IntoStates = States<'a, Class<S>>;

    fn into_states(self) -> Self::IntoStates {
        todo!()
    }
}

impl<'a, S: Symbol> IntoTransitions for &'a UltimatelyPeriodicWord<S> {
    type TransitionRef = CongruenceTransition<S>;

    type IntoTransitions = WordTransitions<Self>;

    fn into_transitions(self) -> Self::IntoTransitions {
        WordTransitions::new(self)
    }
}
