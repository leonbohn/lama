use crate::{
    ts::{
        path::{Lasso, LassoIn},
        Deterministic, SymbolOf,
    },
    word::OmegaWord,
    Color, Set,
};

use super::{Congruence, FiniteWord};

pub type NewDFA<D> = Automaton<D, Boolean>;

pub struct Automaton<D, A, const OMEGA: bool = false> {
    pub ts: D,
    pub acceptance: A,
}

impl<D, A> Automaton<D, A> {
    pub fn new(ts: D, acceptance: A) -> Self {
        Self { ts, acceptance }
    }
}

impl<D, A> Automaton<D, A, false>
where
    D: Congruence,
    A: FiniteSemantics<D>,
{
    pub fn accepts<W: FiniteWord<SymbolOf<D>>>(&self, word: W) -> A::Output {
        self.acceptance.finite_semantic(&self.ts, word)
    }
}

impl<D, A> Automaton<D, A, true>
where
    D: Congruence,
    A: OmegaSemantics<D>,
{
    pub fn accepts<W: OmegaWord<SymbolOf<D>>>(&self, word: W) -> A::Output {
        self.acceptance.omega_semantic(&self.ts, word)
    }
}

pub trait HasParity {
    fn parity(&self) -> bool;
}

impl HasParity for bool {
    fn parity(&self) -> bool {
        *self
    }
}

impl HasParity for usize {
    fn parity(&self) -> bool {
        self % 2 == 1
    }
}

pub struct Boolean;

impl<D> FiniteSemantics<D> for Boolean
where
    D: Congruence,
    D::StateColor: HasParity,
{
    type Output = bool;

    fn finite_semantic<W: FiniteWord<SymbolOf<D>>>(&self, ts: &D, word: W) -> Self::Output {
        ts.reached_state_color(word)
            .map(|c| c.parity())
            .unwrap_or(false)
    }
}

pub trait FiniteSemantics<D: Congruence> {
    type Output;
    fn finite_semantic<W: FiniteWord<SymbolOf<D>>>(&self, ts: &D, word: W) -> Self::Output;
}

pub trait OmegaSemantics<D: Congruence> {
    type Output;
    fn omega_semantic<W: OmegaWord<SymbolOf<D>>>(&self, ts: &D, word: W) -> Self::Output;
}
