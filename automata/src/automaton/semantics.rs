use crate::{
    alphabet::Symbol,
    ts::{
        path::{Lasso, LassoIn},
        run::{FiniteRun, OmegaRun},
        Deterministic, EdgeColor, StateColor, SymbolOf,
    },
    word::OmegaWord,
    Color, Set, Void,
};

use super::{Congruence, FiniteWord, Initialized, DTS};

pub type NewDFA<A> = Automaton<Initialized<DTS<A, bool, Void>>, DFASemantics>;

pub struct Automaton<D, A, const OMEGA: bool = false> {
    pub ts: D,
    pub acceptance: A,
}

impl<D, A> Automaton<D, A> {
    pub fn new(ts: D, acceptance: A) -> Self {
        Self { ts, acceptance }
    }
}

pub trait FiniteSemantics<Q, C> {
    type Output;
    fn finite_semantic<R>(&self, run: R) -> Self::Output
    where
        R: FiniteRun<StateColor = Q, EdgeColor = C>;
}

pub trait OmegaSemantics<Q, C> {
    type Output;
    fn omega_semantic<R>(&self, run: R) -> Self::Output
    where
        R: OmegaRun<StateColor = Q, EdgeColor = C>;
}

impl<D, A> Automaton<D, A, false>
where
    D: Congruence,
    A: FiniteSemantics<StateColor<D>, EdgeColor<D>>,
{
    pub fn accepts<W: FiniteWord<SymbolOf<D>>>(&self, word: W) -> bool
    where
        A: FiniteSemantics<StateColor<D>, EdgeColor<D>, Output = bool>,
    {
        self.transform(word)
    }

    pub fn transform<W: FiniteWord<SymbolOf<D>>>(&self, word: W) -> A::Output {
        self.acceptance.finite_semantic(self.ts.finite_run(word))
    }
}

impl<D, A> Automaton<D, A, true>
where
    D: Congruence,
    A: OmegaSemantics<StateColor<D>, EdgeColor<D>>,
{
    pub fn accepts<W: OmegaWord<SymbolOf<D>>>(&self, word: W) -> bool
    where
        A: OmegaSemantics<StateColor<D>, EdgeColor<D>, Output = bool>,
    {
        self.acceptance.omega_semantic(self.ts.omega_run(word))
    }

    pub fn transform<W: OmegaWord<SymbolOf<D>>>(&self, word: W) -> A::Output {
        self.acceptance.omega_semantic(self.ts.omega_run(word))
    }
}

pub struct DFASemantics;

impl<C> FiniteSemantics<bool, C> for DFASemantics {
    type Output = bool;
    fn finite_semantic<R>(&self, run: R) -> Self::Output
    where
        R: FiniteRun<StateColor = bool, EdgeColor = C>,
    {
        run.state_colors()
            .and_then(|colors| colors.last())
            .unwrap_or(false)
    }
}

#[cfg(test)]
mod tests {
    use crate::{ts::TSBuilder, TransitionSystem, Void};

    use super::NewDFA;

    #[test]
    fn new_dfa() {
        let ts = TSBuilder::default()
            .with_transitions([
                (0, 'a', Void, 0),
                (0, 'b', Void, 1),
                (1, 'a', Void, 0),
                (1, 'b', Void, 1),
            ])
            .with_colors([true, false])
            .deterministic()
            .with_initial(0);
        let dfa = NewDFA::new(ts, super::DFASemantics);
        assert!(dfa.accepts("abba"));
        assert!(!dfa.accepts("b"));
        assert!(dfa.accepts(""));
    }
}
