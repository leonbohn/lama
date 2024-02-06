use crate::{
    ts::{
        run::{FiniteRun, OmegaRun},
        EdgeColor, Sproutable, StateColor, SymbolOf,
    },
    word::OmegaWord,
    Alphabet, Pointed, TransitionSystem,
};

use super::{Congruence, Deterministic, FiniteWord, PredecessorIterable};

#[derive(Clone, Eq, PartialEq, Copy)]
pub struct Automaton<D, A, const OMEGA: bool = false> {
    pub ts: D,
    pub acceptance: A,
}

impl<D, A, const OMEGA: bool> Automaton<D, A, OMEGA> {
    pub fn from_parts(ts: D, acceptance: A) -> Self {
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

impl<D, A, const OMEGA: bool> AsRef<Automaton<D, A, OMEGA>> for Automaton<D, A, OMEGA> {
    fn as_ref(&self) -> &Automaton<D, A, OMEGA> {
        self
    }
}

impl<D: Deterministic, A, const OMEGA: bool> Deterministic for Automaton<D, A, OMEGA> {}

impl<D: PredecessorIterable, A, const OMEGA: bool> PredecessorIterable for Automaton<D, A, OMEGA> {
    type PreEdgeRef<'this> = D::PreEdgeRef<'this>
    where
        Self: 'this;

    type EdgesToIter<'this> = D::EdgesToIter<'this>
    where
        Self: 'this;

    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        self.ts.predecessors(state)
    }
}

impl<D: Pointed, A, const OMEGA: bool> Pointed for Automaton<D, A, OMEGA> {
    fn initial(&self) -> Self::StateIndex {
        self.ts.initial()
    }
}

impl<D: Sproutable, A: Default, const OMEGA: bool> Sproutable for Automaton<D, A, OMEGA> {
    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        Automaton::from_parts(D::new_for_alphabet(alphabet), Default::default())
    }

    fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
        self.ts.add_state(color)
    }

    type ExtendStateIndexIter = D::ExtendStateIndexIter;

    fn extend_states<I: IntoIterator<Item = StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter {
        self.ts.extend_states(iter)
    }

    fn set_state_color<X: Into<StateColor<Self>>>(&mut self, index: Self::StateIndex, color: X) {
        self.ts.set_state_color(index, color)
    }

    fn add_edge<X, Y>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: EdgeColor<Self>,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: super::Indexes<Self>,
        Y: super::Indexes<Self>,
    {
        let from = from.to_index(self)?;
        let to = to.to_index(self)?;
        self.ts.add_edge(from, on, to, color)
    }

    fn remove_edges<X: super::Indexes<Self>>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool {
        let Some(from) = from.to_index(self) else {
            return false;
        };
        self.ts.remove_edges(from, on)
    }
}

impl<D: TransitionSystem, A, const OMEGA: bool> TransitionSystem for Automaton<D, A, OMEGA> {
    type Alphabet = D::Alphabet;

    type StateIndex = D::StateIndex;

    type StateColor = D::StateColor;

    type EdgeColor = D::EdgeColor;

    type EdgeRef<'this> = D::EdgeRef<'this>
    where
        Self: 'this;

    type EdgesFromIter<'this> = D::EdgesFromIter<'this>
    where
        Self: 'this;

    type StateIndices<'this> = D::StateIndices<'this>
    where
        Self: 'this;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts.state_indices()
    }

    fn edges_from<Idx: super::Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        self.ts.edges_from(state.to_index(self)?)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        self.ts.state_color(state)
    }
}

impl<T, A, const OMEGA: bool> std::fmt::Debug for Automaton<T, A, OMEGA>
where
    T: std::fmt::Debug,
    A: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\nacceptance\t\t{:?}\n{:?}",
            match OMEGA {
                true => "Omega word automaton",
                false => "Finite word automaton",
            },
            self.acceptance,
            self.ts
        )
    }
}
