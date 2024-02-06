use std::fmt::Debug;

use crate::{
    alphabet::Symbol,
    ts::{
        path::{Lasso, LassoIn},
        run::{FiniteRun, OmegaRun},
        Deterministic, EdgeColor, Sproutable, StateColor, SymbolOf,
    },
    word::OmegaWord,
    Alphabet, Color, Pointed, Set, TransitionSystem, Void,
};

use super::{Congruence, FiniteWord, Initialized, PredecessorIterable, DTS};

#[derive(Clone, Eq, PartialEq)]
pub struct Automaton<D, A, const OMEGA: bool = false> {
    pub ts: D,
    pub acceptance: A,
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
        todo!()
    }

    fn set_state_color<X: Into<StateColor<Self>>>(&mut self, index: Self::StateIndex, color: X) {
        todo!()
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
        todo!()
    }

    fn remove_edges<X: super::Indexes<Self>>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool {
        todo!()
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

impl<T, A, const OMEGA: bool> Debug for Automaton<T, A, OMEGA>
where
    T: Debug,
    A: Debug,
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

macro_rules! define_automaton_type {
    ($acctype:ident $name:ident; $q:ident, $c:ident; $omega:ident) => {
        paste::paste! {
            pub type $name<A = crate::alphabet::Simple> = crate::automaton::Automaton<Initialized<DTS<A, $q, $c>>, $acctype, $omega>;
            pub type [< As $name >]<T> = crate::automaton::Automaton<T, $acctype, $omega>;
            pub trait [< Into $name >] : Congruence
            {
                #[doc = "Consumes self and produces a new `" $name "` instance."]
                fn [< into_ $name:lower >](self) -> [< As $name >]<Self>
                {
                    crate::automaton::Automaton::from_parts(self, $acctype)
                }
                // #[doc = "Collects the reachable part of `self` to build a new transition system and build an instance of `" $name "`." ]
                // fn [< collect_trim_ $name:lower >](&self) -> $name<Self::Alphabet>
                // where
                //     EdgeColor<Self>: Into<$c>,
                //     StateColor<Self>: Into<$q>, {
                //     crate::automaton::Automaton::from_parts(self.trim_collect(), $acctype)
                // }
                #[doc = "Collects `self` to build a new transition system and build an instance of `" $name "`." ]
                fn [< collect_ $name:lower >](&self) -> $name<Self::Alphabet>
                where
                    EdgeColor<Self>: Into<$c>,
                    StateColor<Self>: Into<$q>,{
                    crate::automaton::Automaton::from_parts(self.collect_pointed().0, $acctype)
                }
            }
            impl<T> [< Into $name >] for T
            where
                T: Congruence,
                StateColor<T>: Into<$q>,
                EdgeColor<T>: Into<$c>,
            {

            }
            pub trait [< $name Like >] : Congruence<StateColor = $q, EdgeColor = $c> {}
            impl<T: Congruence<StateColor = $q, EdgeColor = $c>> [< $name Like >] for T {}
        }
    };
}
