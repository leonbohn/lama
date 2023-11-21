#![allow(missing_docs)]
use itertools::Itertools;
use std::{fmt::Debug, marker::PhantomData};

use crate::prelude::*;

use super::MooreLike;

#[derive(Clone)]
pub struct MealyMachine<A, C = usize, Ts = WithInitial<BTS<A, NoColor, C, usize>>> {
    ts: Ts,
    _q: PhantomData<(A, C)>,
}

pub type IntoMealyMachine<Ts> =
    MealyMachine<<Ts as HasAlphabet>::Alphabet, <Ts as TransitionSystem>::EdgeColor, Ts>;

impl<A: Alphabet> MealyMachine<A> {
    pub fn new(alphabet: A) -> Self {
        Self {
            ts: WithInitial::new(alphabet),
            _q: PhantomData,
        }
    }
}

impl<Ts: TransitionSystem> TransitionSystem for MealyMachine<Ts::Alphabet, Ts::EdgeColor, Ts> {
    type StateIndex = Ts::StateIndex;

    type StateColor = Ts::StateColor;

    type EdgeColor = Ts::EdgeColor;

    type TransitionRef<'this> = Ts::TransitionRef<'this>
    where
        Self: 'this;

    type EdgesFromIter<'this> = Ts::EdgesFromIter<'this>
    where
        Self: 'this;

    type StateIndices<'this> = Ts::StateIndices<'this>
    where
        Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts().state_indices()
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        self.ts().edges_from(state.to_index(self)?)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        self.ts().state_color(state.to_index(self)?)
    }
}

impl<D: Deterministic> Deterministic for MealyMachine<D::Alphabet, D::EdgeColor, D> {
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        self.ts().transition(state.to_index(self)?, symbol)
    }
}

impl<Ts: TransitionSystem> HasAlphabet for MealyMachine<Ts::Alphabet, Ts::EdgeColor, Ts> {
    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts().alphabet()
    }
}

impl<Ts: Sproutable> Sproutable for MealyMachine<Ts::Alphabet, Ts::EdgeColor, Ts> {
    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        Self {
            ts: Ts::new_for_alphabet(alphabet),
            _q: PhantomData,
        }
    }

    fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
        self.ts_mut().add_state(color)
    }

    type ExtendStateIndexIter = Ts::ExtendStateIndexIter;

    fn extend_states<I: IntoIterator<Item = StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter {
        self.ts_mut().extend_states(iter)
    }

    fn set_state_color<X: Into<StateColor<Self>>>(&mut self, index: Self::StateIndex, color: X) {
        self.ts_mut().set_state_color(index, color)
    }

    fn add_edge<X, Y>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: EdgeColor<Self>,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Into<Self::StateIndex>,
        Y: Into<Self::StateIndex>,
    {
        self.ts_mut().add_edge(from, on, to, color)
    }

    fn remove_edge(
        &mut self,
        from: Self::StateIndex,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool {
        self.ts_mut().remove_edge(from, on)
    }
}

impl<Ts: Pointed> Pointed for MealyMachine<Ts::Alphabet, Ts::EdgeColor, Ts> {
    fn initial(&self) -> Self::StateIndex {
        self.ts().initial()
    }
}

impl<Ts: TransitionSystem> MealyMachine<Ts::Alphabet, Ts::EdgeColor, Ts> {
    pub fn ts(&self) -> &Ts {
        &self.ts
    }

    pub fn ts_mut(&mut self) -> &mut Ts {
        &mut self.ts
    }
}

impl<Ts: PredecessorIterable> PredecessorIterable
    for MealyMachine<Ts::Alphabet, Ts::EdgeColor, Ts>
{
    type PreTransitionRef<'this> = Ts::PreTransitionRef<'this>
    where
        Self: 'this;

    type EdgesToIter<'this> = Ts::EdgesToIter<'this>
    where
        Self: 'this;

    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        self.ts().predecessors(state)
    }
}

impl<C: Color, Ts: MealyLike<C>> From<Ts> for MealyMachine<Ts::Alphabet, C, Ts> {
    fn from(ts: Ts) -> Self {
        Self {
            ts,
            _q: PhantomData,
        }
    }
}

impl<Ts: TransitionSystem + Debug> std::fmt::Debug
    for MealyMachine<Ts::Alphabet, Ts::EdgeColor, Ts>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[allow(missing_docs)]
macro_rules! impl_mealy_automaton {
    ($name:ident, $color:ident) => {
        paste::paste! {
            pub type [< Into $name >]<Ts> = $name<<Ts as HasAlphabet>::Alphabet, <Ts as TransitionSystem>::StateColor, Ts>;
        }

        #[derive(Clone)]
        pub struct $name<
            A = Simple,
            Q = (),
            Ts = WithInitial<BTS<A, Q, $color, usize>>,
        > {
            ts: Ts,
            _alphabet: std::marker::PhantomData<(A, Q, $color)>,
        }
        impl<A: Alphabet, Q: Color + Default>
            $name<A, Q, WithInitial<BTS<A, Q, $color, usize>>>
        {
            pub fn new(alphabet: A, initial_state_color: Q) -> $name<A, Q, WithInitial<BTS<A, Q, $color, usize>>> {
                let mut ts = WithInitial::new(alphabet);
                ts.set_initial_color(initial_state_color);
                $name {
                    ts,
                    _alphabet: std::marker::PhantomData,
                }
            }
        }
        impl<Ts: TransitionSystem> $name<Ts::Alphabet, Ts::StateColor, Ts> {
            pub fn ts(&self) -> &Ts {
                &self.ts
            }
            pub fn ts_mut(&mut self) -> &mut Ts {
                &mut self.ts
            }
        }
        impl<Ts: TransitionSystem<EdgeColor = $color>> From<Ts>
            for $name<Ts::Alphabet,  Ts::StateColor, Ts>
        {
            fn from(ts: Ts) -> Self {
                Self {
                    ts,
                    _alphabet: std::marker::PhantomData,
                }
            }
        }
        impl<Ts: PredecessorIterable> PredecessorIterable
            for $name<Ts::Alphabet, Ts::StateColor, Ts>
        {
            type PreTransitionRef<'this> = Ts::PreTransitionRef<'this> where Self: 'this;
            type EdgesToIter<'this> = Ts::EdgesToIter<'this> where Self: 'this;
            fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
                self.ts().predecessors(state)
            }
        }
        impl<Ts: Pointed> std::fmt::Debug for $name<Ts::Alphabet,  Ts::StateColor, Ts> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use itertools::Itertools;
                use crate::prelude::IsTransition;
                writeln!(
                    f,
                    "Initial state {} with states {} and transitions\n{}",
                    self.ts().initial(),
                    self.ts().state_indices().map(|i| format!("{i}")).join(", "),
                    self.ts().state_indices().map(|i| self.edges_from(i).unwrap().map(|e| format!("{} --{:?}--> {}", i, e.expression(), e.target())).join("\n")).join("\n")
                )
            }
        }
        impl<Ts: Sproutable> Sproutable
            for $name<Ts::Alphabet, Ts::StateColor, Ts>
        {
            type ExtendStateIndexIter = Ts::ExtendStateIndexIter;
            fn extend_states<I: IntoIterator<Item = StateColor<Self>>>(
                &mut self,
                iter: I,
            ) -> Self::ExtendStateIndexIter {
                self.ts_mut().extend_states(iter)
            }
            fn set_state_color<X: Into<StateColor<Self>>>(
                &mut self,
                index: Self::StateIndex,
                color: X,
            ) {
                self.ts_mut().set_state_color(index, color)
            }
            fn add_edge<X, Y>(
                &mut self,
                from: X,
                on: <Self::Alphabet as Alphabet>::Expression,
                to: Y,
                color: EdgeColor<Self>,
            ) -> Option<(Self::StateIndex, Self::EdgeColor)>
            where
                X: Into<Self::StateIndex>,
                Y: Into<Self::StateIndex>,
            {
                self.ts_mut().add_edge(from, on, to, color)
            }
            fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
                Self {
                    ts: Ts::new_for_alphabet(alphabet),
                    _alphabet: std::marker::PhantomData,
                }
            }
            fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
                self.ts_mut().add_state(color)
            }
            fn remove_edge(
                &mut self,
                from: Self::StateIndex,
                on: <Self::Alphabet as Alphabet>::Expression,
            ) -> bool {
                self.ts_mut().remove_edge(from, on)
            }
        }
        impl<Ts: TransitionSystem> HasAlphabet for $name<Ts::Alphabet, Ts::StateColor, Ts> {
            type Alphabet = Ts::Alphabet;
            fn alphabet(&self) -> &Self::Alphabet {
                self.ts.alphabet()
            }
        }
        impl<Ts: TransitionSystem> TransitionSystem
            for $name<Ts::Alphabet, Ts::StateColor, Ts>
        {
            type StateIndex = Ts::StateIndex;
            type EdgeColor = Ts::EdgeColor;
            type StateColor = Ts::StateColor;
            type TransitionRef<'this> = Ts::TransitionRef<'this> where Self: 'this;
            type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
            type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;
            fn state_indices(&self) -> Self::StateIndices<'_> {
                self.ts().state_indices()
            }

            fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
                self.ts().state_color(state)
            }


            fn edges_from<Idx: $crate::prelude::Indexes<Self>>(
                &self,
                state: Idx,
            ) -> Option<Self::EdgesFromIter<'_>> {
                self.ts().edges_from(state.to_index(self)?)
            }

            fn maybe_initial_state(&self) -> Option<Self::StateIndex> {
                self.ts().maybe_initial_state()
            }
        }
        impl<Ts: Pointed> Pointed for $name<Ts::Alphabet, Ts::StateColor, Ts> {
            fn initial(&self) -> Self::StateIndex {
                self.ts().initial()
            }
        }
        impl<Ts: Deterministic> Deterministic for $name<Ts::Alphabet, Ts::StateColor, Ts> {
            fn transition<Idx: $crate::prelude::Indexes<Self>>(
                &self,
                state: Idx,
                symbol: SymbolOf<Self>,
            ) -> Option<Self::TransitionRef<'_>> {
                self.ts().transition(state.to_index(self)?, symbol)
            }

            fn edge_color(
                &self,
                state: Self::StateIndex,
                expression: &ExpressionOf<Self>,
            ) -> Option<EdgeColor<Self>> {
                self.ts().edge_color(state, expression)
            }
        }
    };
}

/// Implemented by objects which can be viewed as a MealyMachine, i.e. a finite transition system
/// which has outputs of type usize on its edges.
pub trait MealyLike<C: Color>: TransitionSystem<EdgeColor = C> + Pointed {
    /// Uses a reference to `self` for obtaining a [`MealyMachine`].
    fn as_mealy(&self) -> MealyMachine<Self::Alphabet, C, &Self> {
        MealyMachine::from(self)
    }

    /// Consumes `self`, returning a [`MealyMachine`] that uses the underlying transition system.
    fn into_mealy(self) -> MealyMachine<Self::Alphabet, C, Self> {
        MealyMachine::from(self)
    }

    /// Attempts to run the given finite word in `self`, returning the color of the last transition that
    /// is taken wrapped in `Some`. If no successful run on `input` is possible, the function returns `None`.
    fn try_mealy_map<W: FiniteWord<SymbolOf<Self>>>(&self, input: W) -> Option<C> {
        todo!()
    }

    /// Returns a vector over all colors that can be emitted.
    fn color_range(&self) -> Vec<Self::EdgeColor> {
        self.reachable_state_indices()
            .flat_map(|o| self.edges_from(o).unwrap().map(|e| IsTransition::color(&e)))
            .unique()
            .collect()
    }
}
impl<Ts: TransitionSystem + Pointed + Sized> MealyLike<Ts::EdgeColor> for Ts {}
