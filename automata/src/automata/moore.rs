use itertools::Itertools;
use std::fmt::Debug;

use crate::prelude::*;

use super::MooreLike;

#[derive(Clone)]
pub struct MooreMachine<A, Q = usize, C: Color = NoColor, Ts = WithInitial<BTS<A, Q, C, usize>>> {
    ts: Ts,
    _q: std::marker::PhantomData<(A, Q, C)>,
}

pub type IntoMooreMachine<Ts> = MooreMachine<
    <Ts as HasAlphabet>::Alphabet,
    <Ts as TransitionSystem>::StateColor,
    <Ts as TransitionSystem>::EdgeColor,
    Ts,
>;

impl<A: Alphabet, Q: Color, C: Color> MooreMachine<A, Q, C> {
    pub fn new(alphabet: A, initial_state_output: Q) -> Self {
        Self {
            ts: WithInitial::with_initial_color(alphabet, initial_state_output),
            _q: std::marker::PhantomData,
        }
    }
}

// impl_ts_by_passthrough_on_wrapper!(MooreMachine <Ts, Q: Color>);

impl<Ts: TransitionSystem> TransitionSystem
    for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
{
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

    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        todo!()
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        todo!()
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        todo!()
    }
}

impl<Ts: TransitionSystem> HasAlphabet
    for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
{
    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts().alphabet()
    }
}

impl<Ts: Sproutable> Sproutable for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts> {
    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        Self {
            ts: Ts::new_for_alphabet(alphabet),
            _q: std::marker::PhantomData,
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

impl<Ts: Pointed> Pointed for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts> {
    fn initial(&self) -> Self::StateIndex {
        self.ts().initial()
    }
}

impl<Ts: TransitionSystem> MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts> {
    pub fn ts(&self) -> &Ts {
        &self.ts
    }

    pub fn ts_mut(&mut self) -> &mut Ts {
        &mut self.ts
    }
}

impl<Ts: PredecessorIterable> PredecessorIterable
    for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
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

impl<Q: Color, Ts: MooreLike<Q>> From<Ts> for MooreMachine<Ts::Alphabet, Q, Ts::EdgeColor, Ts> {
    fn from(ts: Ts) -> Self {
        Self {
            ts,
            _q: std::marker::PhantomData,
        }
    }
}

impl<Ts: TransitionSystem + Debug> std::fmt::Debug
    for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[allow(missing_docs)]
macro_rules! impl_moore_automaton {
    (
        $(#[$($attributes:tt)*])*
        $name:ident, $color:ident
    ) => {
        $(#[$($attributes)*])*
        #[derive(Clone)]
        pub struct $name<
            A = Simple,
            C = NoColor,
            Ts = WithInitial<BTS<A, $color, C, usize>>,
        > {
            ts: Ts,
            _alphabet: std::marker::PhantomData<(A, $color, C)>,
        }
        paste::paste! {
            pub type [< Into $name >]<Ts> = $name<<Ts as HasAlphabet>::Alphabet, <Ts as TransitionSystem>::EdgeColor, Ts>;
        }

        impl<A: Alphabet, C: Color>
            $name<A, C, WithInitial<BTS<A, $color, C, usize>>>
        {
            pub fn new(alphabet: A) -> $name<A, C, WithInitial<BTS<A, $color, C, usize>>> {
                $name {
                    ts: WithInitial::new(alphabet),
                    _alphabet: std::marker::PhantomData,
                }
            }
        }
        impl<Ts: TransitionSystem> $name<Ts::Alphabet, Ts::EdgeColor, Ts> {
            pub fn ts(&self) -> &Ts {
                &self.ts
            }
            pub fn ts_mut(&mut self) -> &mut Ts {
                &mut self.ts
            }
        }
        impl<Ts: TransitionSystem<StateColor = $color>> From<Ts>
            for $name<Ts::Alphabet,  Ts::EdgeColor, Ts>
        {
            fn from(ts: Ts) -> Self {
                Self {
                    ts,
                    _alphabet: std::marker::PhantomData,
                }
            }
        }
        impl<Ts: PredecessorIterable> PredecessorIterable
            for $name<Ts::Alphabet, Ts::EdgeColor, Ts>
        {
            type PreTransitionRef<'this> = Ts::PreTransitionRef<'this> where Self: 'this;
            type EdgesToIter<'this> = Ts::EdgesToIter<'this> where Self: 'this;
            fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
                self.ts().predecessors(state)
            }
        }
        impl<Ts: Pointed> std::fmt::Debug for $name<Ts::Alphabet,  Ts::EdgeColor, Ts> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use itertools::Itertools;
                writeln!(
                    f,
                    "Initial state {} with states {}",
                    self.ts().initial(),
                    self.ts().state_indices().map(|i| format!("{i}")).join(", ")
                )
            }
        }
        impl<Ts: Sproutable> Sproutable
            for $name<Ts::Alphabet, Ts::EdgeColor, Ts>
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
        impl<Ts: TransitionSystem> HasAlphabet for $name<Ts::Alphabet, Ts::EdgeColor, Ts> {
            type Alphabet = Ts::Alphabet;
            fn alphabet(&self) -> &Self::Alphabet {
                self.ts.alphabet()
            }
        }
        impl<Ts: TransitionSystem> TransitionSystem
            for $name<Ts::Alphabet, Ts::EdgeColor, Ts>
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

            fn transition<Idx: $crate::prelude::Indexes<Self>>(
                &self,
                state: Idx,
                symbol: SymbolOf<Self>,
            ) -> Option<Self::TransitionRef<'_>> {
                self.ts().transition(state.to_index(self)?, symbol)
            }

            fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
                self.ts().state_color(state)
            }

            fn edge_color(
                &self,
                state: Self::StateIndex,
                expression: &ExpressionOf<Self>,
            ) -> Option<EdgeColor<Self>> {
                self.ts().edge_color(state, expression)
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
        impl<Ts: Pointed> Pointed for $name<Ts::Alphabet, Ts::EdgeColor, Ts> {
            fn initial(&self) -> Self::StateIndex {
                self.ts().initial()
            }
        }
    };
}
