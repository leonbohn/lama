use itertools::Itertools;
use owo_colors::OwoColorize;
use std::fmt::Debug;

use crate::prelude::*;

/// A Moore machine is a transition system where each state has an output. Thus, the output
/// of running a Moore machine on a word produces a sequence of outputs, one for each state
/// that is visited. For a word of length `n`, there are `n+1` outputs, note in particular
/// that the empty word produce an output, which is in contrast to [`MealyMachine`]s, where
/// the empty word produces no output.
///
/// Usually, we are interested in the output of the last state that is reached during a run
/// on a word. In case of a deterministic Moore machine, this is the only output that is
/// produced. A [`DFA`] is then a Moore machine, where the colors are `bool`. A word reaches
/// a state and the corresponding `bool` is emitted, where `true` corresponds to an accepted
/// input, whereas `false` represents a rejected input. For infinite runs, we usually
/// consider the colors that are produced infinitely often and base acceptance around them. It
/// is, however, prefered to use a [`MealyMachine`] for this purpose, as for infinite inputs
/// switching to transition-based acceptance is preferable.
#[derive(Clone)]
pub struct MooreMachine<A, Q = usize, C: Color = NoColor, Ts = WithInitial<BTS<A, Q, C, usize>>> {
    ts: Ts,
    _q: std::marker::PhantomData<(A, Q, C)>,
}

/// Helper type that takes a pointed transition system and returns the corresponding
/// [`MooreMachine`], which the ts can be converted into using [`Into::into`].
/// For concrete automaton types such as [`DFA`], the [`IntoDFA`] type can be used to
/// obtain the type of a [`DFA`] for the given ts.
pub type IntoMooreMachine<Ts> = MooreMachine<
    <Ts as TransitionSystem>::Alphabet,
    <Ts as TransitionSystem>::StateColor,
    <Ts as TransitionSystem>::EdgeColor,
    Ts,
>;

impl<A: Alphabet, Q: Color, C: Color> MooreMachine<A, Q, C> {
    /// Creates a new MooreMachine on a [`BTS`].
    pub fn new(
        alphabet: A,
        initial_state_output: Q,
    ) -> IntoMooreMachine<WithInitial<BTS<A, Q, C, usize>>> {
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

    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts().state_indices()
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        self.ts().edges_from(state.to_index(self)?)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        self.ts().state_color(state)
    }
}

impl<D: Deterministic> Deterministic for MooreMachine<D::Alphabet, D::StateColor, D::EdgeColor, D> {
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        self.ts().transition(state.to_index(self)?, symbol)
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
    /// Gives a reference to the underlying ts.
    pub fn ts(&self) -> &Ts {
        &self.ts
    }

    /// Gives a mutable reference to the underlying ts.
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
        write!(f, "{}\n{:?}", "Moore Machine".italic(), self.ts())
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
            /// See [`IntoMooreMachine`].
            pub type [< Into $name >]<Ts> = $name<<Ts as TransitionSystem>::Alphabet, <Ts as TransitionSystem>::EdgeColor, Ts>;
        }

        impl<A: Alphabet, C: Color>
            $name<A, C, WithInitial<BTS<A, $color, C, usize>>>
        {
            /// Creates a new automaton.
            pub fn new(alphabet: A) -> $name<A, C, WithInitial<BTS<A, $color, C, usize>>> {
                $name {
                    ts: WithInitial::new(alphabet),
                    _alphabet: std::marker::PhantomData,
                }
            }
        }
        impl<Ts: TransitionSystem> $name<Ts::Alphabet, Ts::EdgeColor, Ts> {
            /// Gives a reference to the underlying transition system.
            pub fn ts(&self) -> &Ts {
                &self.ts
            }
            /// Gives a mutable reference to the underlying transition system.
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
        impl<Ts: Pointed> std::fmt::Debug for $name<Ts::Alphabet,  Ts::EdgeColor, Ts> where Ts::StateColor: std::fmt::Display {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use itertools::Itertools;
                use crate::prelude::IsTransition;
                writeln!(
                    f,
                    "Initial state {} with states {} and transitions\n{}",
                    self.ts().initial(),
                    self.ts().state_indices().map(|i| if let Some(state_color) = self.ts().state_color(i) {
                        format!("{i}|{state_color}")
                    } else {
                        format!("{i}")
                    }).join(", "),
                    self.ts().state_indices().map(|i| self.edges_from(i).unwrap().map(|e| format!("{} --{:?}--> {}", i, e.expression(), e.target())).join(", ")).join("\n")
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
        impl<Ts: TransitionSystem> TransitionSystem
            for $name<Ts::Alphabet, Ts::EdgeColor, Ts>
        {
            type StateIndex = Ts::StateIndex;
            type EdgeColor = Ts::EdgeColor;
            type StateColor = Ts::StateColor;
            type TransitionRef<'this> = Ts::TransitionRef<'this> where Self: 'this;
            type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
            type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;
            type Alphabet = Ts::Alphabet;

            fn alphabet(&self) -> &Self::Alphabet {
                self.ts().alphabet()
            }

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
        impl<D: Deterministic> Deterministic for $name<D::Alphabet, D::EdgeColor, D> {
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
        impl<Ts: Pointed> Pointed for $name<Ts::Alphabet, Ts::EdgeColor, Ts> {
            fn initial(&self) -> Self::StateIndex {
                self.ts().initial()
            }
        }
    };
}

/// Implemented by objects that can be viewed as MooreMachines, i.e. finite transition systems
/// that have usize annotated/outputting states.
pub trait MooreLike<Q: Color>: Deterministic<StateColor = Q> + Pointed {
    /// Takes a reference to `self` and turns the underlying transition system into a [`MooreMachine`].
    fn as_moore(&self) -> MooreMachine<Self::Alphabet, Q, Self::EdgeColor, &Self> {
        MooreMachine::from(self)
    }

    /// Consumes and thereby turns `self` into a [`MooreMachine`].
    fn into_moore(self) -> MooreMachine<Self::Alphabet, Q, Self::EdgeColor, Self> {
        MooreMachine::from(self)
    }

    /// Runs the given `input` word in self. If the run is successful, the color of the state that it reaches
    /// is emitted (wrapped in a `Some`). For unsuccessful runs, `None` is returned.
    fn try_moore_map<W: FiniteWord<SymbolOf<Self>>>(&self, input: W) -> Option<Q> {
        self.reached_state_color(input)
    }

    /// Obtains a vec containing the possible colors emitted by `self` (without duplicates).
    fn color_range(&self) -> Vec<Self::StateColor> {
        self.reachable_state_indices()
            .map(|o| {
                self.state_color(o)
                    .expect("We know it is reachable and it must be colored")
            })
            .unique()
            .collect()
    }

    /// Decomposes `self` into a sequence of DFAs, where the i-th DFA accepts all words which
    /// produce a color less than or equal to i.
    fn decompose_dfa(&self) -> Vec<DFA<Self::Alphabet>> {
        self.color_range()
            .into_iter()
            .sorted()
            .map(|i| self.color_or_below_dfa(i))
            .collect()
    }

    /// Builds a DFA that accepts all words which emit a color less than or equal to `color`.
    fn color_or_below_dfa(&self, color: Q) -> DFA<Self::Alphabet> {
        self.map_state_colors(|o| o <= color)
            .erase_edge_colors()
            .minimize()
            .erase_edge_colors()
            .map_state_colors(|o| o.contains(&true))
            .collect_with_initial()
    }
}
impl<Ts: Deterministic + Pointed> MooreLike<Ts::StateColor> for Ts {}
