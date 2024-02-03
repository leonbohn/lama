use itertools::Itertools;
use owo_colors::OwoColorize;
use std::fmt::Debug;

use crate::{prelude::*, Void};

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
pub struct MooreMachine<A, Q = usize, C = Void, Ts = Initialized<DTS<A, Q, C>>> {
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
/// Helper type that takes a pointed transition system and returns the corresponding
/// [`MooreMachine`]. Note that this will consume the underlying ts and the given
/// [`MooreMachine`] will use the default ts, which is `Initialized<DTS<A, Q, C>>`.
pub type AsMooreMachine<Ts> = MooreMachine<
    <Ts as TransitionSystem>::Alphabet,
    <Ts as TransitionSystem>::StateColor,
    <Ts as TransitionSystem>::EdgeColor,
>;

impl<A: Alphabet, Q: Color, C: Color> MooreMachine<A, Q, C> {
    /// Creates a new MooreMachine on a [`BTS`].
    pub fn new(
        alphabet: A,
        initial_state_output: Q,
    ) -> IntoMooreMachine<Initialized<DTS<A, Q, C>>> {
        Self {
            ts: Initialized::with_initial_color(alphabet, initial_state_output),
            _q: std::marker::PhantomData,
        }
    }
}

impl<D: MooreLike + Deterministic> IntoMooreMachine<D>
where
    StateColor<D>: Color,
{
    /// Returns the unique minimal moore machine that is bisimilar to `self`. This means
    /// for every finite word, the output of `self` and the output of the returned moore
    /// machine is the same. This is done using the Hopcroft and Moore algorithms for
    /// minimizing deterministic finite automata, implemented in the
    /// [`crate::algorithms::moore_partition_refinement`] function.
    pub fn minimize(&self) -> AsMooreMachine<D>
    where
        StateColor<D>: Color,
    {
        crate::algorithms::moore_partition_refinement(self)
    }
}

impl<Ts: MooreLike> TransitionSystem
    for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
where
    StateColor<Ts>: Color,
{
    type StateIndex = Ts::StateIndex;

    type StateColor = Ts::StateColor;

    type EdgeColor = Ts::EdgeColor;

    type EdgeRef<'this> = Ts::EdgeRef<'this>
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

impl<D: MooreLike> Deterministic for MooreMachine<D::Alphabet, D::StateColor, D::EdgeColor, D>
where
    StateColor<D>: Color,
{
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        self.ts().transition(state.to_index(self)?, symbol)
    }
}

impl<Ts: Sproutable + MooreLike> Sproutable
    for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
where
    StateColor<Ts>: Color,
{
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

    fn remove_edges(
        &mut self,
        from: Self::StateIndex,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool {
        self.ts_mut().remove_edges(from, on)
    }
}

impl<Ts: Pointed + MooreLike> Pointed
    for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
where
    StateColor<Ts>: Color,
{
    fn initial(&self) -> Self::StateIndex {
        self.ts().initial()
    }
}

impl<Ts: MooreLike> MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
where
    StateColor<Ts>: Color,
{
    /// Gives a reference to the underlying ts.
    pub fn ts(&self) -> &Ts {
        &self.ts
    }

    /// Gives a mutable reference to the underlying ts.
    pub fn ts_mut(&mut self) -> &mut Ts {
        &mut self.ts
    }
}

impl<Ts: MooreLike + PredecessorIterable> PredecessorIterable
    for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
where
    StateColor<Ts>: Color,
{
    type PreEdgeRef<'this> = Ts::PreEdgeRef<'this>
    where
        Self: 'this;

    type EdgesToIter<'this> = Ts::EdgesToIter<'this>
    where
        Self: 'this;

    fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
        self.ts().predecessors(state)
    }
}

impl<Ts: MooreLike> From<Ts> for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
where
    StateColor<Ts>: Color,
{
    fn from(ts: Ts) -> Self {
        Self {
            ts,
            _q: std::marker::PhantomData,
        }
    }
}

impl<Ts: MooreLike + Debug> std::fmt::Debug
    for MooreMachine<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
where
    StateColor<Ts>: Color,
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
            C = crate::Void,
            Ts = Initialized<DTS<A, $color, C>>,
        > {
            ts: Ts,
            _alphabet: std::marker::PhantomData<(A, $color, C)>,
        }
        paste::paste! {
            /// See [`IntoMooreMachine`].
            pub type [< Into $name >]<Ts> = $name<<Ts as TransitionSystem>::Alphabet, <Ts as TransitionSystem>::EdgeColor, Ts>;
            pub type [< As $name >]<Ts> = $name<<Ts as TransitionSystem>::Alphabet, <Ts as TransitionSystem>::EdgeColor>;
        }

        impl<A: Alphabet, C: Clone>
            $name<A, C, Initialized<DTS<A, $color, C>>>
        {
            /// Creates a new automaton.
            pub fn new(alphabet: A) -> $name<A, C, Initialized<DTS<A, $color, C>>> {
                $name {
                    ts: Initialized::new(alphabet),
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
            type PreEdgeRef<'this> = Ts::PreEdgeRef<'this> where Self: 'this;
            type EdgesToIter<'this> = Ts::EdgesToIter<'this> where Self: 'this;
            fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
                self.ts().predecessors(state)
            }
        }
        impl<Ts: Pointed> std::fmt::Debug for $name<Ts::Alphabet,  Ts::EdgeColor, Ts> where Ts::StateColor: std::fmt::Display {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use itertools::Itertools;
                use crate::prelude::IsEdge;
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
            fn remove_edges(
                &mut self,
                from: Self::StateIndex,
                on: <Self::Alphabet as Alphabet>::Expression,
            ) -> bool {
                self.ts_mut().remove_edges(from, on)
            }
        }
        impl<Ts: TransitionSystem> TransitionSystem
            for $name<Ts::Alphabet, Ts::EdgeColor, Ts>
        {
            type StateIndex = Ts::StateIndex;
            type EdgeColor = Ts::EdgeColor;
            type StateColor = Ts::StateColor;
            type EdgeRef<'this> = Ts::EdgeRef<'this> where Self: 'this;
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
pub trait MooreLike: Congruence
where
    StateColor<Self>: Color,
{
    /// Takes a reference to `self` and turns the underlying transition system into a [`MooreMachine`].
    fn as_moore(&self) -> MooreMachine<Self::Alphabet, Self::StateColor, Self::EdgeColor, &Self> {
        MooreMachine::from(self)
    }

    /// Consumes and thereby turns `self` into a [`MooreMachine`].
    fn into_moore(self) -> MooreMachine<Self::Alphabet, Self::StateColor, Self::EdgeColor, Self> {
        MooreMachine::from(self)
    }

    /// Runs the given `input` word in self. If the run is successful, the color of the state that it reaches
    /// is emitted (wrapped in a `Some`). For unsuccessful runs, `None` is returned.
    fn try_moore_map<W: FiniteWord<SymbolOf<Self>>>(&self, input: W) -> Option<Self::StateColor> {
        self.reached_state_color(input)
    }

    /// Obtains a vec containing the possible colors emitted by `self` (without duplicates).
    fn color_range(&self) -> Vec<Self::StateColor>
    where
        StateColor<Self>: Color,
    {
        self.reachable_state_indices()
            .map(|o| {
                self.state_color(o)
                    .expect("We know it is reachable and it must be colored")
            })
            .unique()
            .collect()
    }

    /// Builds a moore machine from a reference to `self`. Note that this allocates a new
    /// transition system, which is a copy of the underlying one.
    fn collect_moore(&self) -> AsMooreMachine<Self> {
        let ts = self.collect_pointed();
        MooreMachine {
            ts,
            _q: std::marker::PhantomData,
        }
    }

    /// Returns true if `self` is bisimilar to `other`, i.e. if the two moore machines
    /// produce the same output for each finite word. This is done by checking whether
    /// [`Self::moore_witness_non_bisimilarity`] returns `None`.
    fn moore_bisimilar<M>(&self, other: M) -> bool
    where
        M: MooreLike<Alphabet = Self::Alphabet, StateColor = Self::StateColor>,
        StateColor<Self>: Color,
    {
        self.moore_witness_non_bisimilarity(other).is_none()
    }

    /// Returns a witness for the non-bisimilarity of `self` and `other`, i.e. a finite word
    /// that produces different outputs in the two moore machines. If the two machines are
    /// bisimilar, `None` is returned.
    fn moore_witness_non_bisimilarity<M>(&self, other: M) -> Option<Vec<SymbolOf<Self>>>
    where
        M: MooreLike<Alphabet = Self::Alphabet, StateColor = Self::StateColor>,
        StateColor<Self>: Color,
    {
        let prod = self.ts_product(other);
        for (mr, idx) in prod.minimal_representatives() {
            let (c, d) = prod.state_color(idx).unwrap();
            if c != d {
                return Some(mr);
            }
        }
        None
    }

    /// Decomposes `self` into a sequence of DFAs, where the i-th DFA accepts all words which
    /// produce a color less than or equal to i.
    fn decompose_dfa(&self) -> Vec<DFA<Self::Alphabet>>
    where
        StateColor<Self>: Color,
    {
        self.color_range()
            .into_iter()
            .sorted()
            .map(|i| self.color_or_below_dfa(i))
            .collect()
    }

    /// Builds a DFA that accepts all words which emit a color less than or equal to `color`.
    fn color_or_below_dfa(&self, color: Self::StateColor) -> DFA<Self::Alphabet>
    where
        StateColor<Self>: Color,
    {
        self.map_state_colors(|o| o <= color)
            .erase_edge_colors()
            .dfa_minimized()
            .collect_dfa()
    }
}
impl<Ts: Congruence> MooreLike for Ts where StateColor<Ts>: Color {}
