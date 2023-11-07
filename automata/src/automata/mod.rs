use std::{
    collections::BTreeSet,
    fmt::{Debug, Display},
    marker::PhantomData,
};

use impl_tools::autoimpl;
use itertools::Itertools;
use owo_colors::OwoColorize;

use crate::{
    algorithms::minimize_dfa,
    alphabet::{Alphabet, HasAlphabet, Symbol, SymbolOf},
    prelude::*,
    prelude::{ExpressionOf, Simple},
    ts::{
        finite::{InfinityColors, ReachedColor},
        operations::{MapStateColor, MatchingProduct, Product, ProductIndex, ProductTransition},
        transition_system::IsTransition,
        EdgeColor, HasMutableStates, HasStates, IndexType, Pointed, Quotient, Sproutable,
        StateColor, TransitionSystem, BTS,
    },
    word::{Normalized, OmegaWord},
    Color, FiniteLength, InfiniteLength, Length, Word,
};

#[macro_use]
mod moore;
pub use moore::{IntoMooreMachine, MooreMachine};

mod mealy;
pub use mealy::{IntoDBA, IntoDPA, IntoMealyMachine, MealyMachine, DBA, DPA};

mod dfa;
pub use dfa::{IntoDFA, DFA};

/// Wrapper around a [`TransitionSystem`] with a designated initial state.
#[derive(Clone, PartialEq)]
pub struct WithInitial<Ts: TransitionSystem>(Ts, Ts::StateIndex);

impl<Ts: TransitionSystem> WithInitial<Ts> {
    /// Gives a reference to the underlying transition system.
    pub fn ts(&self) -> &Ts {
        &self.0
    }
    /// Returns a mutable reference to the underlying transition system.
    pub fn ts_mut(&mut self) -> &mut Ts {
        &mut self.0
    }
}

impl<Ts: TransitionSystem> Pointed for WithInitial<Ts> {
    fn initial(&self) -> Self::StateIndex {
        self.1
    }
}

impl<Ts> std::fmt::Debug for WithInitial<Ts>
where
    Ts: TransitionSystem + Debug,
    Ts::StateColor: Debug,
    Ts::EdgeColor: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Initial state {} with table\n{}",
            self.initial(),
            self.build_transition_table(|index, color| {
                if index == self.initial() {
                    format!("{} : {:?}", index.to_string().bold(), color)
                } else {
                    format!("{} : {:?}", index, color)
                }
            })
        )
    }
}

impl<Ts: TransitionSystem> From<(Ts, Ts::StateIndex)> for WithInitial<Ts> {
    fn from(value: (Ts, Ts::StateIndex)) -> Self {
        Self(value.0, value.1)
    }
}

impl<A, C, Q> WithInitial<BTS<A, Q, C, usize>>
where
    A: Alphabet,
    C: Color,
    Q: Color,
{
    /// Takes an alphabet and a color and constructs a [`WithInitial`] instance with the given alphabet, no
    /// transitions and a single initial state with the given color.
    pub fn with_initial_color(alphabet: A, color: Q) -> Self {
        let mut ts = BTS::new(alphabet);
        let initial = ts.add_state(color);
        Self(ts, initial)
    }

    /// Creates a new instance for the given `alphabet`.
    pub fn new(alphabet: A) -> Self
    where
        StateColor<Self>: Default,
    {
        Self::with_initial_color(alphabet, Q::default())
    }

    /// Creats a new instance for the given `alphabet` and ensures capacity for at least `size` states.
    pub fn with_capacity(alphabet: A, size: usize) -> Self
    where
        StateColor<Self>: Default,
    {
        let mut ts = BTS::with_capacity(alphabet, size);
        let initial = ts.add_state(<StateColor<Self> as Default>::default());
        Self(ts, initial)
    }
}

impl<Ts: TransitionSystem + Sproutable> Sproutable for WithInitial<Ts>
where
    StateColor<Ts>: Default,
{
    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        let mut ts = Ts::new_for_alphabet(alphabet);
        let initial = ts.add_state(<StateColor<Ts> as Default>::default());
        Self(ts, initial)
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

    fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
        self.ts_mut().add_state(color)
    }

    fn set_state_color<X: Into<StateColor<Self>>>(&mut self, index: Self::StateIndex, color: X) {
        self.ts_mut().set_state_color(index, color)
    }

    type ExtendStateIndexIter = Ts::ExtendStateIndexIter;
    fn extend_states<I: IntoIterator<Item = StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter {
        self.ts_mut().extend_states(iter)
    }
}
impl<Ts: TransitionSystem + HasStates> HasStates for WithInitial<Ts> {
    type State<'this> = Ts::State<'this>
        where
            Self: 'this;

    type StatesIter<'this> = Ts::StatesIter<'this>
        where
            Self: 'this;

    fn state(&self, index: Self::StateIndex) -> Option<Self::State<'_>> {
        self.ts().state(index)
    }

    fn states_iter(&self) -> Self::StatesIter<'_> {
        self.ts().states_iter()
    }
}
impl<Ts: TransitionSystem + HasMutableStates> HasMutableStates for WithInitial<Ts> {
    type StateMut<'this>  = Ts::StateMut<'this> where Self:'this;

    fn state_mut(&mut self, index: Self::StateIndex) -> Option<Self::StateMut<'_>> {
        self.ts_mut().state_mut(index)
    }
}

/// Type alias for the unit type. Purpose is mainly to be used in Macros, as `()` is more difficult to
/// handle than a simple alphabetic identifier.
pub type NoColor = ();

#[allow(missing_docs)]
macro_rules! impl_automaton_type {
    ($name:ident, onstates $color:ident) => {
        impl_automaton_type!($name, $color, NoColor);
    };
    ($name:ident, onedges $color:ident) => {
        impl_automaton_type!($name, NoColor, $color);
    };
    ($name:ident, $color:ident, $edgecolor:ident) => {
        paste::paste! {
            /// Type alias to allow conversion of a transition system into an automaton of the type.
            pub type [< Into $name >]<Ts> = $name<<Ts as HasAlphabet>::Alphabet, <Ts as TransitionSystem>::StateColor, <Ts as TransitionSystem>::EdgeColor, Ts>;
        }

        #[allow(missing_docs)]
        #[derive(Clone)]
        pub struct $name<
            A = Simple,
            Q = $color,
            C = $edgecolor,
            Ts = WithInitial<BTS<A, Q, C, usize>>,
        > {
            ts: Ts,
            _alphabet: PhantomData<(A, Q, C)>,
        }
        impl<A: Alphabet>
            $name<A, $color, $edgecolor, WithInitial<BTS<A, $color, $edgecolor, usize>>>
        {
            /// Creates a new automaton from a given alphabet.
            pub fn new(alphabet: A) -> $name<A, $color, $edgecolor, WithInitial<BTS<A, $color, $edgecolor, usize>>> {
                $name {
                    ts: WithInitial::new(alphabet),
                    _alphabet: PhantomData,
                }
            }
        }
        impl<Ts: TransitionSystem> $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts> {
            /// Returns a reference to the underlying transition system.
            pub fn ts(&self) -> &Ts {
                &self.ts
            }
            /// Returns a mutable reference to the underlying transition system.
            pub fn ts_mut(&mut self) -> &mut Ts {
                &mut self.ts
            }
        }
        impl<Ts: TransitionSystem<StateColor = $color>> From<Ts>
            for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
        {
            fn from(ts: Ts) -> Self {
                Self {
                    ts,
                    _alphabet: PhantomData,
                }
            }
        }
        impl<Ts: PredecessorIterable> PredecessorIterable
            for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
        {
            type PreTransitionRef<'this> = Ts::PreTransitionRef<'this> where Self: 'this;
            type EdgesToIter<'this> = Ts::EdgesToIter<'this> where Self: 'this;
            fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
                self.ts().predecessors(state)
            }
        }
        impl<Ts: Pointed> std::fmt::Debug for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                writeln!(
                    f,
                    "Initial state {} with states {}",
                    self.ts().initial(),
                    self.ts().state_indices().map(|i| format!("{i}")).join(", ")
                )
            }
        }
        impl<Ts: Sproutable> Sproutable
            for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
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
                    _alphabet: PhantomData,
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
        impl<Ts: TransitionSystem> HasAlphabet for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts> {
            type Alphabet = Ts::Alphabet;
            fn alphabet(&self) -> &Self::Alphabet {
                self.ts.alphabet()
            }
        }
        impl<Ts: TransitionSystem> TransitionSystem
            for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
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
        impl<Ts: Pointed> Pointed for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts> {
            fn initial(&self) -> Self::StateIndex {
                self.ts().initial()
            }
        }
    };
}

#[allow(missing_docs)]
// impl_automaton_type!(DFA, onstates bool);
// impl_automaton_type!(MooreMachine, onstates usize);
impl_automaton_type!(StateBasedDBA, onstates bool);
impl_automaton_type!(StateBasedDPA, onstates usize);

/// Implementors of this trait are acceptors of sequences of symbols `S` with length `K`
#[autoimpl( for<T: trait> &T, &mut T)]
pub trait Acceptor<S, K> {
    /// Returns true if and only if `self` accepts the given `word`.
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = K, Symbol = S>;

    /// Verifies that the acceptor is consistent with the given iterator of words and their acceptance.
    fn consistent_with<W, I>(&self, iter: I) -> bool
    where
        W: Word<Length = K, Symbol = S>,
        I: IntoIterator<Item = (W, bool)>,
    {
        iter.into_iter().all(|(w, b)| self.accepts(w) == b)
    }
}

/// Helper trait to convert from boolean to usize. Normally, a `true` value corresponds to `1`, while
/// a `false` value corresponds to `0`. This does not really work well with min-even parity conditions
/// so this helper trait is introduced.
// TODO: remove this if possible.
pub trait ReducesTo<T = bool> {
    /// Reduce `self` to a value of type `T`.
    fn reduce(self) -> T;
}

impl ReducesTo<bool> for bool {
    fn reduce(self) -> bool {
        self
    }
}

impl ReducesTo<bool> for usize {
    fn reduce(self) -> bool {
        (self % 2) == 0
    }
}

impl ReducesTo<bool> for BTreeSet<bool> {
    fn reduce(self) -> bool {
        self.into_iter().any(|x| x)
    }
}

impl ReducesTo<bool> for BTreeSet<usize> {
    fn reduce(self) -> bool {
        self.into_iter().min().unwrap() % 2 == 0
    }
}

type DfaProductReduced<L, R> = MapStateColor<MatchingProduct<L, R>, fn((bool, bool)) -> bool>;

/// Iterator over the accepting states of a [`TransitionSystem`] that have a certain coloring.
pub struct StatesWithColor<'a, Ts: TransitionSystem> {
    ts: &'a Ts,
    iter: Ts::StateIndices<'a>,
    color: Ts::StateColor,
}

impl<'a, Ts: TransitionSystem> StatesWithColor<'a, Ts> {
    /// Creates a new instance for the given transition system and color.
    pub fn new(ts: &'a Ts, color: Ts::StateColor) -> Self {
        Self {
            iter: ts.state_indices(),
            ts,
            color,
        }
    }
}

impl<'a, Ts: TransitionSystem> Clone for StatesWithColor<'a, Ts> {
    fn clone(&self) -> Self {
        Self {
            ts: self.ts,
            iter: self.ts.state_indices(),
            color: self.color.clone(),
        }
    }
}

impl<'a, Ts: TransitionSystem<StateColor = bool>> Iterator for StatesWithColor<'a, Ts> {
    type Item = Ts::StateIndex;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .find(|&index| self.ts.state_color(index).unwrap() == self.color)
    }
}

/// This trait is (automatically) implemented by everything which can be viewed as a [`DFA`].
pub trait DFALike: TransitionSystem<StateColor = bool> + Pointed
// + Acceptor<SymbolOf<Self>, FiniteLength>
// + Transformer<SymbolOf<Self>, FiniteLength, Output = bool>
{
    /// Consumes and turns `self` into a [`DFA`].
    fn into_dfa(self) -> DFA<Self::Alphabet, Self::EdgeColor, Self> {
        DFA::from(self)
    }

    /// Uses a reference to `self` for creating a [`DFA`].
    fn as_dfa(&self) -> DFA<Self::Alphabet, Self::EdgeColor, &Self> {
        DFA::from(self)
    }

    /// Returns the indices of all states that are accepting.
    fn accepting_states(&self) -> StatesWithColor<'_, Self> {
        StatesWithColor::new(self, true)
    }

    /// Returns the indices of all states that are rejecting.
    fn rejecting_states(&self) -> StatesWithColor<'_, Self> {
        StatesWithColor::new(self, false)
    }

    /// Minimizes `self` using Hopcroft's partition refinement algorithm.
    fn minimize(self) -> Quotient<Self> {
        minimize_dfa(self)
    }

    /// Checks whether `self` is equivalent to `other`, i.e. whether the two DFAs accept
    /// the same language. This is done by negating `self` and then verifying that the intersection
    /// of the negated automaton with `other` is empty.
    fn equivalent<D: DFALike<Alphabet = Self::Alphabet>>(&self, other: D) -> bool {
        self.negation()
            .intersection(other)
            .dfa_give_word()
            .is_none()
    }

    /// Tries to construct a (finite) word witnessing that the accepted language is empty. If such a word exists,
    /// the function returns it, otherwise `None`.
    fn dfa_give_word(&self) -> Option<Vec<SymbolOf<Self>>> {
        self.minimal_representatives().find_map(|(mr, index)| {
            if self
                .state_color(index)
                .expect("Every state must be colored")
            {
                Some(mr)
            } else {
                None
            }
        })
    }

    /// Returns true if and only if the accepted language is empty.
    fn dfa_is_empty(&self) -> bool {
        self.dfa_give_word().is_none()
    }

    /// Computes the union of `self` with the given `other` object (that can be viewed as a DFA) through
    /// a simple product construction.
    fn union<Ts: DFALike<Alphabet = Self::Alphabet>>(
        self,
        other: Ts,
    ) -> DfaProductReduced<Self, Ts> {
        self.ts_product(other).map_state_colors(|(a, b)| a || b)
    }

    /// Computes the intersection of `self` with the given `other` object (that can be viewed as a DFA) through
    /// a simple product construction.
    fn intersection<Ts: DFALike<Alphabet = Self::Alphabet>>(
        self,
        other: Ts,
    ) -> DfaProductReduced<Self, Ts> {
        self.ts_product(other).map_state_colors(|(a, b)| a && b)
    }

    /// Computes the negation of `self` by swapping accepting and non-accepting states.
    fn negation(self) -> MapStateColor<Self, fn(bool) -> bool> {
        self.map_state_colors(|x| !x)
    }
}

impl<Ts> DFALike for Ts where
    Ts: TransitionSystem<StateColor = bool> + Pointed + Sized // + Acceptor<SymbolOf<Self>, FiniteLength>
                                                              // + Transformer<SymbolOf<Self>, FiniteLength, Output = bool>
{
}

/// Similar to [`IsDfa`], this trait is supposed to be (automatically) implemented by everything that can be viewed
/// as a [`crate::DBA`].
pub trait DBALike: TransitionSystem<EdgeColor = bool> + Pointed {
    /// Uses a reference to `self` for creating a [`DBA`].
    fn as_dba(&self) -> IntoDBA<&Self> {
        DBA::from(self)
    }

    /// Consumes `self` and returns a [`DBA`].
    fn into_dba(self) -> IntoDBA<Self> {
        DBA::from(self)
    }

    /// Tries to identify a word which is accepted by `self`. If such a word exists, it returns it and otherwise
    /// the function gives back `None`.
    fn dba_give_word(&self) -> Option<OmegaWord<SymbolOf<Self>, InfiniteLength>> {
        for good_scc in self
            .sccs()
            .iter()
            .filter(|scc| self.is_reachable(*scc.first().unwrap()))
        {
            if let Some(full_word) = good_scc.maximal_word() {
                let InfinityColors(colors) = self
                    .induced(&full_word, self.initial())
                    .expect("word is valid");
                if colors.contains(&true) {
                    let base = self
                        .word_from_to(self.initial(), *good_scc.first().unwrap())
                        .expect("we know this is reachable");
                    return Some(OmegaWord::from_parts(base, full_word));
                }
            }
        }
        None
    }

    /// Returns `true` if and only if `self` accepts a non-empty language.
    fn dba_is_empty(&self) -> bool {
        self.dba_give_word().is_none()
    }
}

impl<Ts> DBALike for Ts where Ts: TransitionSystem<EdgeColor = bool> + Pointed {}

/// Trait that should be implemented by every object that can be viewed as a [`crate::DPA`].
pub trait DPALike: TransitionSystem<EdgeColor = usize> + Pointed {
    /// Consumes `self` and returns a [`DPA`] from the transition system underlying `self`.
    fn into_dpa(self) -> IntoDPA<Self> {
        DPA::from(self)
    }

    /// Uses a reference to `self` for creating a [`DPA`] from the underlying transition system.
    fn as_dpa(&self) -> IntoDPA<&Self> {
        DPA::from(self)
    }
}

impl<Ts> DPALike for Ts where Ts: TransitionSystem<EdgeColor = usize> + Pointed {}

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

    /// Attempts to run the given finite word in `self`, returning the coclor of the last transition that
    /// is taken wrapped in `Some`. If no successful run on `input` is possible, the function returns `None`.
    fn try_mealy_map<W: Word<Symbol = SymbolOf<Self>, Length = FiniteLength>>(
        &self,
        input: W,
    ) -> Option<C> {
        self.finite_run_from(self.initial(), &input.finite_to_vec())
            .ok()
            .and_then(|p| p.last_transition_color(self))
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

/// Implemented by objects that can be viewed as MooreMachines, i.e. finite transition systems
/// that have usize annotated/outputting states.
pub trait MooreLike<Q: Color>: TransitionSystem<StateColor = Q> + Pointed {
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
    fn try_moore_map<W: Word<Symbol = SymbolOf<Self>, Length = FiniteLength>>(
        &self,
        input: W,
    ) -> Option<Q> {
        self.finite_run_from(self.initial(), &input.finite_to_vec())
            .ok()
            .map(|p| p.reached_state_color(self))
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
            .rev()
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
impl<Ts: TransitionSystem + Pointed> MooreLike<Ts::StateColor> for Ts {}

#[cfg(test)]
mod tests {
    use super::{Acceptor, DFALike, MooreLike, WithInitial};
    use crate::{automata::NoColor, prelude::*, ts::BTS};

    #[test]
    fn mealy_color_or_below() {
        let mut mm = MooreMachine::new(alphabet!(simple 'a', 'b'), 2);
        let a = mm.initial();
        let b = mm.add_state(1usize);
        let c = mm.add_state(1usize);
        let d = mm.add_state(0usize);
        mm.add_edge(a, 'a', b, ());
        mm.add_edge(a, 'b', c, ());
        mm.add_edge(b, 'a', c, ());
        mm.add_edge(b, 'b', c, ());
        mm.add_edge(c, 'a', d, ());
        mm.add_edge(c, 'b', c, ());
        mm.add_edge(d, 'a', d, ());
        mm.add_edge(d, 'b', d, ());

        let dfas = mm.decompose_dfa();
        let dfa2 = &dfas[2];
        let dfa0 = &dfas[0];

        assert!(dfa0.accepts(&""));
        assert!(dfa0.accepts(&"b"));
        assert!(!dfa2.accepts(&"b"));
        assert!(dfa2.accepts(&"ba"));
    }

    #[test]
    fn dbas() {
        let mut dba = super::DBA::new(Simple::from_iter(['a', 'b']), ());
        let q1 = dba.add_state(());
        let q0 = dba.initial();

        let _e0 = dba.add_edge(q0, 'a', q1, true);
        let _e1 = dba.add_edge(q0, 'b', q0, false);
        let _e2 = dba.add_edge(q1, 'a', q1, true);
        let _e3 = dba.add_edge(q1, 'b', q0, false);
        assert!(dba.accepts(OmegaWord::new("abb", InfiniteLength(3, 0))));
        assert!(!dba.accepts(OmegaWord::new("b", InfiniteLength(1, 0))));
        assert!(dba.accepts(upw!("a")));
        assert!(!dba.accepts(upw!("b")));

        assert!(!dba.dba_is_empty());
        println!("{:?}", dba.dba_give_word());

        println!("{:?}", &dba);
    }

    #[test]
    fn dfas_and_boolean_operations() {
        let mut dfa = super::DFA::new(Simple::new(['a', 'b']));
        let s0 = dfa.initial();
        dfa.set_initial_color(true);
        let s1 = dfa.add_state(false);
        let _e0 = dfa.add_edge(s0, 'a', s1, ());
        let _e1 = dfa.add_edge(s0, 'b', s0, ());
        let _e2 = dfa.add_edge(s1, 'a', s1, ());
        let _e3 = dfa.add_edge(s1, 'b', s0, ());

        assert!(!dfa.dfa_is_empty());
        assert_eq!(dfa.dfa_give_word(), Some(vec![]));

        let _dfb = dfa.clone();

        assert!(dfa.accepts("ababab"));
        assert!(!dfa.accepts("a"));

        let notdfa = (&dfa).negation().into_dfa();
        assert!(!notdfa.accepts("ababab"));
        assert!(notdfa.accepts("a"));

        let intersection = (&dfa).intersection(&notdfa).into_dfa();
        assert!(!intersection.accepts("ababab"));
        assert!(!intersection.accepts("a"));

        let union = (&dfa).union(&notdfa).into_dfa();
        assert!(union.accepts("ababab"));
        assert!(union.accepts("a"));
    }
}
