use std::{
    collections::BTreeSet,
    fmt::{Debug, Display},
};

use itertools::Itertools;
use owo_colors::OwoColorize;

use crate::{
    algorithms::minimize_dfa,
    alphabet::{Alphabet, HasAlphabet, Symbol, SymbolOf},
    prelude::Simple,
    ts::{
        finite::{InfinityColors, ReachedColor},
        operations::{MapStateColor, MatchingProduct, Product},
        EdgeColor, FiniteState, FiniteStatesIterType, HasFiniteStates, HasMutableStates, HasStates,
        IndexType, Pointed, Quotient, Sproutable, StateColor, TransitionSystem, BTS,
    },
    word::{Normalized, OmegaWord},
    Color, FiniteLength, InfiniteLength, Length, Word,
};

/// Wrapper around a [`TransitionSystem`] with a designated initial state.
#[derive(Clone, PartialEq)]
pub struct WithInitial<Ts: TransitionSystem>(Ts, Ts::StateIndex);

impl<Ts> std::fmt::Debug for WithInitial<Ts>
where
    Ts: TransitionSystem + FiniteState + Debug,
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

impl<'a, Ts: TransitionSystem + HasFiniteStates<'a>> HasFiniteStates<'a> for WithInitial<Ts> {
    type StateIndicesIter = Ts::StateIndicesIter;
}

impl<Ts: FiniteState> FiniteState for WithInitial<Ts> {
    fn state_indices(&self) -> FiniteStatesIterType<'_, Self> {
        self.ts().state_indices()
    }
}

impl<A, C, Q> WithInitial<BTS<A, Q, C, usize>>
where
    A: Alphabet,
    C: Color,
    Q: Color,
{
    /// Creates a new instance for the given `alphabet`.
    pub fn new(alphabet: A) -> Self
    where
        StateColor<Self>: Default,
    {
        let mut ts = BTS::new(alphabet);
        let initial = ts.add_state(<StateColor<Self> as Default>::default());
        Self(ts, initial)
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

impl<Ts: TransitionSystem> WithInitial<Ts> {
    /// Returns a reference to the underlying [`TransitionSystem`].
    pub fn ts(&self) -> &Ts {
        &self.0
    }

    /// Returns a mutable reference to the underlying [`TransitionSystem`].
    pub fn ts_mut(&mut self) -> &mut Ts {
        &mut self.0
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
impl<Ts: TransitionSystem + HasAlphabet> HasAlphabet for WithInitial<Ts> {
    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.0.alphabet()
    }
}

impl<Ts: TransitionSystem> Pointed for WithInitial<Ts> {
    fn initial(&self) -> Self::StateIndex {
        self.1
    }
}

/// A Moore machine is simply a [`TransitionSystem`] with a designated initial state, which has colors on states.
pub type MooreMachine<A, Q = usize, Idx = usize> = WithInitial<BTS<A, Q, (), Idx>>;
/// A Mealy machine is a [`TransitionSystem`] with a designated initial state and colors on edges.
pub type MealyMachine<A, C = usize, Idx = usize> = WithInitial<BTS<A, (), C, Idx>>;

/// A [`MooreMachine`] which outputs booleans.
pub type DFA<A = Simple, Idx = usize> = MooreMachine<A, bool, Idx>;
/// A [`MealyMachine`] that ouputs booleans.
pub type DBA<A = Simple, Idx = usize> = MealyMachine<A, bool, Idx>;
/// A [`MealyMachine`] that ouputs non-negative integers.
pub type DPA<A = Simple, Idx = usize> = MealyMachine<A, usize, Idx>;
/// A variant of [`DBA`] which has colors on its states.
pub type SBDBA<A = Simple, Idx = usize> = MooreMachine<A, bool, Idx>;
/// A variant of [`DPA`] which has colors on states.
pub type SBDPA<A = Simple, Idx = usize> = MooreMachine<A, usize, Idx>;

/// Implementors of this trait can take in a word and transform it into some kind of output.
pub trait Transformer<S, Len: Length> {
    /// The object that results from the transformation.
    type Output;
    /// Transform the given word into an object of type [`Self::Output`].
    fn transform<W: Word<Symbol = S, Length = Len>>(&self, input: W) -> Self::Output;
}

impl<Ts> Transformer<SymbolOf<Ts>, FiniteLength> for Ts
where
    Ts: TransitionSystem + Pointed,
    Ts::StateColor: Clone + Default,
{
    type Output = Ts::StateColor;

    fn transform<W: Word<Symbol = SymbolOf<Ts>, Length = FiniteLength>>(
        &self,
        input: W,
    ) -> Self::Output {
        if let Some(ReachedColor(c)) = self.induced(&input, self.initial()) {
            c
        } else {
            Default::default()
        }
    }
}

impl<Ts> Transformer<SymbolOf<Ts>, InfiniteLength> for Ts
where
    Ts: TransitionSystem + Pointed,
    Ts::EdgeColor: Clone,
{
    type Output = BTreeSet<Ts::EdgeColor>;

    fn transform<W: Word<Symbol = SymbolOf<Ts>, Length = InfiniteLength>>(
        &self,
        input: W,
    ) -> Self::Output {
        if let Some(InfinityColors(c)) = self.induced(&input, self.initial()) {
            c
        } else {
            unreachable!()
        }
    }
}

/// Implementors of this trait are able to classify objects of type `X`.
pub trait Classifies<X> {
    /// Given an object of type `X`, classify it, i.e. return a boolean. This is e.g. implemented by automata,
    /// which classify words and return `true` iff the given word is accepted.
    fn classify(&self, x: X) -> bool;
}

impl<A: Alphabet, Idx: IndexType> Classifies<Normalized<A::Symbol, InfiniteLength>> for DBA<A, Idx>
where
    A::Symbol: Clone + Ord,
{
    fn classify(&self, x: Normalized<A::Symbol, InfiniteLength>) -> bool {
        match self.omega_run(self.initial(), x.initial_segment(), x.repeating_segment()) {
            Ok(path) => path.infinity_set(self).contains(&true),
            Err(_) => false,
        }
    }
}

/// Implementors of this trait are acceptors of sequences of symbols `S` with length `K`
pub trait Acceptor<S, K> {
    /// Returns true if and only if `self` accepts the given `word`.
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = K, Symbol = S>;
}

impl<S: Symbol, T: Transformer<S, FiniteLength, Output = bool>> Acceptor<S, FiniteLength> for T {
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = FiniteLength, Symbol = S>,
    {
        self.transform(word)
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

impl<S, T> Acceptor<S, InfiniteLength> for T
where
    S: Symbol,
    T: Transformer<S, InfiniteLength>,
    T::Output: ReducesTo<bool>,
{
    fn accepts<W>(&self, word: W) -> bool
    where
        W: Word<Length = InfiniteLength, Symbol = S>,
    {
        let transformed = self.transform(word);
        transformed.reduce()
    }
}

type DfaProductReduced<L, R> = MapStateColor<MatchingProduct<L, R>, fn((bool, bool)) -> bool>;

/// Iterator over the accepting states of a [`TransitionSystem`] that have a certain coloring.
pub struct StatesWithColor<'a, Ts: TransitionSystem + FiniteState> {
    ts: &'a Ts,
    iter: FiniteStatesIterType<'a, Ts>,
    color: Ts::StateColor,
}

impl<'a, Ts: TransitionSystem + FiniteState> StatesWithColor<'a, Ts> {
    /// Creates a new instance for the given transition system and color.
    pub fn new(ts: &'a Ts, color: Ts::StateColor) -> Self {
        Self {
            iter: ts.state_indices(),
            ts,
            color,
        }
    }
}

impl<'a, Ts: TransitionSystem + FiniteState> Clone for StatesWithColor<'a, Ts> {
    fn clone(&self) -> Self {
        Self {
            ts: self.ts,
            iter: self.ts.state_indices(),
            color: self.color.clone(),
        }
    }
}

impl<'a, Ts: TransitionSystem<StateColor = bool> + FiniteState> Iterator
    for StatesWithColor<'a, Ts>
{
    type Item = Ts::StateIndex;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.find(|&index| self.ts.state_color(index).unwrap())
    }
}

/// This trait is (automatically) implemented by everything which can be viewed as a [`DFA`].
pub trait DFALike: TransitionSystem<StateColor = bool> + Pointed + FiniteState
// + Acceptor<SymbolOf<Self>, FiniteLength>
// + Transformer<SymbolOf<Self>, FiniteLength, Output = bool>
{
    /// Returns the indices of all states that are accepting.
    fn accepting_states(&self) -> StatesWithColor<'_, Self>
    where
        Self: FiniteState,
    {
        StatesWithColor::new(self, true)
    }

    /// Returns the indices of all states that are rejecting.
    fn rejecting_states(&self) -> StatesWithColor<'_, Self>
    where
        Self: FiniteState,
    {
        StatesWithColor::new(self, false)
    }

    /// Minimizes `self` using Hopcroft's partition refinement algorithm.
    fn minimize(self) -> Quotient<Self>
    where
        Self: FiniteState,
    {
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
    Ts: TransitionSystem<StateColor = bool> + Pointed + FiniteState + Sized // + Acceptor<SymbolOf<Self>, FiniteLength>
                                                                            // + Transformer<SymbolOf<Self>, FiniteLength, Output = bool>
{
}

/// Similar to [`IsDfa`], this trait is supposed to be (automatically) implemented by everything that can be viewed
/// as a [`crate::DBA`].
pub trait DBALike:
    TransitionSystem<EdgeColor = bool>
    + Pointed
    + Sized
    + Acceptor<SymbolOf<Self>, InfiniteLength>
    + Transformer<SymbolOf<Self>, InfiniteLength, Output = BTreeSet<bool>>
{
    /// Tries to identify a word which is accepted by `self`. If such a word exists, it returns it and otherwise
    /// the function gives back `None`.
    fn dba_give_word(&self) -> Option<OmegaWord<SymbolOf<Self>, InfiniteLength>>
    where
        Self: FiniteState,
    {
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
    fn dba_is_empty(&self) -> bool
    where
        Self: FiniteState,
    {
        self.dba_give_word().is_none()
    }
}

impl<Ts> DBALike for Ts where
    Ts: TransitionSystem<EdgeColor = bool>
        + Pointed
        + Sized
        + Acceptor<SymbolOf<Self>, InfiniteLength>
        + Transformer<SymbolOf<Self>, InfiniteLength, Output = BTreeSet<bool>>
{
}

/// Trait that should be implemented by every object that can be viewed as a [`crate::DPA`].
pub trait DPALike:
    TransitionSystem<EdgeColor = usize>
    + Pointed
    + Sized
    + Acceptor<SymbolOf<Self>, InfiniteLength>
    + Transformer<SymbolOf<Self>, InfiniteLength, Output = BTreeSet<usize>>
{
}

impl<Ts> DPALike for Ts where
    Ts: TransitionSystem<EdgeColor = usize>
        + Pointed
        + Sized
        + Acceptor<SymbolOf<Self>, InfiniteLength>
        + Transformer<SymbolOf<Self>, InfiniteLength, Output = BTreeSet<usize>>
{
}

/// Implemented by objects which can be viewed as a MealyMachine, i.e. a finite transition system
/// which has outputs of type usize on its edges.
pub trait IsMealy: TransitionSystem<EdgeColor = usize> + Pointed + Sized {}
impl<Ts: TransitionSystem<EdgeColor = usize> + Pointed + Sized> IsMealy for Ts {}

/// Implemented by objects that can be viewed as MooreMachines, i.e. finite transition systems
/// that have usize annotated/outputting states.
pub trait IsMoore: TransitionSystem<StateColor = usize> + Pointed + Sized {
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
    fn decompose_dfa(&self) -> Vec<DFA<Self::Alphabet>>
    where
        Self: FiniteState,
    {
        self.color_range()
            .into_iter()
            .map(|i| (&self).color_or_below_dfa(i))
            .collect()
    }

    /// Builds a DFA that accepts all words which emit a color less than or equal to `color`.
    fn color_or_below_dfa(&self, color: usize) -> DFA<Self::Alphabet>
    where
        Self: FiniteState,
    {
        self.map_state_colors(|o| o <= color)
            .erase_edge_colors()
            .minimize()
            .erase_edge_colors()
            .map_state_colors(|o| o.contains(&true))
            .collect_with_initial()
    }
}
impl<Ts: TransitionSystem<StateColor = usize> + Pointed + Sized> IsMoore for Ts {}

#[cfg(test)]
mod tests {
    use super::{DFALike, IsMoore, WithInitial};
    use crate::{prelude::*, ts::BTS};

    #[test]
    fn mealy_color_or_below() {
        let mut mm: WithInitial<BTS<_, usize, _>> = MooreMachine::new(alphabet!(simple 'a', 'b'));
        let a = mm.initial();
        mm.set_initial_color(2usize);
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
        let mut dba = super::DBA::new(Simple::from_iter(['a', 'b']));
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
        dfa.state_mut(s0).unwrap().set_color(true);
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

        let notdfa = (&dfa).negation();
        assert!(!notdfa.accepts("ababab"));
        assert!(notdfa.accepts("a"));

        let intersection = (&dfa).intersection(&notdfa);
        assert!(!intersection.accepts("ababab"));
        assert!(!intersection.accepts("a"));

        let union = (&dfa).union(&notdfa);
        assert!(union.accepts("ababab"));
        assert!(union.accepts("a"));
    }
}
