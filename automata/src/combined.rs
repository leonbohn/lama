use std::borrow::Borrow;

use hoars::HoaSymbol;
use itertools::Itertools;

use crate::{
    acceptance::{
        AcceptanceCondition, BuchiCondition, ParityCondition, ReachabilityCondition, ToOmega,
    },
    congruence::CongruenceTrigger,
    helpers::MooreMachine,
    output::{Assignment, IntoAssignments, Mapping, MutableTransformer},
    ts::{
        transitionsystem::{States, Transitions},
        Growable, HasInput, HasStates, InputOf, IntoStates, IntoTransitions, Pointed, Shrinkable,
        Successor, TransitionReference, TransitionSystem,
    },
    AnonymousGrowable, Class, HasAlphabet, MealyMachine, OmegaAutomaton, RightCongruence, Set,
    State, Symbol, Transformer, Transition, Value, DBA, DFA,
};

/// Struct that represents the 'usual' automata, which is a combination of a transition system, a designated initial state and an acceptance condition.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Combined<TS: Successor, Acc> {
    pub(crate) ts: TS,
    pub(crate) initial: TS::Q,
    pub(crate) acc: Acc,
}

impl<TS: Successor, Acc> Combined<TS, Acc> {
    /// Returns a mutable reference to the underlying acceptance condition.
    pub fn acceptance_mut(&mut self) -> &mut Acc {
        &mut self.acc
    }

    /// Returns a reference to the underlying acceptance condition.
    pub fn acceptance(&self) -> &Acc {
        &self.acc
    }

    /// Returns a new [`Combined`] instance with the same transition system in which
    /// the acceptance condition is replaced by `acc`.
    pub fn with_acceptance<Bdd>(&self, acc: Bdd) -> Combined<TS, Bdd>
    where
        Bdd: AcceptanceCondition,
        TS: Clone,
    {
        Combined {
            ts: self.ts.clone(),
            initial: self.initial.clone(),
            acc,
        }
    }

    /// Constructs a new instance from the given transition system, initial state and acceptance condition.
    pub fn from_parts(ts: TS, initial: TS::Q, acc: Acc) -> Self {
        Self { ts, initial, acc }
    }

    /// Returns a reference to the underlying transition system.
    pub fn ts(&self) -> &TS {
        &self.ts
    }

    /// Returns a mutable reference to the underlying transition system.
    pub fn ts_mut(&mut self) -> &mut TS {
        &mut self.ts
    }

    /// Counts the number of unique states in the transition system.
    pub fn size(&self) -> usize
    where
        for<'a> &'a TS: IntoStates<Q = TS::Q>,
    {
        self.ts.into_states().count()
    }
}

impl<'a, TS: Successor + IntoTransitions, Acc> IntoTransitions for &'a Combined<TS, Acc> {
    type TransitionRef = TS::TransitionRef;

    type IntoTransitions = TS::IntoTransitions;

    fn into_transitions(self) -> Self::IntoTransitions {
        self.ts.into_transitions()
    }
}

impl<'a, TS: IntoStates, Acc> IntoStates for &'a Combined<TS, Acc> {
    type StateRef = TS::StateRef;

    type IntoStates = TS::IntoStates;

    fn into_states(self) -> Self::IntoStates {
        self.ts.into_states()
    }
}

impl<'a, TS: Successor, M: IntoAssignments> IntoAssignments for &'a Combined<TS, M> {
    type AssignmentRef = M::AssignmentRef;

    type Assignments = M::Assignments;

    fn into_assignments(self) -> Self::Assignments {
        self.acc.into_assignments()
    }
}

impl<TS: Successor, M: Transformer> Transformer for Combined<TS, M> {
    type Domain = M::Domain;

    type Range = M::Range;

    fn apply<R: std::borrow::Borrow<Self::Domain>>(&self, input: R) -> Self::Range {
        self.acc.apply(input)
    }
}

impl<TS: Successor, M: MutableTransformer> MutableTransformer for Combined<TS, M> {
    fn set_map<R: Borrow<Self::Domain>>(&mut self, of: R, to: Self::Range) -> Option<Self::Range> {
        self.acc.set_map(of, to)
    }
}

impl<TS: Successor + HasAlphabet<Alphabet = TS::Sigma>, Acc> HasAlphabet for Combined<TS, Acc> {
    type Alphabet = TS::Sigma;

    type AlphabetIter = TS::AlphabetIter;

    fn alphabet_iter(&self) -> Self::AlphabetIter {
        self.ts.alphabet_iter()
    }
}

impl<TS: Successor, Acc> HasInput for Combined<TS, Acc> {
    type Sigma = InputOf<TS>;

    type Input<'me> = itertools::Unique<TS::Input<'me>>
    where
        Self: 'me;

    fn input_alphabet(&self) -> Self::Input<'_> {
        self.ts.input_alphabet().unique()
    }
}

impl<TS, Acc> Pointed for Combined<TS, Acc>
where
    TS: Successor,
{
    fn initial(&self) -> Self::Q {
        self.initial.clone()
    }
}

impl<Q: State, S: Symbol, X: Value> MooreMachine<X, Q, S> {
    /// Function which maps the acceptance condition of a Moore machine to a new acceptance condition.
    /// It gets as parameter a function which is applied to every element of the acceptance mapping.
    pub fn map_acceptance<Y, F>(self, f: F) -> MooreMachine<Y, Q, S>
    where
        Y: Value,
        F: Fn(&X) -> Y + Copy,
    {
        let acc = self
            .acceptance()
            .into_assignments()
            .map(|x| x.map_right(f))
            .collect();
        Combined::from_parts(self.ts, self.initial, acc)
    }
}

impl<Q: State, S: Symbol, X: Value> MealyMachine<X, Q, S> {
    /// Function which maps the acceptance condition of a Mealy machine to a new acceptance condition.
    /// It gets as parameter a function which is applied to every element of the acceptance mapping.
    pub fn map_acceptance<Y, F>(self, f: F) -> MealyMachine<Y, Q, S>
    where
        Y: Value,
        F: Fn(&X) -> Y + Copy,
    {
        let acc = self
            .acceptance()
            .into_assignments()
            .map(|x| x.map_right(f))
            .collect();
        Combined::from_parts(self.ts, self.initial, acc)
    }
}

impl<Q: State, S: Symbol, Acc> Combined<TransitionSystem<Q, S>, Acc> {
    /// Converts the automaton to an [`OmegaAutomaton`], which boils down to simply
    /// replacing the acceptance condition with an [`OmegaCondition`].
    pub fn to_omega(&self) -> OmegaAutomaton<Q, S>
    where
        Acc: ToOmega<X = (Q, S)> + AcceptanceCondition,
    {
        self.with_acceptance(self.acceptance().to_omega())
    }
}

impl<Q: State, S: Symbol, C: Value> MealyMachine<C, Q, S> {
    /// Creates a mealy machine from an iterator of transitions annotated with their output.
    pub fn from_iter<
        X: Transition<S = S, Q = Q> + Assignment<Left = (Q, S), Right = C>,
        T: IntoIterator<Item = X>,
    >(
        iter: T,
        initial: Q,
    ) -> Self {
        let (ts, assignments) = iter
            .into_iter()
            .map(|x| {
                let transition = (x.source().clone(), x.sym().clone(), x.target().clone());
                let assignment = (x.left(), x.right());
                (transition, assignment)
            })
            .unzip();
        Self {
            ts,
            initial,
            acc: assignments,
        }
    }
}

impl<S: Symbol, Acc> From<Combined<TransitionSystem<Class<S>, S>, Acc>> for RightCongruence<S> {
    fn from(val: Combined<TransitionSystem<Class<S>, S>, Acc>) -> Self {
        RightCongruence(val.ts, Class::epsilon())
    }
}

impl<Q: State, S: Symbol> DFA<Q, S> {
    /// Creates a mealy machine from an iterator of transitions annotated with their output.
    pub fn from_parts_iters<
        X: Transition<S = S, Q = Q>,
        T: IntoIterator<Item = X>,
        A: IntoIterator<Item = Q>,
    >(
        transition_iter: T,
        accepting_iter: A,
        initial: Q,
    ) -> Self {
        let ts: TransitionSystem<Q, S> = transition_iter
            .into_iter()
            .map(|x| (x.source().clone(), x.sym().clone(), x.target().clone()))
            .collect();
        let accepting: Set<_> = accepting_iter.into_iter().collect();

        let acc = ts
            .states()
            .map(|x| (x.clone(), accepting.contains(x)))
            .collect();
        Self { ts, initial, acc }
    }

    /// Creates a [`DFA`] from an iterator of transitions and a given initial state.
    /// In the resulting automaton, all states are accepting.
    pub fn all_accepting_iters<X: Transition<S = S, Q = Q>, T: IntoIterator<Item = X>>(
        transition_iter: T,
        initial: Q,
    ) -> Self {
        let ts: TransitionSystem<Q, S> = transition_iter
            .into_iter()
            .map(|x| (x.source().clone(), x.sym().clone(), x.target().clone()))
            .collect();
        let acc = ts.states().map(|q| (q.clone(), true)).collect();

        Self { ts, initial, acc }
    }
}

#[allow(unused)]
impl<TS: Successor + Default + AnonymousGrowable, Acc: AcceptanceCondition + Default>
    Combined<TS, Acc>
{
    /// Creates a new instance with a single state that serves as the initial state.
    /// Note that the resulting automaton is not empty per se, it has a single initial state, which can be accessed via [`Combined::initial`].
    pub fn trivial() -> Self {
        let mut ts = TS::default();
        let initial = ts.add_new_state();
        let acc = Acc::default();
        Self { ts, initial, acc }
    }
}

impl<TS, Acc> HasStates for Combined<TS, Acc>
where
    TS: Successor,
{
    type Q = TS::Q;

    fn contains_state<X: Borrow<Self::Q>>(&self, state: X) -> bool {
        self.ts.contains_state(state)
    }
}

impl<TS: Successor, Acc> Successor for Combined<TS, Acc> {
    fn successor<X: Borrow<Self::Q>, Y: Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q> {
        self.ts.successor(from, on)
    }
}

impl<TS: Successor, Acc: AcceptanceCondition> AcceptanceCondition for Combined<TS, Acc> {
    type Induced = Acc::Induced;

    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        self.acc.is_accepting(induced)
    }
}

impl<TS: Growable, Acc: AcceptanceCondition> Growable for Combined<TS, Acc> {
    fn add_state(&mut self, state: &Self::Q) -> bool {
        self.ts.add_state(state)
    }

    fn add_transition<X: std::borrow::Borrow<Self::Q>, Y: std::borrow::Borrow<Self::Q>>(
        &mut self,
        from: X,
        on: crate::ts::InputOf<Self>,
        to: Y,
    ) -> Option<Self::Q> {
        self.ts.add_transition(from, on, to)
    }
}

impl<TS: AnonymousGrowable, Acc: AcceptanceCondition> AnonymousGrowable for Combined<TS, Acc> {
    fn add_new_state(&mut self) -> Self::Q {
        self.ts.add_new_state()
    }
}

impl<TS: Shrinkable, Acc: AcceptanceCondition> Shrinkable for Combined<TS, Acc> {
    fn remove_state(&mut self, state: Self::Q) -> Option<Self::Q> {
        self.ts.remove_state(state)
    }

    fn remove_transition(&mut self, from: Self::Q, on: Self::Sigma) -> Option<Self::Q> {
        self.ts.remove_transition(from, on)
    }
}

#[cfg(feature = "det")]
/// Type alias for a deterministic finite automaton, only available for crate feature `det`.
pub type Dfa<Q = u32, S = char> = Combined<TransitionSystem<Q, S>, ReachabilityCondition<Q>>;
#[cfg(feature = "det")]
/// Type alias for a deterministic Büchi automaton, only available for crate feature `det`.
pub type Dba<Q = u32, S = char> = Combined<TransitionSystem<Q, S>, BuchiCondition<(Q, S)>>;
#[cfg(feature = "det")]
/// Type alias for a deterministic parity automaton, only available for crate feature `det`.
pub type Dpa<Q = u32, S = char> = Combined<TransitionSystem<Q, S>, ParityCondition<(Q, S)>>;

/// Alias that makes working with HOA easier. This is the same as a [`Dpa`], but with [`HoaSymbol`] as the symbol type.
pub type HoaDpa<Q = u32, S = HoaSymbol> = Combined<TransitionSystem<Q, S>, ParityCondition<(Q, S)>>;
/// Alias that makes working with HOA easier. This is the same as a [`Dba`], but with [`HoaSymbol`] as the symbol type.
pub type HoaDba<Q = u32, S = HoaSymbol> = Combined<TransitionSystem<Q, S>, BuchiCondition<(Q, S)>>;

/// Alias that makes working with congruences easier, using [`RightCongruence`] as the transition system.
pub type CongruenceDba<S = char> =
    Combined<RightCongruence<S>, BuchiCondition<CongruenceTrigger<S>>>;
/// Alias that makes working with congruences easier, using [`RightCongruence`] as the transition system.
pub type CongruenceDpa<S = char> =
    Combined<RightCongruence<S>, ParityCondition<CongruenceTrigger<S>>>;

#[cfg(test)]
mod tests {}
