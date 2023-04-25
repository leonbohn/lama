use hoars::HoaSymbol;

use crate::{
    acceptance::{
        AcceptanceCondition, BuchiCondition, ParityCondition, ReachabilityCondition, ToOmega,
    },
    congruence::CongruenceTrigger,
    ts::{
        Deterministic, Growable, HasInput, HasStates, Pointed, Shrinkable, SymbolOf,
        TransitionSystem,
    },
    AnonymousGrowable, HasAlphabet, OmegaAutomaton, RightCongruence, StateIndex, Symbol,
};

/// Struct that represents the 'usual' automata, which is a combination of a transition system, a designated initial state and an acceptance condition.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Combined<TS: TransitionSystem, Acc> {
    ts: TS,
    initial: TS::Q,
    acc: Acc,
}

impl<TS: TransitionSystem + HasAlphabet<Alphabet = TS::Sigma>, Acc> HasAlphabet
    for Combined<TS, Acc>
{
    type Alphabet = TS::Sigma;

    type AlphabetIter = TS::AlphabetIter;

    fn alphabet_iter(&self) -> Self::AlphabetIter {
        self.ts.alphabet_iter()
    }
}

impl<TS: TransitionSystem, Acc> HasInput for Combined<TS, Acc> {
    type Sigma = SymbolOf<TS>;

    type Input<'me> = TS::Input<'me>
    where
        Self: 'me;

    fn input_alphabet_iter(&self) -> Self::Input<'_> {
        self.ts.input_alphabet_iter()
    }
}

impl<TS, Acc> Pointed for Combined<TS, Acc>
where
    TS: TransitionSystem,
{
    fn initial(&self) -> Self::Q {
        self.initial.clone()
    }
}

impl<TS: TransitionSystem, Acc> Combined<TS, Acc> {
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
}

impl<Q: StateIndex, S: Symbol, Acc> Combined<Deterministic<Q, S>, Acc> {
    /// Converts the automaton to an [`OmegaAutomaton`], which boils down to simply
    /// replacing the acceptance condition with an [`OmegaCondition`].
    pub fn to_omega(&self) -> OmegaAutomaton<Q, S>
    where
        Acc: ToOmega<X = (Q, S)> + AcceptanceCondition,
    {
        self.with_acceptance(self.acceptance().to_omega())
    }
}

#[allow(unused)]
impl<TS: TransitionSystem + Default + AnonymousGrowable, Acc: AcceptanceCondition + Default>
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

impl<TS: TransitionSystem, Acc> Combined<TS, Acc> {
    /// Constructs a new instance from the given transition system, initial state and acceptance condition.
    pub fn from_parts(ts: TS, initial: TS::Q, acc: Acc) -> Self {
        Self { ts, initial, acc }
    }

    /// Returns a reference to the underlying transition system.
    pub fn ts(&self) -> &TS {
        &self.ts
    }
}

impl<TS, Acc> HasStates for Combined<TS, Acc>
where
    TS: TransitionSystem,
{
    type Q = TS::Q;

    type States<'me> = TS::States<'me> where Self:'me;

    fn states_iter(&self) -> Self::States<'_> {
        self.ts.states_iter()
    }
}

impl<TS: TransitionSystem, Acc> TransitionSystem for Combined<TS, Acc> {
    fn successor(
        &self,
        from: &Self::Q,
        on: &crate::ts::SymbolOf<Self>,
    ) -> Option<crate::ts::StateOf<Self>> {
        self.ts.successor(from, on)
    }
}

impl<TS: TransitionSystem, Acc: AcceptanceCondition> AcceptanceCondition for Combined<TS, Acc> {
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
        on: crate::ts::SymbolOf<Self>,
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
pub type Dfa<Q = u32, S = char> = Combined<Deterministic<Q, S>, ReachabilityCondition<Q>>;
#[cfg(feature = "det")]
/// Type alias for a deterministic Büchi automaton, only available for crate feature `det`.
pub type Dba<Q = u32, S = char> = Combined<Deterministic<Q, S>, BuchiCondition<(Q, S)>>;
#[cfg(feature = "det")]
/// Type alias for a deterministic parity automaton, only available for crate feature `det`.
pub type Dpa<Q = u32, S = char> = Combined<Deterministic<Q, S>, ParityCondition<(Q, S)>>;

/// Alias that makes working with HOA easier. This is the same as a [`Dpa`], but with [`HoaSymbol`] as the symbol type.
pub type HoaDpa<Q = u32, S = HoaSymbol> = Combined<Deterministic<Q, S>, ParityCondition<(Q, S)>>;
/// Alias that makes working with HOA easier. This is the same as a [`Dba`], but with [`HoaSymbol`] as the symbol type.
pub type HoaDba<Q = u32, S = HoaSymbol> = Combined<Deterministic<Q, S>, BuchiCondition<(Q, S)>>;

/// Alias that makes working with congruences easier, using [`RightCongruence`] as the transition system.
pub type CongruenceDba<S = char> =
    Combined<RightCongruence<S>, BuchiCondition<CongruenceTrigger<S>>>;
/// Alias that makes working with congruences easier, using [`RightCongruence`] as the transition system.
pub type CongruenceDpa<S = char> =
    Combined<RightCongruence<S>, ParityCondition<CongruenceTrigger<S>>>;

#[cfg(test)]
mod tests {
    use crate::{
        acceptance::Acceptor,
        ts::{Growable, Pointed},
        words::PeriodicWord,
    };

    #[test]
    fn dfa_acceptor() {
        let mut dfa = super::Dfa::trivial();
        let q0 = dfa.initial();
        let q1 = 1;
        assert!(dfa.add_state(&q1));
        dfa.add_transition(&q0, 'a', &q1);
        dfa.add_transition(&q1, 'a', &q0);
        dfa.add_transition(&q0, 'b', &q0);
        dfa.add_transition(&q1, 'b', &q1);
        *dfa.acceptance_mut() += q1;
        assert!(dfa.accepts("a"));
        *dfa.acceptance_mut() -= q1;
        assert!(!dfa.accepts("a"));
        assert!(dfa.rejects("b"))
    }

    #[test]
    fn dba_accetor() {
        let mut dba = super::Dba::trivial();
        let q0 = dba.initial();
        let q1 = 1;
        assert!(dba.add_state(&q1));
        dba.add_transition(&q0, 'a', &q1);
        dba.add_transition(&q1, 'a', &q0);
        dba.add_transition(&q0, 'b', &q0);
        dba.add_transition(&q1, 'b', &q1);
        *dba.acceptance_mut() += (q1, 'a');
        assert!(dba.accepts(PeriodicWord::from("a")));
    }
}
