use std::fmt::Display;

use hoars::HoaSymbol;

use crate::{
    acceptance::{AcceptanceCondition, BuchiCondition, ParityCondition, ReachabilityCondition},
    congruence::CongruenceTrigger,
    ts::{Deterministic, Growable, Pointed, Shrinkable, TransitionSystem},
    AnonymousGrowable, RightCongruence,
};

/// Struct that represents the 'usual' automata, which is a combination of a transition system, a designated initial state and an acceptance condition.
pub struct Combined<TS: TransitionSystem, Acc> {
    ts: TS,
    initial: TS::Q,
    acc: Acc,
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

    /// Returns a mutable reference to the underlying acceptance condition.
    pub fn acceptance_mut(&mut self) -> &mut Acc {
        &mut self.acc
    }

    /// Returns a reference to the underlying acceptance condition.
    pub fn acceptance(&self) -> &Acc {
        &self.acc
    }
}

impl<TS: TransitionSystem, Acc> Combined<TS, Acc> {
    /// Constructs a new instance from the given transition system, initial state and acceptance condition.
    pub fn from_parts(ts: TS, initial: TS::Q, acc: Acc) -> Self {
        Self { ts, initial, acc }
    }
}

impl<TS: TransitionSystem, Acc: AcceptanceCondition> TransitionSystem for Combined<TS, Acc> {
    type Q = TS::Q;

    type S = TS::S;

    fn succ(
        &self,
        from: &Self::Q,
        on: &crate::ts::SymbolOf<Self>,
    ) -> Option<crate::ts::StateOf<Self>> {
        self.ts.succ(from, on)
    }

    fn vec_alphabet(&self) -> Vec<Self::S> {
        self.ts.vec_alphabet()
    }

    fn vec_states(&self) -> Vec<Self::Q> {
        self.ts.vec_states()
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

    fn remove_transition(&mut self, from: Self::Q, on: Self::S) -> Option<Self::Q> {
        self.ts.remove_transition(from, on)
    }
}

impl<TS: TransitionSystem, Acc: AcceptanceCondition> Pointed for Combined<TS, Acc> {
    fn initial(&self) -> Self::Q {
        self.initial.clone()
    }
}

impl<TS, Acc> Display for Combined<TS, Acc>
where
    TS: TransitionSystem + Display,
    Acc: AcceptanceCondition + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{}", self.ts, self.acc)
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
        ts::{Growable, Pointed},
        words::PeriodicWord,
        Acceptor,
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
