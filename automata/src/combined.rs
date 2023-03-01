use crate::{
    acceptance::{AcceptanceCondition, BuchiCondition, ParityCondition, ReachabilityCondition},
    ts::{Deterministic, Growable, Pointed, Shrinkable, TransitionSystem},
    Pair,
};

/// Struct that represents the 'usual' automata, which is a combination of a transition system, a designated initial state and an acceptance condition.
pub struct Combined<Acc, TS: TransitionSystem> {
    ts: TS,
    initial: TS::Q,
    acc: Acc,
}

#[allow(unused)]
impl<TS: TransitionSystem + Default + Growable, Acc: AcceptanceCondition + Default>
    Combined<Acc, TS>
{
    /// Creates a new instance with a single state that serves as the initial state.
    /// Note that the resulting automaton is not empty per se, it has a single initial state, which can be accessed via [`Combined::initial`].
    pub fn trivial() -> Self {
        let mut ts = TS::default();
        let initial = ts.add_state();
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

impl<TS: TransitionSystem, Acc: AcceptanceCondition> TransitionSystem for Combined<Acc, TS> {
    type Q = TS::Q;

    type S = TS::S;

    type Trigger = TS::Trigger;

    type Transition = TS::Transition;

    fn succ(
        &self,
        from: &Self::Q,
        on: &crate::ts::SymbolOf<Self>,
    ) -> Option<crate::ts::StateOf<Self>> {
        self.ts.succ(from, on)
    }
}

impl<TS: TransitionSystem, Acc: AcceptanceCondition> AcceptanceCondition for Combined<Acc, TS> {
    type Induced = Acc::Induced;

    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        self.acc.is_accepting(induced)
    }

    type Kind = Acc::Kind;
}

impl<TS: Growable, Acc: AcceptanceCondition> Growable for Combined<Acc, TS> {
    fn add_state(&mut self) -> Self::Q {
        self.ts.add_state()
    }

    fn add_transition(
        &mut self,
        from: Self::Q,
        on: crate::ts::SymbolOf<Self>,
        to: Self::Q,
    ) -> Option<Self::Q> {
        self.ts.add_transition(from, on, to)
    }
}

impl<TS: Shrinkable, Acc: AcceptanceCondition> Shrinkable for Combined<Acc, TS> {
    fn remove_state(&mut self, state: Self::Q) -> Option<Self::Q> {
        self.ts.remove_state(state)
    }

    fn remove_transition(
        &mut self,
        from: Self::Q,
        on: crate::ts::SymbolOf<Self>,
    ) -> Option<Self::Q> {
        self.ts.remove_transition(from, on)
    }
}

impl<TS: TransitionSystem, Acc: AcceptanceCondition> Pointed for Combined<Acc, TS> {
    fn initial(&self) -> Self::Q {
        self.initial.clone()
    }
}

#[cfg(feature = "det")]
/// Type alias for a deterministic finite automaton, only available for crate feature `det`.
pub type Dfa<S = char, Q = u32> = Combined<ReachabilityCondition<Q>, Deterministic<S, Q>>;
#[cfg(feature = "det")]
/// Type alias for a deterministic Büchi automaton, only available for crate feature `det`.
pub type Dba<S = char, Q = u32> = Combined<BuchiCondition<Pair<S, Q>>, Deterministic<S, Q>>;
#[cfg(feature = "det")]
/// Type alias for a deterministic parity automaton, only available for crate feature `det`.
pub type Dpa<S = char, Q = u32> = Combined<ParityCondition<Pair<S, Q>>, Deterministic<S, Q>>;

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
        let q1 = dfa.add_state();
        dfa.add_transition(q0, 'a', q1);
        dfa.add_transition(q1, 'a', q0);
        dfa.add_transition(q0, 'b', q0);
        dfa.add_transition(q1, 'b', q1);
        *dfa.acceptance_mut() += q1;
        assert!(dfa.accepts(&"a"));
        *dfa.acceptance_mut() -= q1;
        assert!(!dfa.accepts(&"a"));
        assert!(dfa.rejects(&"b"))
    }

    #[test]
    fn dba_accetor() {
        let mut dba = super::Dba::trivial();
        let q0 = dba.initial();
        let q1 = dba.add_state();
        dba.add_transition(q0, 'a', q1);
        dba.add_transition(q1, 'a', q0);
        dba.add_transition(q0, 'b', q0);
        dba.add_transition(q1, 'b', q1);
        *dba.acceptance_mut() += (q1, 'a');
        assert!(dba.accepts(&PeriodicWord::from("a")));
    }
}
