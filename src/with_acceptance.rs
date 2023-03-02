use crate::{
    acceptance::AcceptanceCondition,
    ts::{deterministic::Deterministic, Pointed, TransitionSystem},
};

/// Allows us to add an acceptance condition to an existing transition system, is used in [`TransitionSystem::with_acceptance`].
pub struct WithAcceptance<'ts, Acc: AcceptanceCondition, TS: TransitionSystem = Deterministic> {
    /// The acceptance condition.
    pub acceptance: Acc,
    /// Pointer to the transition system.
    pub ts: &'ts TS,
}

impl<'ts, Acc: AcceptanceCondition, TS: TransitionSystem> WithAcceptance<'ts, Acc, TS> {
    /// Creates a new `WithAcceptance` object from the given acceptance condition and transition system.
    pub fn new(acceptance: Acc, ts: &'ts TS) -> Self {
        Self { acceptance, ts }
    }
}

impl<'ts, Acc: AcceptanceCondition, TS: TransitionSystem> TransitionSystem
    for WithAcceptance<'ts, Acc, TS>
{
    type Q = TS::Q;

    type S = TS::S;

    type Trigger = TS::Trigger;

    fn succ(
        &self,
        from: &Self::Q,
        on: &crate::ts::SymbolFor<Self>,
    ) -> Option<crate::ts::OutputOf<Self>> {
        self.ts.succ(from, on)
    }
}

impl<'ts, Acc: AcceptanceCondition, TS: TransitionSystem> AcceptanceCondition
    for WithAcceptance<'ts, Acc, TS>
{
    type Induced = Acc::Induced;

    type Kind = Acc::Kind;

    fn is_accepting(&self, induced: &Self::Induced) -> bool {
        self.acceptance.is_accepting(induced)
    }
}

impl<'ts, Acc: AcceptanceCondition, TS: TransitionSystem + Pointed> Pointed
    for WithAcceptance<'ts, Acc, TS>
{
    fn initial(&self) -> Self::Q {
        self.ts.initial()
    }
}
