use crate::{
    acceptance::AcceptanceCondition,
    acceptor::Acceptor,
    run::{RunResult, WalkIn},
    ts::{deterministic::Deterministic, TransitionSystem},
    words::Word,
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

impl<'ts, 'w, W, Acc, TS> Acceptor<W> for WithAcceptance<'ts, Acc, TS>
where
    W: Word<S = TS::S> + WalkIn<'ts, 'w, TS>,
    Acc: AcceptanceCondition,
    <W as WalkIn<'ts, 'w, TS>>::Walker: RunResult<W, Success = Acc::Induced>,
    // <W as WalkIn<TS>>::Walker?:,
    TS: TransitionSystem,
{
    type TS = TS;

    fn accepts(&self, _word: &W) -> bool {
        // word.run(&self.ts)
        todo!()
    }
}
