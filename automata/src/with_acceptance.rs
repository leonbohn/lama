use crate::{
    acceptance::AcceptanceCondition,
    acceptor::Acceptor,
    run::Runnable,
    ts::{deterministic::Deterministic, TransitionSystem},
    words::Word,
};

pub struct WithAcceptance<Acc: AcceptanceCondition, TS: TransitionSystem = Deterministic> {
    pub acceptance: Acc,
    pub ts: TS,
}

impl<W, Acc, TS> Acceptor<W> for WithAcceptance<Acc, TS>
where
    W: Word<S = TS::S> + Runnable<TS>,
    Acc: AcceptanceCondition<Induced = W::Induces>,
    TS: TransitionSystem,
{
    type TS = TS;

    fn accepts(&self, _word: &W) -> bool {
        // word.run(&self.ts)
        todo!()
    }
}
