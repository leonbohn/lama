use std::collections::HashSet;

use crate::{
    run::TakeTransition,
    ts::{SymbolFor, TransitionSystem, TransitionTrigger},
    words::{FiniteWord, PeriodicWord, UltimatelyPeriodicWord, Word},
};

use super::RunOutput;

pub trait RunResult<W: Word> {
    type Success;
    type Failure;
    fn result(&mut self) -> Result<Self::Success, Self::Failure>;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EscapePrefix<TS: TransitionSystem>(
    pub FiniteWord<SymbolFor<TS>>,
    pub TS::Q,
    pub SymbolFor<TS>,
);

#[derive(Clone, Debug)]
pub struct Walker<'ts, 'w, W: Word, TS: TransitionSystem<S = W::S>> {
    pub(crate) word: &'w W,
    pub(crate) ts: &'ts TS,
    pub(crate) state: Option<TS::Q>,
    pub(crate) position: usize,
    pub(crate) seq: Vec<TS::Q>,
}

impl<'t, 'w, W: Word, TS: TransitionSystem<S = W::S>> Iterator for Walker<'t, 'w, W, TS> {
    type Item = RunOutput<TS>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.take_transition() {
            RunOutput::FailedBefore => None,
            otherwise => Some(otherwise),
        }
    }
}

impl<'t, 'w, TS: TransitionSystem + 't> RunResult<FiniteWord<TS::S>>
    for Walker<'t, 'w, FiniteWord<TS::S>, TS>
{
    type Success = TS::Q;
    type Failure = EscapePrefix<TS>;

    fn result(&mut self) -> Result<Self::Success, Self::Failure> {
        let prefix = FiniteWord::from_iter(
            self.take_while(|output| output.is_trigger())
                .map(|output| output.trigger().unwrap().on().clone()),
        );
        match self.next() {
            Some(RunOutput::WordEnd(q)) => Ok(q),
            Some(RunOutput::Missing(q, s)) => Err(EscapePrefix(prefix, q, s)),
            _ => unreachable!(),
        }
    }
}

impl<'t, 'w, TS: TransitionSystem + 't> RunResult<PeriodicWord<TS::S>>
    for Walker<'t, 'w, PeriodicWord<TS::S>, TS>
{
    type Success = HashSet<TS::Q>;
    type Failure = EscapePrefix<TS>;

    fn result(&mut self) -> Result<Self::Success, Self::Failure> {
        todo!()
    }
}

impl<'t, 'w, TS: TransitionSystem + 't> RunResult<UltimatelyPeriodicWord<TS::S>>
    for Walker<'t, 'w, UltimatelyPeriodicWord<TS::S>, TS>
{
    type Success = HashSet<TS::Q>;
    type Failure = EscapePrefix<TS>;

    fn result(&mut self) -> Result<Self::Success, Self::Failure> {
        todo!()
    }
}
