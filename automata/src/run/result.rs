use std::collections::HashSet;

use crate::{
    ts::{TransitionSystem, TransitionTrigger},
    words::{FiniteWord, PeriodicWord, UltimatelyPeriodicWord, Word},
};

use super::{walker::EscapePrefix, RunOutput, Walker};

/// Abstracts the evaluation of a run.
pub trait RunResult<W: Word> {
    /// Type that is returned for successful runs. This is usually a state in the case of a finite input and a set of states (usually a [`HashSet`]) in the case of an infinite input.
    type Success;
    /// Type that is returned for failed runs. This is usually an [`EscapePrefix`] consisting of the successfully read prefix, and a the first state-symbol pair for which a missing transition was encountered.
    type Failure;

    /// Evaluates the run and returns the result.
    fn result(&mut self) -> Result<Self::Success, Self::Failure>;
}

impl<'t, 'w, TS: TransitionSystem + 't> RunResult<FiniteWord<TS::S>>
    for Walker<'t, 'w, FiniteWord<TS::S>, TS>
{
    type Success = TS::Q;
    type Failure = EscapePrefix<TS>;

    fn result(&mut self) -> Result<Self::Success, Self::Failure> {
        let prefix = FiniteWord::from_iter(
            self.take_while(|output| output.is_trigger())
                .map(|output| output.get_trigger().unwrap().on().clone()),
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
