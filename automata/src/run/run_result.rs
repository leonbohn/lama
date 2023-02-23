use std::collections::HashSet;

use crate::{
    ts::{SymbolFor, TransitionSystem},
    words::{FiniteWord, Word},
    Boundedness, FiniteKind, InfiniteKind,
};

use super::PartialRun;

pub trait RunResult<RunKind: Boundedness> {
    type Success;
    type Failure;
    fn run_result(&self) -> Result<Self::Success, Self::Failure>;
}

impl<'ts, 'w, TS: TransitionSystem, W: Word<S = SymbolFor<TS>, Kind = FiniteKind>>
    RunResult<FiniteKind> for PartialRun<'ts, 'w, W, TS>
{
    type Success = TS::Q;

    type Failure = (FiniteWord<SymbolFor<TS>>, TS::Transition);

    fn run_result(&self) -> Result<Self::Success, Self::Failure> {
        todo!()
    }
}

impl<'ts, 'w, TS: TransitionSystem, W: Word<S = SymbolFor<TS>, Kind = InfiniteKind>>
    RunResult<InfiniteKind> for PartialRun<'ts, 'w, W, TS>
{
    type Success = HashSet<TS::Q>;

    type Failure = (FiniteWord<SymbolFor<TS>>, TS::Transition);

    fn run_result(&self) -> Result<Self::Success, Self::Failure> {
        todo!()
    }
}
