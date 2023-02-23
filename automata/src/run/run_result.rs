use std::collections::HashSet;

use crate::{
    ts::{SymbolFor, TransitionSystem},
    words::{FiniteWord, Word},
    Boundedness, FiniteKind, InfiniteKind,
};

use super::Walker;

pub trait RunResult<RunKind: Boundedness> {
    type Success;
    type Failure;
    fn run_result(&self) -> Result<Self::Success, Self::Failure>;
}

impl<'ts, 'w, TS: TransitionSystem, W: Word<S = SymbolFor<TS>, Kind = FiniteKind>>
    RunResult<FiniteKind> for Walker<'ts, 'w, W, TS>
{
    type Success = TS::Q;

    type Failure = (FiniteWord<SymbolFor<TS>>, TS::Transition);

    fn run_result(&self) -> Result<Self::Success, Self::Failure> {
        todo!()
    }
}

impl<'ts, 'w, TS: TransitionSystem, W: Word<S = SymbolFor<TS>, Kind = InfiniteKind>>
    RunResult<InfiniteKind> for Walker<'ts, 'w, W, TS>
{
    type Success = HashSet<TS::Q>;

    type Failure = (FiniteWord<SymbolFor<TS>>, TS::Transition);

    fn run_result(&self) -> Result<Self::Success, Self::Failure> {
        todo!()
    }
}
