use std::collections::HashSet;

use crate::{
    ts::TransitionSystem,
    words::{IsFinite, IsInfinite, Word},
    FiniteKind, InfiniteKind,
};

use super::{walker::EscapePrefix, RunOutput, Walk};

/// Abstracts the evaluation of a run.
pub trait Run<W, Kind>: TransitionSystem {
    /// Type that is returned for successful runs. This is usually a state in the case of a finite input and a set of states (usually a [`HashSet`]) in the case of an infinite input.
    type Induces;
    /// Type that is returned for failed runs. This is usually an [`EscapePrefix`] consisting of the successfully read prefix, and a the first state-symbol pair for which a missing transition was encountered.
    type Failure;

    /// Evaluates the run and returns the result.
    fn run(&self, from: Self::Q, on: &W) -> Result<Self::Induces, Self::Failure>;
}

impl<W: Word + IsFinite, TS: TransitionSystem<S = W::S>> Run<W, FiniteKind> for TS {
    type Induces = TS::Q;

    type Failure = EscapePrefix<TS>;

    fn run(&self, from: Self::Q, on: &W) -> Result<Self::Induces, Self::Failure> {
        let mut walker = self.walk(from, on);
        let prefix = walker
            .by_ref()
            .take_while(RunOutput::is_trigger)
            .map(|o| o.get_trigger().expect("Must be a trigger!").clone())
            .collect();
        match walker.next() {
            Some(RunOutput::WordEnd(q)) => Ok(q),
            Some(RunOutput::Missing(q, a)) => Err(EscapePrefix::new(prefix, q, a)),
            _ => unreachable!(),
        }
    }
}

impl<W: Word + IsInfinite, TS: TransitionSystem<S = W::S>> Run<W, InfiniteKind> for TS {
    type Induces = HashSet<TS::Q>;

    type Failure = EscapePrefix<TS>;

    fn run(&self, _from: Self::Q, _on: &W) -> Result<Self::Induces, Self::Failure> {
        todo!()
    }
}
