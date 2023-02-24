mod escaping;
mod run_result;
mod take_transition;
mod walker;

pub use run_result::RunResult;
pub use take_transition::TakeTransition;
pub use walker::Walker;

use crate::words::{FiniteWord, Word};

use crate::ts::{OutputOf, Pointed, SymbolFor, TransitionSystem};

pub enum RunOutput<TS: TransitionSystem> {
    Transition(TS::Trigger),
    WordEnd,
    Missing(TS::Q, SymbolFor<TS>),
    FailedBefore,
}

pub trait Run<W: Word>: TransitionSystem + Sized {
    type Output;
    fn run<'ts, 'w, X: AsRef<W>>(&'ts self, on: &'w X) -> Walker<'ts, 'w, W, Self>;
}

impl<T: TransitionSystem + Pointed<OutputOf<T>>> Run<FiniteWord<T::S>> for T {
    type Output = T::Q;

    fn run<'ts, 'w, X: AsRef<FiniteWord<T::S>>>(
        &'ts self,
        on: &'w X,
    ) -> Walker<'ts, 'w, FiniteWord<T::S>, T> {
        Walker {
            word: on.as_ref(),
            ts: self,
            state: Some(self.initial()),
            position: 0,
            seq: vec![self.initial()],
        }
    }
}
