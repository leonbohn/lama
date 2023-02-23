use crate::run::{RunOutput, Walker};
use crate::ts::{SymbolFor, TransitionSystem};
use crate::words::Word;

/// Implementors of this trait can take individual transitions in a system of type `TS`.
pub trait TakeTransition {
    type TS: TransitionSystem;

    fn take_transition(&mut self) -> RunOutput<Self::TS>;
}

impl<'ts, 'w, TS: TransitionSystem, W: Word<S = SymbolFor<TS>>> TakeTransition
    for Walker<'ts, 'w, W, TS>
where
    TS::Transition: From<(TS::Q, SymbolFor<TS>, TS::Q)>,
{
    type TS = TS;

    fn take_transition(&mut self) -> RunOutput<TS> {
        if let Some(state) = self.state.clone() {
            if let Some(symbol) = self.word.nth(self.position) {
                if let Some(successor) = self.ts.succ(&state, &symbol) {
                    self.position += 1;
                    self.state = Some(successor.clone());
                    self.seq.push(successor.clone());
                    RunOutput::Transition((state, symbol, successor).into())
                } else {
                    RunOutput::Missing(state, symbol)
                }
            } else {
                RunOutput::WordEnd
            }
        } else {
            RunOutput::FailedBefore
        }
    }
}
