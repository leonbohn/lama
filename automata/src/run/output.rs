use crate::{ts::StateOf, TransitionOutput};

use super::{EscapePrefix, Induces};

pub trait RunWithOutput<TS: TransitionOutput + ?Sized, K>: Induces<TS, K> {
    type Output: Clone + std::fmt::Debug + Eq;

    fn run_with_output(
        &self,
        on: &TS,
        from: StateOf<TS>,
    ) -> Result<Self::Output, EscapePrefix<StateOf<TS>, Self>>
    where
        Self: Sized;
}
