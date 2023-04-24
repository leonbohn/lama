use crate::{ts::StateOf, WithOutput};

use super::{EscapePrefix, Run};

pub trait RunWithOutput<TS: WithOutput + ?Sized, K>: Run<TS, K> {
    type Output: Clone + std::fmt::Debug + Eq;

    fn run_with_output(
        &self,
        on: &TS,
        from: StateOf<TS>,
    ) -> Result<Self::Output, EscapePrefix<TS::State, Self>>
    where
        Self: Sized;
}
