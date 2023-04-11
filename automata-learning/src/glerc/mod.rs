#[allow(dead_code)]
mod constraint;
#[allow(dead_code)]
mod state;

use std::fmt::Display;

use automata::RightCongruence;
use automata::Symbol;

use automata::Class;

/// Represents an intermediate output of the GLERC algorithm.
#[derive(Eq, Debug, Clone, PartialEq)]
pub enum GlercSignal<S: Symbol> {
    /// Indicates that the transition (q,a) is missing
    MissingTransition(Class<S>, S),
    /// Failed to insert a transition
    FailedInsertion(Class<S>, S, Class<S>),
    /// Successfully inserted a transition
    SuccessfulInsertion(Class<S>, S, Class<S>),
    /// Indicates that a new state was created
    NewState(Class<S>, S, Class<S>),
    /// Indicates that the algorithm has finished and produces the result.
    Finished(GlercOutput<S>),
}

/// Encapsulates the output of the GLERC algorithm. This includes the learned
/// congruence relation, but also includes statistics about the execution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlercOutput<S: Symbol> {
    learned_congruence: RightCongruence<S>,
    execution_time: std::time::Duration,
}

impl<S: Symbol> GlercOutput<S> {
    pub(crate) fn new(
        learned_congruence: &RightCongruence<S>,
        execution_time: std::time::Duration,
    ) -> Self {
        Self {
            learned_congruence: learned_congruence.clone(),
            execution_time,
        }
    }
}

impl<S: Symbol> Display for GlercOutput<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Learned congruence:")?;
        writeln!(f, "{}", self.learned_congruence)?;
        writeln!(f, "Execution time: {:?}", self.execution_time)
    }
}

#[cfg(test)]
mod tests {}
