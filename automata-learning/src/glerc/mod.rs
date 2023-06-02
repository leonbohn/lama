#[allow(dead_code)]
/// This module contains the implementations of the individual constraint algorithms.
mod constraint;
pub use constraint::*;

#[allow(dead_code)]
mod state;
pub use state::GlercState;

use std::fmt::Display;

use automata::RightCongruence;
use automata::Symbol;

use automata::Class;

use self::constraint::Constraint;

/// Represents an intermediate output of the GLERC algorithm.
#[derive(Eq, Debug, Clone, PartialEq)]
pub enum GlercSignal<S: Symbol, X> {
    /// Indicates that the transition (q,a) is missing
    MissingTransition(Class<S>, S),
    /// Failed to insert a transition
    FailedInsertion(Class<S>, S, Class<S>),
    /// Successfully inserted a transition
    SuccessfulInsertion(Class<S>, S, Class<S>),
    /// Indicates that a new state was created
    NewState(Class<S>, S, Class<S>),
    /// Indicates that the algorithm has finished and produces the result.
    Finished(GlercOutput<S, X>),
}

/// Encapsulates the output of the GLERC algorithm. This includes the learned
/// congruence relation, but also includes statistics about the execution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlercOutput<S: Symbol, X> {
    learned_congruence: RightCongruence<S>,
    execution_time: std::time::Duration,
    constraint_produced: X,
}

impl<S: Symbol, X> GlercOutput<S, X> {
    pub(crate) fn new(
        learned_congruence: &RightCongruence<S>,
        execution_time: std::time::Duration,
        constraint_produced: X,
    ) -> Self {
        Self {
            learned_congruence: learned_congruence.clone(),
            execution_time,
            constraint_produced,
        }
    }

    /// Returns the time that was taken by the execution of the algorithm.
    pub fn execution_time_ms(&self) -> u128 {
        self.execution_time.as_millis()
    }

    /// Returns both the learned congruence as well as the object produced by the constraint.
    pub fn learned(self) -> (RightCongruence<S>, X) {
        (self.learned_congruence, self.constraint_produced)
    }
}

impl<S: Symbol, X: Display> Display for GlercOutput<S, X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Learned congruence:")?;
        writeln!(f, "{}", self.learned_congruence)?;
        writeln!(f, "Execution time: {:?}", self.execution_time)?;
        writeln!(f, "Produced {}", self.constraint_produced)
    }
}

/// Runs the GLeRC algorithm for inferring a right congruence relation with the given
/// `constraint`, over the given `alphabet`, while respecting the given `fallback`. Specifically,
/// this means that if the size of the `fallback` would be exceeded by the constructed TS, then the
/// algorithm terminates prematurely, returning `fallback` instead.
pub fn glerc<S: Symbol, C: Constraint<S>, I: IntoIterator<Item = S>>(
    constraint: C,
    alphabet: I,
    fallback: RightCongruence<S>,
) -> (RightCongruence<S>, C::Output) {
    let mut glerc = GlercState::new(fallback, alphabet, constraint);
    loop {
        if let GlercSignal::Finished(output) = glerc.step() {
            return output.learned();
        }
    }
}

#[cfg(test)]
mod tests {}
