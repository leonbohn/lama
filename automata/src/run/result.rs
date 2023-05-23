use std::{borrow::Borrow, collections::HashSet, fmt::Debug};

use crate::{
    ts::{StateOf, Successor},
    words::{IsFinite, IsInfinite, Subword},
    Boundedness, FiniteKind, InfiniteKind, Pointed, Set,
};

use super::{EscapePrefix, RunOutput, Walk, Walker};

/// Abstracts the evaluation of a run.
pub trait Induces<TS: Successor + ?Sized, K>: Subword {
    /// Type that is returned for successful runs. This is usually a state in the case of a finite input and a set of states (usually a [`HashSet`]) in the case of an infinite input.
    type Induces: Clone + Debug + Eq;

    /// Evaluates the run and returns the result.
    fn evaluate<X: Borrow<StateOf<TS>>>(
        &self,
        on: &TS,
        from: X,
    ) -> Result<Self::Induces, EscapePrefix<StateOf<TS>, Self>>
    where
        Self: Sized;
}

/// Abstracts the evaluation of an initial run, i.e. a [`Run`] that starts at the initial state of the transition system.
pub trait InitialRun<TS: Successor + ?Sized, K>: Induces<TS, K> {
    /// Evaluates the run and returns the result.
    fn initial_run(&self, on: &TS) -> Result<Self::Induces, EscapePrefix<StateOf<TS>, Self>>
    where
        Self: Sized;
}

impl<TS: Successor + Pointed + ?Sized, K: Boundedness, R: Induces<TS, K>> InitialRun<TS, K> for R {
    fn initial_run(&self, on: &TS) -> Result<Self::Induces, EscapePrefix<StateOf<TS>, Self>>
    where
        Self: Sized,
    {
        self.evaluate(on, on.initial())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        run::Evaluate,
        ts::{transitionsystem::TransitionSystem, Growable},
        words::{PeriodicWord, Str},
        AnonymousGrowable, UltimatelyPeriodicWord,
    };

    use super::*;

    #[test]
    fn omega_acceptance() {
        let mut ts: TransitionSystem<u32> = TransitionSystem::new();
        let q0 = ts.add_new_state();
        let q1 = ts.add_new_state();
        let q2 = ts.add_new_state();
        ts.add_transition(&q0, 'a', &q1);
        ts.add_transition(&q0, 'b', &q0);
        ts.add_transition(&q1, 'a', &q2);
        ts.add_transition(&q1, 'b', &q0);
        ts.add_transition(&q2, 'b', &q0);

        let word = PeriodicWord::from(Str::from("b"));

        let run = ts.run_from(q0, &word);

        let result = run.evaluate();
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), vec![(q0, 'b', q0)].into_iter().collect());

        let ab_run = ts
            .run_from(q0, &UltimatelyPeriodicWord::from("ab"))
            .evaluate();
        assert!(ab_run.is_ok());
        assert_eq!(
            ab_run.unwrap(),
            vec![(q0, 'a', q1), (q1, 'b', q0)].into_iter().collect()
        )
    }
}
