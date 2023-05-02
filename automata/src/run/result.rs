use std::{collections::HashSet, fmt::Debug};

use crate::{
    ts::{StateOf, Successor},
    words::{IsFinite, IsInfinite, Subword},
    Boundedness, FiniteKind, InfiniteKind, Pointed, Set,
};

use super::{EscapePrefix, RunOutput, Walk, Walker};

/// Abstracts the evaluation of a run.
pub trait Run<TS: Successor + ?Sized, K>: Subword {
    /// Type that is returned for successful runs. This is usually a state in the case of a finite input and a set of states (usually a [`HashSet`]) in the case of an infinite input.
    type Induces: Clone + Debug + Eq;

    /// Evaluates the run and returns the result.
    fn run(
        &self,
        on: &TS,
        from: StateOf<TS>,
    ) -> Result<Self::Induces, EscapePrefix<StateOf<TS>, Self>>
    where
        Self: Sized;
}

/// Abstracts the evaluation of an initial run, i.e. a [`Run`] that starts at the initial state of the transition system.
pub trait InitialRun<TS: Successor + ?Sized, K>: Run<TS, K> {
    /// Evaluates the run and returns the result.
    fn initial_run(&self, on: &TS) -> Result<Self::Induces, EscapePrefix<StateOf<TS>, Self>>
    where
        Self: Sized;
}

impl<TS: Successor + Pointed + ?Sized, K: Boundedness, R: Run<TS, K>> InitialRun<TS, K> for R {
    fn initial_run(&self, on: &TS) -> Result<Self::Induces, EscapePrefix<StateOf<TS>, Self>>
    where
        Self: Sized,
    {
        self.run(on, on.initial())
    }
}

impl<W: IsFinite + Subword, TS: Successor<Sigma = W::S>> Run<TS, FiniteKind> for W {
    type Induces = StateOf<TS>;

    fn run(
        &self,
        on: &TS,
        from: StateOf<TS>,
    ) -> Result<Self::Induces, EscapePrefix<StateOf<TS>, Self>> {
        let mut walker = on.walk(from, self);
        let prefix = walker
            .by_ref()
            .take_while(RunOutput::is_trigger)
            .map(|o| o.get_trigger().expect("Must be a trigger!"))
            .collect();
        match walker.next() {
            Some(RunOutput::WordEnd(q)) => Ok(q),
            Some(RunOutput::Missing(q, a)) => Err(EscapePrefix::new(self, prefix, q, a)),
            _ => unreachable!(),
        }
    }
}

impl<W: IsInfinite + Subword, TS: Successor<Sigma = W::S>> Run<TS, InfiniteKind> for W
where
    <W as Subword>::PrefixType: IsFinite + Run<TS, FiniteKind, Induces = StateOf<TS>>,
{
    type Induces = Set<(StateOf<TS>, TS::Sigma)>;

    fn run(
        &self,
        on: &TS,
        from: StateOf<TS>,
    ) -> Result<Self::Induces, EscapePrefix<StateOf<TS>, Self>> {
        let prefix_length = self.base_length();
        let recur_length = self.recur_length();
        let prefix = self.prefix(prefix_length);
        match prefix.run(on, from) {
            Err(e) => Err(EscapePrefix::from_finite(self, e)),
            Ok(reached) => {
                let recur = self.skip(prefix_length);
                let mut seen = HashSet::new();
                let mut walker = Walker::new(on, &recur, reached);
                loop {
                    // We now collect the individual run pieces and check if we have seen them before.
                    match walker.try_take_n(recur_length) {
                        Ok(recur_reached) => {
                            if !seen.insert(recur_reached) {
                                // We have seen this piece before, so we can stop here.
                                return Ok(walker.seq.into_iter().collect());
                            }
                        }
                        Err(RunOutput::WordEnd(_)) => unreachable!("We are in an infinite run!"),
                        Err(RunOutput::Trigger(_, _)) => {
                            unreachable!("We failed to take a full piece!")
                        }
                        Err(RunOutput::Missing(q, a)) => {
                            return Err(EscapePrefix::new(self, walker.seq, q, a))
                        }
                        Err(RunOutput::FailedBefore) => unreachable!("We would have noticed!"),
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ts::{transitionsystem::TransitionSystem, Growable},
        words::{PeriodicWord, Str},
        AnonymousGrowable,
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
        let run = word.run(&ts, q0);
        assert!(run.is_ok());
        assert_eq!(run.unwrap(), vec![(q0, 'b')].into_iter().collect());

        let ab_run = PeriodicWord::from(Str::from("ab")).run(&ts, q0);
        assert!(ab_run.is_ok());
        assert_eq!(
            ab_run.unwrap(),
            vec![(q0, 'a'), (q1, 'b')].into_iter().collect()
        )
    }
}
