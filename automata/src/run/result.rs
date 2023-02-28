use std::collections::HashSet;

use crate::{
    coloring::Finite,
    ts::{OutputOf, SymbolFor, TransitionSystem},
    words::{IsFinite, IsInfinite, Subword, Word},
    Boundedness, FiniteKind, InfiniteKind,
};

use super::{walker::EscapePrefix, RunOutput, Walk};

/// Abstracts the evaluation of a run.
pub trait Run<TS: TransitionSystem, K>: Word {
    /// Type that is returned for successful runs. This is usually a state in the case of a finite input and a set of states (usually a [`HashSet`]) in the case of an infinite input.
    type Induces;
    /// Type that is returned for failed runs. This is usually an [`EscapePrefix`] consisting of the successfully read prefix, and a the first state-symbol pair for which a missing transition was encountered.
    type Failure;

    /// Evaluates the run and returns the result.
    fn run(&self, on: &TS, from: OutputOf<TS>) -> Result<Self::Induces, Self::Failure>;
}

impl<W: IsFinite, TS: TransitionSystem<S = W::S>> Run<TS, FiniteKind> for W {
    type Induces = TS::Q;

    type Failure = EscapePrefix<TS>;

    fn run(&self, on: &TS, from: TS::Q) -> Result<Self::Induces, Self::Failure> {
        let mut walker = on.walk(from, self);
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

impl<TS: TransitionSystem, W: IsInfinite + Subword> Run<TS, InfiniteKind> for W
where
    <W as Subword>::PrefixType: IsFinite,
{
    type Induces = HashSet<TS::Trigger>;

    type Failure = EscapePrefix<TS>;

    fn run(&self, on: &TS, from: TS::Q) -> Result<Self::Induces, Self::Failure> {
        todo!()
        // let prefix_length = self.base_length();
        // let recur_length = self.recur_length();
        // match on.run(&from, &self.prefix(prefix_length)) {
        //     Err(e) => Err(e),
        //     Ok(reached) => {
        //         let recur = on.skip(prefix_length);
        //         let mut seen = HashSet::new();
        //         let mut walker = self.walk(reached, &recur);
        //         loop {
        //             // We now collect the individual run pieces and check if we have seen them before.
        //             match walker.try_take_n(recur_length) {
        //                 Ok(recur_reached) => {
        //                     if !seen.insert(recur_reached) {
        //                         // We have seen this piece before, so we can stop here.
        //                         return Ok(walker.seq.into_iter().collect());
        //                     }
        //                 }
        //                 Err(RunOutput::WordEnd(_)) => unreachable!("We are in an infinite run!"),
        //                 Err(RunOutput::Trigger(_)) => {
        //                     unreachable!("We failed to take a full piece!")
        //                 }
        //                 Err(RunOutput::Missing(q, a)) => {
        //                     return Err(EscapePrefix::new(walker.seq, q, a))
        //                 }
        //                 Err(RunOutput::FailedBefore) => unreachable!("We would have noticed!"),
        //             }
        //         }
        //     }
        // }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ts::{deterministic::Deterministic, Growable},
        words::{FiniteWord, PeriodicWord},
    };

    use super::*;

    #[test]
    fn omega_acceptance() {
        let mut ts = Deterministic::new();
        let q0 = ts.add_state();
        let q1 = ts.add_state();
        let q2 = ts.add_state();
        ts.add_transition(q0, 'a', q1);
        ts.add_transition(q0, 'b', q0);
        ts.add_transition(q1, 'a', q2);
        ts.add_transition(q1, 'b', q0);
        ts.add_transition(q2, 'b', q0);

        let word = PeriodicWord::from(FiniteWord::from("b"));
        let run = word.run(&ts, q0);
        assert!(run.is_ok());
        assert_eq!(
            run.unwrap(),
            vec![Deterministic::make_trigger(&q0, &'b')]
                .into_iter()
                .collect()
        );

        let ab_run = PeriodicWord::from(FiniteWord::from("ab")).run(&ts, q0);
        assert!(ab_run.is_ok());
        assert_eq!(
            ab_run.unwrap(),
            vec![
                Deterministic::make_trigger(&q0, &'a'),
                Deterministic::make_trigger(&q1, &'b')
            ]
            .into_iter()
            .collect()
        )
    }
}
