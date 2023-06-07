use tracing::trace;
/// Allows the evaluation of a run.
mod result;
use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
};

use crate::{
    ts::{InputOf, Path, StateOf, Successor, TransitionOf},
    words::{HasLength, InducesFromPath, Repr, Representable, Word},
    Pointed, Set, State, Symbol, Trigger, Value,
};

pub trait Runnable: Word {
    type Induces<Q: State, S: Symbol>: Eq;

    fn run_in_from<TS: Successor<Sigma = Self::S>>(
        &self,
        ts: TS,
        origin: TS::Q,
    ) -> Result<Self::Induces<TS::Q, TS::Sigma>, PartialRun<'_, TS::Q, TS::Sigma, Self::Len>>;

    fn run_in<TS: Successor<Sigma = Self::S> + Pointed>(
        &self,
        ts: TS,
    ) -> Result<Self::Induces<TS::Q, TS::Sigma>, PartialRun<'_, TS::Q, TS::Sigma, Self::Len>> {
        let initial = ts.initial();
        self.run_in_from(ts, initial)
    }
}

impl<W> Runnable for W
where
    W: Word,
    for<'word> &'word W: Into<Repr<'word, W::S, W::Len>>,
    W::Len: InducesFromPath,
{
    type Induces<Q: State, S: Symbol> = <W::Len as InducesFromPath>::Induces<Q, S>;

    fn run_in_from<TS: Successor<Sigma = Self::S>>(
        &self,
        ts: TS,
        origin: TS::Q,
    ) -> Result<Self::Induces<TS::Q, TS::Sigma>, PartialRun<'_, TS::Q, TS::Sigma, W::Len>> {
        let mut cane = Cane::new(self, ts.borrow(), origin);
        cane.result()
    }
}

pub struct Cane<'a, TS: Successor, L: InducesFromPath> {
    ts: TS,
    repr: Repr<'a, TS::Sigma, L>,
    position: usize,
    seen: Set<(usize, TS::Q)>,
    seq: Path<TS::Q, TS::Sigma>,
}

pub type PartialRun<'a, Q, S, L> = (Path<Q, S>, S, Repr<'a, S, L>, usize);

impl<'a, TS: Successor, L: InducesFromPath> Cane<'a, TS, L> {
    pub fn new<I: Into<Repr<'a, TS::Sigma, L>>>(into_repr: I, ts: TS, source: TS::Q) -> Self {
        Self {
            ts,
            repr: into_repr.into(),
            position: 0,
            seen: Set::new(),
            seq: Path::empty(source),
        }
    }

    fn step(&mut self) -> Option<TransitionOf<TS>> {
        if let Some(transition) = self
            .repr
            .nth(self.position)
            .and_then(|sym| self.ts.transition_for(self.seq.reached(), sym))
        {
            if self
                .seen
                .insert((self.position, transition.source().clone()))
            {
                self.position += 1;
                self.seq
                    .extend_with(transition.sym().clone(), transition.2.clone());
                Some(transition)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn take_all_steps(&mut self) {
        while let Some(transition) = self.step() {
            trace!("Took transition {:?}", transition);
        }
    }

    fn current_sym(&self) -> Option<TS::Sigma> {
        self.repr.nth(self.position)
    }

    fn extract_partial_run(self) -> PartialRun<'a, TS::Q, TS::Sigma, L> {
        let Some(sym) = self.current_sym() else {
            panic!("Cannot extract partial run if run has been completed!")
        };
        debug_assert!(
            self.repr.length().is_end(self.position)
                || self.ts.successor(self.seq.reached(), &sym).is_none(),
        );
        (self.seq, sym, self.repr, self.position)
    }

    fn result(
        mut self,
    ) -> Result<L::Induces<TS::Q, TS::Sigma>, PartialRun<'a, TS::Q, TS::Sigma, L>> {
        self.take_all_steps();
        let len = self.repr.length();
        if !len.is_end(self.position) {
            if self
                .seen
                .contains(&(self.position, self.seq.reached().clone()))
            {
                // already seen, we have a loop
                Ok(len.induces_from_path::<TS>(&self.seq))
            } else {
                // partial infinite run
                Err(self.extract_partial_run())
            }
        } else {
            Err(self.extract_partial_run())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ts::{transitionsystem::TransitionSystem, AnonymousGrowable, Growable},
        words::Str,
    };

    use super::*;

    #[test]
    fn basic_run() {
        let mut ts: TransitionSystem<u32> = TransitionSystem::new();
        let q0 = ts.add_new_state();
        let q1 = ts.add_new_state();
        let q2 = ts.add_new_state();
        ts.add_transition(&q0, 'a', &q1);
        ts.add_transition(&q0, 'b', &q0);
        ts.add_transition(&q1, 'a', &q2);
        ts.add_transition(&q1, 'b', &q0);
        ts.add_transition(&q2, 'a', &q2);
        ts.add_transition(&q2, 'b', &q0);

        let w = Str::from("abba");
        todo!()
        // assert_eq!(ts.run_from(q0, &w).evaluate(), Ok(q1));
    }

    #[test]
    fn basic_run_with_missing() {
        let mut ts: TransitionSystem<u32> = TransitionSystem::new();
        let q0 = ts.add_new_state();
        let q1 = ts.add_new_state();
        let q2 = ts.add_new_state();
        ts.add_transition(&q0, 'a', &q1);
        ts.add_transition(&q0, 'b', &q0);
        ts.add_transition(&q1, 'a', &q2);
        ts.add_transition(&q1, 'b', &q0);
        ts.add_transition(&q2, 'b', &q0);

        let w = Str::from("abaaa");
        let result = w.run_in_from(&ts, q0);
        assert!(result.is_err());
        println!("{:?}", result);
        let other_res = ts.run_from(&w, q0);
        // {
        //     let mut run = ts.walk(q0, &w);
        //     assert_eq!(run.next(), Some(RunOutput::transition(q0, 'a', q1)));
        //     assert_eq!(run.next(), Some(RunOutput::transition(q1, 'b', q0)));
        //     assert_eq!(run.next(), Some(RunOutput::transition(q0, 'a', q1)));
        //     assert_eq!(run.next(), Some(RunOutput::transition(q1, 'a', q2)));
        //     assert_eq!(run.next(), Some(RunOutput::missing(q2, 'a')));
        // }

        // assert_eq!(ts.run_from(q0, &w).evaluate(), Ok(q0))
    }

    #[test]
    fn input_to_run() {
        let mut ts: TransitionSystem<u32> = TransitionSystem::new();
        let q0 = ts.add_new_state();
        let q1 = ts.add_new_state();
        let q2 = ts.add_new_state();
        ts.add_transition(&q0, 'a', &q1);
        ts.add_transition(&q0, 'b', &q0);
        ts.add_transition(&q1, 'a', &q2);
        ts.add_transition(&q1, 'b', &q0);
        ts.add_transition(&q2, 'b', &q0);

        todo!()
        // assert_eq!(ts.run_from(q0, "abba").evaluate(), Ok(q1));
    }
}
