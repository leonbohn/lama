use std::{borrow::Borrow, fmt::Debug};

use hoars::State as HoaState;
use tracing::trace;

use crate::{
    ts::{HasStates, InputOf, Path, StateOf, TransitionOf},
    words::IsInfinite,
    PeriodicWord, Pointed, Set, State, Str, Subword, Successor, Symbol, UltimatelyPeriodicWord,
    Value, Word,
};

use super::{RunOutput, Walk};

pub trait Runnable: Subword {
    type Induces<Q: State>: Eq + Clone + Debug;

    fn run_in_from<TS>(
        &self,
        ts: TS,
        from: StateOf<TS>,
    ) -> Result<
        Self::Induces<StateOf<TS>>,
        (
            Path<StateOf<TS>, Self::S>,
            StateOf<TS>,
            InputOf<TS>,
            Self::SuffixType,
        ),
    >
    where
        TS: Successor<Sigma = Self::S>;

    fn run_in<TS>(
        &self,
        ts: TS,
    ) -> Result<
        Self::Induces<StateOf<TS>>,
        (
            Path<StateOf<TS>, Self::S>,
            StateOf<TS>,
            InputOf<TS>,
            Self::SuffixType,
        ),
    >
    where
        TS: Successor<Sigma = Self::S> + Pointed,
    {
        let initial = ts.initial();
        self.run_in_from(ts, initial)
    }
}

impl<S: Symbol> Runnable for Str<S> {
    type Induces<Q: State> = Q;

    fn run_in_from<TS>(
        &self,
        ts: TS,
        from: StateOf<TS>,
    ) -> Result<
        Self::Induces<StateOf<TS>>,
        (
            Path<StateOf<TS>, Self::S>,
            StateOf<TS>,
            InputOf<TS>,
            Self::SuffixType,
        ),
    >
    where
        TS: Successor<Sigma = S>,
    {
        trace!("Running from state {:?} on word {:?}", from, self);
        let mut trace = Path::empty(from.clone());
        for run_output in ts.walk(from, &self) {
            match run_output {
                RunOutput::Transition(q, a, p) => {
                    trace!("Encountered transition {:?} --{:?}--> {:?}", q, a, p);
                    trace += (q, a, p.clone());
                }
                RunOutput::WordEnd(q) => return Ok(q),
                RunOutput::Missing(q, a) => {
                    let suffix = self.skip(trace.len());
                    return Err((trace, q, a, suffix));
                }
                RunOutput::FailedBefore => unreachable!(),
            }
        }
        unreachable!()
    }
}

impl<S: Symbol> Runnable for UltimatelyPeriodicWord<S> {
    type Induces<Q: State> = Set<(Q, S, Q)>;

    fn run_in_from<TS>(
        &self,
        ts: TS,
        from: StateOf<TS>,
    ) -> Result<
        Self::Induces<StateOf<TS>>,
        (
            Path<StateOf<TS>, Self::S>,
            StateOf<TS>,
            InputOf<TS>,
            Self::SuffixType,
        ),
    >
    where
        TS: Successor<Sigma = S>,
    {
        trace!("Running from state {:?} on word {:?}", from, self);
        let input = self;
        let prefix_length = input.base_length();
        let recur_length = input.recur_length();
        let prefix = input.prefix(prefix_length);
        match prefix.run_in_from(&ts, from) {
            Err((path, q, a, suffix)) => {
                let suffix = self.skip(path.len());
                Err((path, q, a, suffix))
            }
            Ok(reached) => {
                let recur = input.skip(prefix_length);
                let mut seen = Set::new();
                let mut walker = ts.walk(reached, &recur);
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
                        Err(RunOutput::Transition(..)) => {
                            unreachable!("We failed to take a full piece!")
                        }
                        Err(RunOutput::Missing(q, a)) => {
                            let suffix = recur.skip(walker.position());
                            return Err((walker.seq.into(), q, a, suffix));
                        }
                        Err(RunOutput::FailedBefore) => unreachable!("We would have noticed!"),
                    }
                }
            }
        }
    }
}

pub type EvaluateErr<TS> = (Vec<TransitionOf<TS>>, StateOf<TS>, InputOf<TS>);
pub trait Evaluate {
    type TS: Successor;
    type Induces: Clone + Eq;

    fn evaluate(&self) -> Result<Self::Induces, EvaluateErr<Self::TS>>;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Run<TS: Successor, W> {
    ts: TS,
    input: W,
    origin: TS::Q,
}

impl<TS: Successor, W: Word<S = TS::Sigma>> Run<TS, W> {
    /// Creates a new run on the given transition system with the given input.
    pub fn new<X: Borrow<TS::Q>>(on: TS, from: X, word: W) -> Self {
        Self {
            ts: on,
            input: word,
            origin: from.borrow().clone(),
        }
    }

    /// Returns the transition system on which the run is performed.
    pub fn ts(&self) -> &TS {
        &self.ts
    }

    /// Returns the input word of the run.
    pub fn input(&self) -> &W {
        &self.input
    }
}

impl<TS: Successor> Evaluate for Run<TS, &Str<InputOf<TS>>> {
    type TS = TS;
    type Induces = StateOf<TS>;

    fn evaluate(&self) -> Result<StateOf<TS>, EvaluateErr<TS>> {
        trace!("In call to evaluate");
        let mut trace = Vec::new();
        for run_output in self.ts.walk(self.origin.clone(), &self.input) {
            match run_output {
                RunOutput::Transition(q, a, p) => {
                    trace!("Encountered transition {:?} --{:?}--> {:?}", q, a, p);
                    trace.push((q, a, p.clone()));
                }
                RunOutput::WordEnd(q) => return Ok(q),
                RunOutput::Missing(q, a) => return Err((trace, q, a)),
                RunOutput::FailedBefore => unreachable!(),
            }
        }
        unreachable!()
    }
}

impl<TS: Successor<Sigma = char>> Evaluate for Run<TS, &str> {
    type TS = TS;

    type Induces = StateOf<TS>;

    fn evaluate(&self) -> Result<Self::Induces, EvaluateErr<TS>> {
        self.ts
            .run_from(self.origin.clone(), &Str::from(self.input))
            .evaluate()
    }
}

impl<TS: Successor> Evaluate for Run<TS, &UltimatelyPeriodicWord<InputOf<TS>>> {
    type TS = TS;
    type Induces = Set<TransitionOf<TS>>;

    fn evaluate(&self) -> Result<Set<TransitionOf<TS>>, EvaluateErr<TS>> {
        let input = self.input.borrow();
        let prefix_length = input.base_length();
        let recur_length = input.recur_length();
        let prefix = input.prefix(prefix_length);
        match self.ts.run_from(self.origin.clone(), &prefix).evaluate() {
            Err(e) => Err(e),
            Ok(reached) => {
                let recur = input.skip(prefix_length);
                let mut seen = Set::new();
                let mut walker = self.ts.walk(reached, &recur);
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
                        Err(RunOutput::Transition(..)) => {
                            unreachable!("We failed to take a full piece!")
                        }
                        Err(RunOutput::Missing(q, a)) => return Err((walker.seq, q, a)),
                        Err(RunOutput::FailedBefore) => unreachable!("We would have noticed!"),
                    }
                }
            }
        }
    }
}

impl<TS: Successor> Evaluate for Run<TS, &PeriodicWord<InputOf<TS>>> {
    type TS = TS;

    type Induces = Set<TransitionOf<Self::TS>>;

    fn evaluate(&self) -> Result<Self::Induces, EvaluateErr<TS>> {
        self.ts()
            .run_from(
                self.origin.clone(),
                &UltimatelyPeriodicWord::from(self.input.clone()),
            )
            .evaluate()
    }
}
