use crate::{
    words::{IsFinite, IsInfinite},
    FiniteKind, InfiniteKind, Pointed, Set, Subword, TransitionSystem, Word,
};

use super::{EscapePrefix, RunOutput, Walk};

/// Encapsulates the configuration of a run of some `word` in the `ts`, where `start` is the origin
/// where the run started and `q` is the current state of the run.
pub struct Configuration<TS: TransitionSystem, W> {
    /// Where the run originates from.
    pub start: TS::Q,
    /// The state the run is currently in.
    pub q: TS::Q,
    /// The word we are running on.
    pub word: W,
    /// The [`TransitionSystem`] we are running in.
    pub ts: TS,
}

impl<TS: TransitionSystem, W> Configuration<TS, W> {
    /// Creates a cnonfiguration for the given [`Pointed`] [`TransitionSystem`] and [`Word`].
    pub fn for_pointed(ts: TS, word: W) -> Self
    where
        TS: Pointed,
    {
        Configuration {
            q: ts.initial(),
            start: ts.initial(),
            ts,
            word,
        }
    }

    /// Builds a configuration for the given [`TransitionSystem`], state and [`Word`].
    pub fn from_state(ts: TS, q: TS::Q, word: W) -> Self {
        Configuration {
            ts,
            word,
            start: q.clone(),
            q,
        }
    }
}

/// Encapsulates things that can be evaluated to either a positive or negative result. Commonly implemented
/// by a [`Configuration`] and similar things that abstract a run of a [`TransitionSystem`].
pub trait Evaluate {
    /// The type that is returned if the corresponding run is successful.
    type Output;
    /// What is returned if the run is unsuccessful. Usually an [`EscapePrefix`].
    type Failure;

    /// Do the evaluation and return an object of the correct type.
    fn evaluate(&self) -> Result<Self::Output, Self::Failure>;
}

pub trait Eval<K> {
    type Output;
    type Failure;

    fn eval(&self) -> Result<Self::Output, Self::Failure>;
}

impl<TS: TransitionSystem, W: IsFinite + Subword<S = TS::S>> Eval<FiniteKind>
    for Configuration<TS, W>
{
    type Output = TS::Q;
    type Failure = EscapePrefix<TS::Q, W>;

    fn eval(&self) -> Result<Self::Output, Self::Failure> {
        let mut walker = self.ts.walk(self.start.clone(), &self.word);
        let prefix = walker
            .by_ref()
            .take_while(RunOutput::is_trigger)
            .map(|o| o.get_trigger().expect("Must be a trigger!"))
            .collect();
        match walker.next() {
            Some(RunOutput::WordEnd(q)) => Ok(q),
            Some(RunOutput::Missing(q, a)) => Err(EscapePrefix::new(&self.word, prefix, q, a)),
            _ => unreachable!(),
        }
    }
}

impl<TS: TransitionSystem, W: IsInfinite + Subword<S = TS::S>> Eval<InfiniteKind>
    for Configuration<TS, W>
where
    <W as Subword>::PrefixType: IsFinite,
    Configuration<TS, <W as Subword>::PrefixType>: Eval<FiniteKind, Output = TS::Q>,
{
    type Output = Set<(TS::Q, TS::S)>;
    type Failure = EscapePrefix<TS::Q, W>;

    fn eval(&self) -> Result<Self::Output, Self::Failure> {
        let prefix_length = self.word.base_length();
        let recur_length = self.word.recur_length();
        let prefix = self.word.prefix(prefix_length);
        let cfg = Configuration::from_state(&self.ts, self.start.clone(), prefix);
        match cfg.eval() {
            Err(_e) => todo!(),
            Ok(reached) => {
                let recur = self.word.skip(prefix_length);
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
                        Err(RunOutput::Trigger(_, _)) => {
                            unreachable!("We failed to take a full piece!")
                        }
                        Err(RunOutput::Missing(q, a)) => {
                            return Err(EscapePrefix::new(&self.word, walker.seq, q, a))
                        }
                        Err(RunOutput::FailedBefore) => unreachable!("We would have noticed!"),
                    }
                }
            }
        }
    }
}

impl<TS: TransitionSystem, W: Word> Evaluate for Configuration<TS, W>
where
    Configuration<TS, W>: Eval<W::Kind>,
{
    type Output = <Self as Eval<W::Kind>>::Output;
    type Failure = <Self as Eval<W::Kind>>::Failure;

    fn evaluate(&self) -> Result<Self::Output, Self::Failure> {
        self.eval()
    }
}
