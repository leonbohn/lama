use crate::{
    words::{IsFinite, IsInfinite},
    Boundedness, FiniteKind, InfiniteKind, PeriodicWord, Pointed, Set, StateIndex, Str, Subword,
    TransitionSystem, Word,
};

use super::{EscapePrefix, RunOutput, Walk};

pub struct Configuration<TS: TransitionSystem, W> {
    pub start: TS::Q,
    pub q: TS::Q,
    pub word: W,
    pub ts: TS,
}

impl<TS: TransitionSystem, W> Configuration<TS, W> {
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

    pub fn build(ts: TS, q: TS::Q, word: W) -> Self {
        Configuration {
            ts,
            word,
            start: q.clone(),
            q,
        }
    }
}

pub trait Evaluate {
    type Output;
    type Failure;

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
        let cfg = Configuration::build(&self.ts, self.start.clone(), prefix);
        let res = match cfg.eval() {
            Err(e) => todo!(),
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
        };
        res
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
