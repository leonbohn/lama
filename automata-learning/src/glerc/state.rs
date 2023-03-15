use automata::{
    run::{Configuration, EscapePrefix, Evaluate},
    ts::Trivial,
    Mapping, Subword, Symbol, TransitionSystem, Word,
};

use crate::{
    forcs::{Class, CongruenceTrigger, RightCongruence},
    sample::Sample,
};

pub struct FreeConfiguration<Q, W> {
    pub(crate) start: Q,
    pub(crate) q: Q,
    pub(crate) word: W,
}

impl<Q: Clone, W: Word + Clone> FreeConfiguration<Q, W> {
    pub fn attach_ts<'c, 't, TS: TransitionSystem<Q = Q, S = W::S>>(
        &'c self,
        ts: &'t TS,
    ) -> Configuration<&'t TS, &'c W> {
        Configuration {
            start: self.start.clone(),
            q: self.q.clone(),
            word: &self.word,
            ts,
        }
    }
}

impl<'t, TS: TransitionSystem, W: Word<S = TS::S>> From<Configuration<&'t TS, W>>
    for FreeConfiguration<TS::Q, W>
{
    fn from(value: Configuration<&'t TS, W>) -> Self {
        Self {
            start: value.start,
            q: value.q,
            word: value.word,
        }
    }
}

/// Holds the current state of the GLERC algorithm
pub struct GlercState<'s, S: Symbol, W: Subword<S = S>> {
    /// The congruence constructed thus far
    pub(crate) cong: RightCongruence<S>,
    /// The default congruence
    pub(crate) default: RightCongruence<S>,
    /// The sample
    pub(crate) sample: &'s Sample<W>,
    /// The current iteration
    pub(crate) iteration: usize,
    pub(crate) runs: Mapping<&'s W, FreeConfiguration<Class<S>, W>>,
}

impl<'s, S, W> GlercState<'s, S, W>
where
    S: Symbol + 's,
    W: Subword<S = S> + Clone + 's,
    Configuration<&'s RightCongruence<S>, &'s W>: Evaluate<Failure = EscapePrefix<Class<S>, W>>,
{
    pub fn new(sample: &'s Sample<W>, default: RightCongruence<S>) -> Self {
        Self {
            cong: RightCongruence::trivial(),
            default,
            sample,
            iteration: 0,
            runs: Mapping::new(),
        }
    }

    pub fn next_missing(&'s self) -> Option<(Class<S>, S)> {
        self.runs
            .iter()
            .filter_map(|(_, result)| result.attach_ts(&self.cong).evaluate().err())
            .min_by(|u_result, v_result| u_result.cmp(v_result))
            .map(|escape_prefix| escape_prefix.trigger())
    }
}
