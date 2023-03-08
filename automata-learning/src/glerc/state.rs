use automata::{
    run::{EscapePrefix, Run},
    ts::Trivial,
    Mapping, Subword, Symbol, Word,
};

use crate::{
    forcs::{CongruenceClass, CongruenceTrigger, RightCongruence},
    sample::Sample,
};

pub type CongrunenceRunResult<S, W> = Result<
    <W as Run<RightCongruence<S>, <W as Word>::Kind>>::Induces,
    EscapePrefix<CongruenceClass<S>, W>,
>;

/// Holds the current state of the GLERC algorithm
pub struct GlercState<'s, S: Symbol, W: Run<RightCongruence<S>, <W as Word>::Kind>> {
    /// The congruence constructed thus far
    pub(crate) cong: RightCongruence<S>,
    /// The default congruence
    pub(crate) default: RightCongruence<S>,
    /// The sample
    pub(crate) sample: &'s Sample<W>,
    /// The current iteration
    pub(crate) iteration: usize,
    pub(crate) runs: Mapping<&'s W, CongrunenceRunResult<S, W>>,
}

impl<'s, S: Symbol, W: Run<RightCongruence<S>, <W as Word>::Kind, S = S>> GlercState<'s, S, W> {
    pub fn new(sample: &'s Sample<W>, default: RightCongruence<S>) -> Self {
        Self {
            cong: RightCongruence::trivial(),
            default,
            sample,
            iteration: 0,
            runs: Mapping::new(),
        }
    }

    pub fn next_missing(&self) -> Option<CongruenceTrigger<S>> {
        self.runs
            .iter()
            .filter_map(|(_, result)| result.as_ref().err())
            .min_by(|u_result, v_result| u_result.cmp(v_result))
            .map(|escape_prefix| escape_prefix.trigger())
    }
}


