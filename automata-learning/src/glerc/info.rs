use automata::{
    run::{InitialRun, Run},
    RightCongruence, Subword, Symbol, Word,
};
use impl_tools::autoimpl;

use crate::sample::Sample;

use super::state::{EscapePair, InducedPair};

#[autoimpl(for<T: trait> &T)]
pub trait ProvidesGlercInfo<
    S: Symbol,
    W: Subword<S = S> + Run<RightCongruence<S>, <W as Word>::Kind>,
>: Clone
{
    fn build_for<'s>(&'s self, cong: &'s RightCongruence<S>) -> GlercInfo<'s, S, W>;
}

#[derive(Clone, Debug)]
pub struct GlercInfo<'s, S: Symbol, W: Subword<S = S> + Run<RightCongruence<S>, <W as Word>::Kind>>
{
    pub(crate) cong: &'s RightCongruence<S>,
    pub(crate) induced: InducedPair<'s, S, W>,
    pub(crate) escaping: EscapePair<'s, S, W>,
}

impl<W> ProvidesGlercInfo<W::S, W> for Sample<W>
where
    W: Subword + Run<RightCongruence<<W as Word>::S>, <W as Word>::Kind> + Clone,
{
    fn build_for<'t>(
        &'t self,
        cong: &'t RightCongruence<<W as Word>::S>,
    ) -> GlercInfo<'t, <W as Word>::S, W> {
        let mut induced = InducedPair::default();
        let mut escaping = EscapePair::default();
        self.positive_iter()
            .for_each(|word| match word.initial_run(cong) {
                Ok(run) => induced.0.push((word, run)),
                Err(escape) => escaping.0.push((word, escape)),
            });
        self.negative_iter()
            .for_each(|word| match word.initial_run(cong) {
                Ok(run) => induced.1.push((word, run)),
                Err(escape) => escaping.1.push((word, escape)),
            });

        GlercInfo {
            cong,
            induced,
            escaping,
        }
    }
}

impl<'s, S, W> GlercInfo<'s, S, W>
where
    S: Symbol,
    W: Subword<S = S> + Run<RightCongruence<S>, <W as Word>::Kind>,
{
    pub fn update(&mut self, sample: &Sample<W>) {
        unimplemented!()
    }
}
