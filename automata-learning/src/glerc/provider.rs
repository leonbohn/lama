use std::hash::Hash;

use automata::{
    run::{Induces, InitialRun},
    Class, RightCongruence, Set, Subword, Successor, Symbol, Word,
};

use crate::sample::Sample;

use super::info::{GlercInfo, ProvidesGlercInfo};

pub trait ProvidesMissing<S: Symbol> {
    fn next_missing(&self, congruence: &RightCongruence<S>) -> Option<(Class<S>, S)>;
}

#[derive(Debug, Clone)]
pub struct LengthLexicographicMissing<S> {
    alphabet: Set<S>,
}

impl<S: Symbol> LengthLexicographicMissing<S> {
    pub fn from_iter<I: IntoIterator<Item = S>>(iter: I) -> Self {
        Self {
            alphabet: Set::from_iter(iter),
        }
    }
}

impl<'a, W> ProvidesMissing<W::S> for &'a Sample<W>
where
    W: Subword + Clone + Hash,
    W: InitialRun<RightCongruence<<W as Word>::S>, <W as Word>::Kind>,
{
    fn next_missing(&self, congruence: &RightCongruence<W::S>) -> Option<(Class<W::S>, W::S)> {
        self.iter()
            .filter_map(|word| word.initial_run(congruence).err())
            .min()
            .map(|ep| ep.trigger())
    }
}

impl<S: Symbol> ProvidesMissing<S> for LengthLexicographicMissing<S> {
    fn next_missing(&self, congruence: &RightCongruence<S>) -> Option<(Class<S>, S)> {
        congruence.states_canonical().find_map(|state| {
            self.alphabet.iter().find_map(|sym| {
                if congruence.successor(state, sym).is_none() {
                    Some((state.clone(), sym.clone()))
                } else {
                    None
                }
            })
        })
    }
}

impl<W> ProvidesGlercInfo<<W as Word>::S, W> for LengthLexicographicMissing<<W as Word>::S>
where
    W: Subword + Induces<RightCongruence<<W as Word>::S>, <W as Word>::Kind> + Clone,
{
    fn build_for<'s>(
        &'s self,
        cong: &'s RightCongruence<<W as Word>::S>,
    ) -> super::info::GlercInfo<'s, <W as Word>::S, W> {
        GlercInfo {
            cong,
            induced: Default::default(),
            escaping: Default::default(),
        }
    }
}
