use std::hash::Hash;

use automata::{run::InitialRun, Class, RightCongruence, Set, Subword, Successor, Symbol, Word};

use crate::sample::Sample;

pub trait ProvidesMissing<S: Symbol> {
    fn next_missing(&self, congruence: &RightCongruence<S>) -> Option<(Class<S>, S)>;
}

pub struct LengthLexicographicMissing<S> {
    alphabet: Set<S>,
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
