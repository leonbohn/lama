use std::fmt::Debug;

use automata::prelude::*;

use crate::active::{oracle::MealyOracle, LStar};

use super::{FiniteSample, Sample};

type DFASample<D> = FiniteSample<<D as TransitionSystem>::Alphabet, bool>;

pub fn characterize_dfa<D: DFALike>(dfa: D) -> DFASample<D> {
    todo!()
}

pub fn actively_exchanged_words_dfa<D: DFALike>(dfa: D) -> DFASample<D> {
    todo!()
}

type MealySample<D> =
    Sample<<D as TransitionSystem>::Alphabet, Vec<SymbolOf<D>>, <D as TransitionSystem>::EdgeColor>;

pub fn actively_exchanged_words_mealy<D: MealyLike>(mm: D) -> MealySample<D> {
    let alphabet = mm.alphabet().clone();
    let oracle = MealyOracle::new(mm);
    // let mut lstar = LStar::logged(oracle, alphabet);
    // let learned = lstar.learn();
    // let mut sample = MealySample::new(alphabet);
    todo!()
}
