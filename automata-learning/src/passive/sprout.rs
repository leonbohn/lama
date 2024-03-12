use automata::{automaton::Buchi, prelude::*};

use super::OmegaSample;

pub trait SproutLearner {
    type Aut;

    fn sprout(sample: OmegaSample) -> Self::Aut;
}

impl SproutLearner for Buchi {
    type Aut = DBA;

    fn sprout(sample: OmegaSample) -> Self::Aut {
        todo!()
    }
}
