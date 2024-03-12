use automata::{automaton::Buchi, prelude::*};

use super::OmegaSample;

/// This trait represents the sprout passive learning algorithm for omega automata 
/// from <https://arxiv.org/pdf/2108.03735.pdf>.
/// The trait is supposed to be implemented on omega acceptance types
pub trait SproutLearner {
    /// type of the automaton to be returned
    type Aut;

    /// gives a deterministic omega automaton that is consistent with the given sample
    fn sprout(sample: OmegaSample) -> Self::Aut;
}

impl SproutLearner for Buchi {
    type Aut = DBA;

    fn sprout(sample: OmegaSample) -> Self::Aut {
        todo!()
    }
}
