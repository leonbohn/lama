use automata::{automaton::Buchi, prelude::*};

use super::OmegaSample;

/// This trait represents the sprout passive learning algorithm for omega automata
/// from <https://arxiv.org/pdf/2108.03735.pdf>.
/// The trait is supposed to be implemented on omega acceptance types
pub trait SproutLearner {
    /// type of the automaton to be returned
    type Aut;

    /// gives a deterministic omega automaton that is consistent with the given sample
    fn sprout(&self, sample: OmegaSample) -> Self::Aut;
}

impl SproutLearner for Buchi {
    type Aut = DBA;

    fn sprout(&self, sample: OmegaSample) -> Self::Aut {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::passive::OmegaSample;
    use automata::{automaton::Buchi, prelude::*};

    #[test]
    fn sprout_buchi() {
        let sigma = alphabet!(simple 'a', 'b');

        // build sample
        let sample =
            OmegaSample::new_omega_from_pos_neg(sigma, [upw!("a"), upw!("a", "b")], [upw!("b")]);

        // build dba
        let dba = NTS::builder()
            .with_transitions([
                (0, 'a', true, 1),
                (1, 'a', true, 0),
                (1, 'b', true, 1),
                (0, 'b', false, 0),
            ])
            .default_color(Void)
            .deterministic()
            .with_initial(0)
            .into_dba();

        assert!(Buchi.sprout(sample).eq(&dba))
    }
}
