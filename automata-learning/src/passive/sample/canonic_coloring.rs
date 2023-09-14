use automata::{alphabet::Simple, Alphabet, RightCongruence, TransitionSystem, Word};

use crate::{
    priority_mapping::{ClassifiesIdempotents, PriorityMapping},
    AnnotatedCongruence,
};

use super::{OmegaSample, PeriodicOmegaSample};

impl<A: Alphabet> ClassifiesIdempotents<A> for PeriodicOmegaSample<A> {
    fn classify(&self, class: &automata::Class<<A as Alphabet>::Symbol>) -> Option<bool> {
        self.classify(class.omega_power())
    }
}

#[cfg(test)]
mod tests {
    use automata::{ts::ToDot, RightCongruence, TransitionSystem};

    use crate::{passive::sprout::tests::testing_larger_forc_sample, AnnotatedCongruence};

    #[test]
    fn classification() {
        let (alphabet, sample) = testing_larger_forc_sample();
        let forc = sample.infer_forc();
        let split = sample.split(forc.leading());
        let periodic = split.get(0).unwrap().to_periodic_sample();

        let annotated = AnnotatedCongruence::build(forc.prc(0).unwrap(), &periodic);

        let coloring = annotated.canonic_coloring();
        coloring
            .collect::<RightCongruence<_, _, usize>>()
            .display_rendered();
    }
}
