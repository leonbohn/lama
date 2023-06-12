use automata::{
    congruence::CongruenceTrigger, words::Length, Class, Equivalent, RightCongruence, Set, Subword,
    Successor, Symbol, Word,
};
use itertools::Itertools;

use super::{Constraint, EscapeSeparabilityConstraint, InducedSeparabilityConstraint};

impl<'a, W> Constraint<W::S> for EscapeSeparabilityConstraint<'a, W>
where
    W: Subword,
{
    type Output = (
        Vec<<W::Len as Length>::Induces<Class<W::S>, W::S>>,
        Vec<<W::Len as Length>::Induces<Class<W::S>, W::S>>,
    );

    type Error = (&'a W, &'a W, W::SuffixType);

    fn satisfied(&self, cong: &RightCongruence<W::S>) -> Result<Self::Output, Self::Error> {
        let mut positive_induced = Vec::new();
        let mut negative_induced = Vec::new();
        let mut positive_escaping = Vec::new();

        for pos_word in self.0.positive_iter() {
            match cong.run(pos_word) {
                Ok(induced) => positive_induced.push(induced),
                Err(partial_run) => {
                    positive_escaping.push(partial_run);
                }
            }
        }

        for neg_word in self.0.negative_iter() {
            match cong.run(neg_word) {
                Ok(induced) => negative_induced.push(induced),
                Err(negative_partial_run) => {
                    for positive_partial_run in &positive_escaping {
                        if negative_partial_run.reached() == positive_partial_run.reached()
                            && negative_partial_run.suffix() == positive_partial_run.suffix()
                        {
                            return Err((
                                positive_partial_run.input(),
                                negative_partial_run.input(),
                                positive_partial_run.suffix(),
                            ));
                        }
                    }
                }
            }
        }

        Ok((positive_induced, negative_induced))
    }
}

impl<'a, W> Constraint<W::S> for InducedSeparabilityConstraint<'a, W>
where
    W: Subword,
{
    type Output = (
        Vec<<W::Len as Length>::Induces<Class<W::S>, W::S>>,
        Vec<<W::Len as Length>::Induces<Class<W::S>, W::S>>,
    );

    type Error = (&'a W, &'a W, <W::Len as Length>::Induces<Class<W::S>, W::S>);

    fn satisfied(&self, cong: &RightCongruence<W::S>) -> Result<Self::Output, Self::Error> {
        let mut positives = Vec::new();
        let mut negatives = Vec::new();
        for pos_word in self.0.positive_iter() {
            if let Ok(pos_induced) = cong.run(pos_word) {
                positives.push(pos_induced.clone());
                for neg_word in self.0.negative_iter() {
                    if let Ok(neg_induced) = cong.run(neg_word) {
                        negatives.push(neg_induced.clone());
                        if pos_induced == neg_induced {
                            return Err((pos_word, neg_word, pos_induced));
                        }
                    }
                }
            }
        }
        Ok((positives, negatives))
    }
}
