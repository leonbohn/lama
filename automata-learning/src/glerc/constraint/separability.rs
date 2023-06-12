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

    type Error = (&'a W, &'a W, CongruenceTrigger<W::S>);

    fn satisfied(&self, cong: &RightCongruence<W::S>) -> Result<Self::Output, Self::Error> {
        let mut positive_induceed = Vec::new();
        let mut negative_induced = Vec::new();
        let mut positive_escaping = Vec::new();

        for pos_word in self.0.positive_iter() {
            match cong.run(pos_word) {
                Ok(induced) => positive_induceed.push(induced),
                Err(partial_run) => {
                    positive_escaping.push(partial_run);
                }
            }
        }

        for neg_word in self.0.negative_iter() {
            match cong.run(neg_word) {
                Ok(induced) => negative_induced.push(induced),
                Err((_seq, q, a, suffix)) => {
                    if let Some((pos_word, _, _, _)) =
                        positive_escaping
                            .iter()
                            .find(|(_pos_word, p, b, known_suffix)| {
                                p == &q && &a == b && &suffix == known_suffix
                            })
                    {
                        return Err((pos_word, neg_word, (q, a)));
                    }
                }
            }
        }

        Ok((positive_induceed, negative_induced))
    }
}

impl<'a, W> Constraint<W::S> for InducedSeparabilityConstraint<'a, W>
where
    W: Word,
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
            if let Ok(pos_induced) = cong.run(pos_word.as_repr()) {
                positives.push(pos_induced.clone());
                for neg_word in self.0.negative_iter() {
                    if let Ok(neg_induced) = cong.run(neg_word.as_repr()) {
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
