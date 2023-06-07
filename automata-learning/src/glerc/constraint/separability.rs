use automata::{
    congruence::CongruenceTrigger,
    run::{EscapePrefix, Runnable},
    Class, Equivalent, RightCongruence, Set, Subword, Successor, Symbol, Word,
};
use itertools::Itertools;

use super::{Constraint, EscapeSeparabilityConstraint, InducedSeparabilityConstraint};

impl<'a, W> Constraint<W::S> for EscapeSeparabilityConstraint<'a, W>
where
    W: Runnable,
{
    type Output = (
        Vec<<W as Runnable>::Induces<Class<W::S>>>,
        Vec<<W as Runnable>::Induces<Class<W::S>>>,
    );

    type Error = (&'a W, &'a W, CongruenceTrigger<W::S>);

    fn satisfied(&self, cong: &RightCongruence<W::S>) -> Result<Self::Output, Self::Error> {
        let mut positive_induceed = Vec::new();
        let mut negative_induced = Vec::new();
        let mut positive_escaping = Vec::new();

        for pos_word in self.0.positive_iter() {
            match pos_word.run_in(cong) {
                Ok(induced) => positive_induceed.push(induced),
                Err((_seq, q, a, suffix)) => {
                    positive_escaping.push((pos_word, q, a, suffix));
                }
            }
        }

        for neg_word in self.0.negative_iter() {
            match neg_word.run_in(cong) {
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
    W: Runnable,
{
    type Output = (
        Vec<<W as Runnable>::Induces<RightCongruence<W::S>>>,
        Vec<<W as Runnable>::Induces<RightCongruence<W::S>>>,
    );

    type Error = (&'a W, &'a W, <W as Runnable>::Induces<Class<W::S>>);

    fn satisfied(&self, cong: &RightCongruence<W::S>) -> Result<Self::Output, Self::Error> {
        let mut positives = Vec::new();
        let mut negatives = Vec::new();
        for pos_word in self.0.positive_iter() {
            if let Ok(pos_induced) = pos_word.run_in(cong) {
                positives.push(pos_induced.clone());
                for neg_word in self.0.negative_iter() {
                    if let Ok(neg_induced) = neg_word.run_in(cong) {
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
