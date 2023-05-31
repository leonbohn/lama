use automata::{
    congruence::CongruenceTrigger,
    run::{EscapePrefix, Evaluate, Induces},
    words::WordKind,
    Class, Equivalent, RightCongruence, Run, Runnable, Set, Subword, Successor, Symbol, Word,
};
use itertools::Itertools;

use super::{Constraint, EscapeSeparabilityConstraint, InducedSeparabilityConstraint};

impl<'a, W> Constraint<W::S> for EscapeSeparabilityConstraint<'a, W>
where
    W: Runnable,
{
    type Output = ();

    type Error = (&'a W, &'a W, CongruenceTrigger<W::S>);

    fn satisfied(&self, cong: &RightCongruence<W::S>) -> Result<Self::Output, Self::Error> {
        for pos_word in self.0.positive_iter() {
            if let Err((path, q, a, pos_suffix)) = pos_word.run_in(cong) {
                for neg_word in self.0.negative_iter() {
                    if let Err((_, p, b, neg_suffix)) = neg_word.run_in(cong) {
                        if p == q && a == b && pos_suffix == neg_suffix {
                            return Err((pos_word, neg_word, (p, a)));
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

impl<'a, W> Constraint<W::S> for InducedSeparabilityConstraint<'a, W>
where
    W: Runnable,
{
    type Output = (
        Vec<<W as Runnable>::Induces<Class<W::S>>>,
        Vec<<W as Runnable>::Induces<Class<W::S>>>,
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
