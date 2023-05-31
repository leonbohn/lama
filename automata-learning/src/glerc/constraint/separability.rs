use automata::{
    congruence::CongruenceTrigger,
    run::{EscapePrefix, Evaluate, Induces},
    words::WordKind,
    Class, Equivalent, RightCongruence, Run, Subword, Successor, Symbol, Word,
};
use itertools::Itertools;

use super::{Constraint, EscapeSeparabilityConstraint, InducedSeparabilityConstraint};

impl<'a, W> Constraint<W::S> for EscapeSeparabilityConstraint<'a, W>
where
    W: Word,
    for<'b> Run<&'b RightCongruence<W::S>, &'a W>: Evaluate<TS = &'b RightCongruence<W::S>>,
{
    type Output = ();

    type Error = (&'a W, &'a W, CongruenceTrigger<W::S>);

    fn satisfied(&self, cong: &RightCongruence<W::S>) -> Result<Self::Output, Self::Error> {
        for pos_word in self.0.positive_iter() {
            if let Err((_, q, a)) = cong.run(pos_word).evaluate() {
                for neg_word in self.0.negative_iter() {
                    if let Err((_, p, b)) = cong.run(neg_word).evaluate() {
                        if p == q && a == b {
                            return Err((pos_word, neg_word, (p, a)));
                        }
                    }
                }
            }
        }
        todo!()
    }
}

impl<'a, W> Constraint<W::S> for InducedSeparabilityConstraint<'a, W>
where
    W: Word,
    for<'b> Run<&'b RightCongruence<W::S>, &'a W>: Evaluate<TS = &'b RightCongruence<W::S>>,
{
    type Output = ();

    type Error = (&'a W, &'a W, CongruenceTrigger<W::S>);

    fn satisfied(&self, cong: &RightCongruence<W::S>) -> Result<Self::Output, Self::Error> {
        todo!()
    }
}
