use automata::{
    run::{EscapePrefix, Induces},
    words::WordKind,
    Class, Equivalent, RightCongruence, Subword, Symbol, Word,
};
use itertools::Itertools;

use crate::glerc::info::GlercInfo;

use super::{
    Constraint, ConstraintError, EscapeSeparabilityConstraint, InducedSeparabilityConstraint,
};

fn escape_consistent<'s, S, W>(
    set_x: &'s [(&'s W, EscapePrefix<Class<S>, W>)],
    set_y: &'s [(&'s W, EscapePrefix<Class<S>, W>)],
) -> Result<(), ConstraintError<'s, S, W>>
where
    W::SuffixType: PartialEq,
    S: Symbol,
    W: Subword<S = S> + Eq + Clone,
{
    if let Some(((positive_word, positive_prefix), (negative_word, negative_prefix))) = set_x
        .iter()
        .cartesian_product(set_y.iter())
        .find(|(x, y)| x.1.equivalent(&y.1))
    {
        Err(ConstraintError::SameEscape(
            positive_word,
            positive_prefix,
            negative_word,
            negative_prefix,
        ))
    } else {
        Ok(())
    }
}

fn induced_consistent<'s, S: Symbol, W: Subword<S = S> + Eq>(
    set_x: &[(&'s W, EscapePrefix<Class<S>, W>)],
    set_y: &[(&'s W, EscapePrefix<Class<S>, W>)],
) -> Result<(), ConstraintError<'s, S, W>> {
    if let Some(((positive_word, _positive_induced), (negative_word, _negative_induced))) = set_x
        .iter()
        .cartesian_product(set_y.iter())
        .find(|(x, y)| x.1 == y.1)
    {
        Err(ConstraintError::SameInduced(positive_word, negative_word))
    } else {
        Ok(())
    }
}

impl<S: Symbol, X> Constraint<S, X> for EscapeSeparabilityConstraint {
    type Output = ();

    fn satisfied<'s, W: Subword<S = S> + Induces<RightCongruence<S>, WordKind<W>, Induces = X>>(
        &self,
        info: &'s GlercInfo<'s, S, W>,
    ) -> Result<Self::Output, ConstraintError<'s, S, W>> {
        for (lword, lesc) in &info.escaping.0 {
            for (rword, resc) in &info.escaping.1 {
                if lesc.equivalent(resc) {
                    return Err(ConstraintError::SameEscape(*lword, lesc, *rword, resc));
                }
            }
        }
        Ok(())
    }
}

impl<S: Symbol, X: Eq> Constraint<S, X> for InducedSeparabilityConstraint {
    type Output = ();

    fn satisfied<'s, W: Subword<S = S> + Induces<RightCongruence<S>, WordKind<W>, Induces = X>>(
        &self,
        info: &'s GlercInfo<'s, S, W>,
    ) -> Result<Self::Output, ConstraintError<'s, S, W>> {
        for (lword, lind) in &info.induced.0 {
            for (rword, rind) in &info.induced.1 {
                if lind == rind {
                    return Err(ConstraintError::SameInduced(lword, rword));
                }
            }
        }
        Ok(())
    }
}
