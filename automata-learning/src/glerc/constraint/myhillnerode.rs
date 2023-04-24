use automata::{
    congruence::CongruenceTrigger, run::Run, words::WordKind, Dfa, RightCongruence, Set,
    StateIndex, Subword, Symbol,
};

use crate::glerc::state::GlercInfo;

use super::Constraint;

/// A constraint which ensures that the constructed right congruence is Myhill-Nerode consistent. A more
/// detailed explanation of this constraint and its computation can be found in the proof of Lemma 23
/// (which is in the appendix) of the (paper)[https://arxiv.org/abs/2302.11043].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MyhillNerodeConstraint<Q: StateIndex, S: Symbol> {
    positive: Dfa<Q, S>,
    negative: Dfa<Q, S>,
    conflicts: Set<(Q, Q)>,
}

impl<Q: StateIndex, S: Symbol> Constraint<S, Set<CongruenceTrigger<S>>>
    for MyhillNerodeConstraint<Q, S>
{
    type Output = ();

    fn satisfied<
        's,
        W: Subword<S = S> + Run<RightCongruence<S>, WordKind<W>, Induces = Set<CongruenceTrigger<S>>>,
    >(
        &self,
        _info: &'s GlercInfo<'s, S, W>,
    ) -> Result<Self::Output, super::ConstraintError<'s, S, W>> {
        // let lp = info.cong.direct_product(&self.positive);
        // let rp = info.cong.direct_product(&self.negative);

        Ok(())
    }
}
