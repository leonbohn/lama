use automata::{
    congruence::CongruenceTrigger, run::Run, BuchiCondition, RightCongruence, Set, Subword, Symbol,
    TriggerIterable, Word,
};

use crate::{acceptance::AcceptanceError, glerc::state::GlercInfo};

use super::{
    BuchiConstraint, Constraint, ConstraintError, EscapeSeparabilityConstraint,
    InducedSeparabilityConstraint, ReachabilityConstraint,
};

impl<S: Symbol, X: Eq> Constraint<S, X> for ReachabilityConstraint {
    type Output = ();

    fn satisfied<
        's,
        W: Subword<S = S> + Run<RightCongruence<S>, <W as Word>::Kind, Induces = X>,
    >(
        &self,
        info: &'s GlercInfo<'s, S, W>,
    ) -> Result<(), ConstraintError<'s, S, W>> {
        EscapeSeparabilityConstraint.satisfied(info)?;
        InducedSeparabilityConstraint.satisfied(info)
    }
}

impl<S: Symbol> Constraint<S, Set<CongruenceTrigger<S>>> for BuchiConstraint {
    type Output = BuchiCondition<CongruenceTrigger<S>>;

    fn satisfied<
        's,
        W: Subword<S = S>
            + Run<RightCongruence<S>, <W as Word>::Kind, Induces = Set<CongruenceTrigger<S>>>,
    >(
        &self,
        info: &'s GlercInfo<'s, S, W>,
    ) -> Result<Self::Output, ConstraintError<'s, S, W>> {
        // A description of this algorithm can be found in the paper
        // ["Constructing deterministic ω-automata from examples by an extension of the RPNI algorithm"](https://arxiv.org/pdf/2108.03735.pdf)
        let mut union = Set::new();
        for (_word, induced) in &info.induced.1 {
            union.extend(induced.iter().cloned());
        }

        // Now we check if there is some positive loop that is fully contained in the union
        if let Some((_word, _)) = info
            .induced
            .0
            .iter()
            .find(|(_, induced)| union.is_superset(induced))
        {
            return Err(ConstraintError::Acceptance(
                AcceptanceError::BuchiPositiveContained,
            ));
        }

        // Now we can compute the actual condition
        let condition_triggers: Set<_> = info
            .cong
            .triggers_iter()
            .filter(|trigger| !union.contains(*trigger))
            .cloned()
            .collect();

        Ok(BuchiCondition(condition_triggers))
    }
}
