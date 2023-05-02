use automata::{
    congruence::CongruenceTrigger, run::Run, words::WordKind, Class, Dfa, Pair, RightCongruence,
    Set, StateIndex, Subword, Symbol, TransitionSystem, UltimatelyPeriodicWord, DFA,
};

use crate::sample::Sample;

use super::Constraint;

/// A constraint which ensures that the constructed right congruence is Myhill-Nerode consistent. A more
/// detailed explanation of this constraint and its computation can be found in the proof of Lemma 23
/// (which is in the appendix) of the (paper)[https://arxiv.org/abs/2302.11043].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MyhillNerodeConstraint<Q: StateIndex, P: StateIndex, S: Symbol> {
    product: (TransitionSystem<Pair<Q, P>, S>, Pair<Q, P>),
    conflicts: Set<(Q, P)>,
}

impl<Q: StateIndex, P: StateIndex, S: Symbol> MyhillNerodeConstraint<Q, P, S> {
    pub fn new(sample: &Sample<UltimatelyPeriodicWord<S>>) -> Self {
        let positive = sample.positive_prefixes();
        let negative = sample.negative_prefixes();
        let product = positive.ts().product_with_transitions(&negative);
        todo!()
    }
}

impl<Q: StateIndex, P: StateIndex, S: Symbol> Constraint<S, Set<CongruenceTrigger<S>>>
    for MyhillNerodeConstraint<Q, P, S>
{
    type Output = ();

    fn satisfied<
        's,
        W: Subword<S = S> + Run<RightCongruence<S>, WordKind<W>, Induces = Set<CongruenceTrigger<S>>>,
    >(
        &self,
        info: &'s crate::glerc::info::GlercInfo<'s, S, W>,
    ) -> Result<Self::Output, super::ConstraintError<'s, S, W>> {
        todo!();
        let product = self.product.0.product_with_transitions(info.cong);
        let initial = Pair::new(self.product.1, Class::epsilon());
        let reachable = product.reachable_states_from(initial);
        if reachable
            .iter()
            .any(|state| self.conflicts.contains(&state.left().raw()))
        {
            Err(super::ConstraintError::MyhillNerodeInconsistent)
        } else {
            Ok(())
        }
    }
}
