use automata::{
    congruence::CongruenceTrigger, Class, Dfa, Pair, RightCongruence, Set, State, Subword, Symbol,
    TransitionSystem, UltimatelyPeriodicWord, DFA,
};

use crate::passive::Sample;

use super::Constraint;

/// A constraint which ensures that the constructed right congruence is Myhill-Nerode consistent. A more
/// detailed explanation of this constraint and its computation can be found in the proof of Lemma 23
/// (which is in the appendix) of the (paper)[https://arxiv.org/abs/2302.11043].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MyhillNerodeConstraint<Q: State, P: State, S: Symbol> {
    product: (TransitionSystem<Pair<Q, P>, S>, Pair<Q, P>),
    conflicts: Set<(Q, P)>,
}

impl<Q: State, P: State, S: Symbol> MyhillNerodeConstraint<Q, P, S> {
    /// Creates a new Myhill-Nerode constraint, which ensures that if two words
    /// lead to the same state, they are equivalent under the right congruence.
    pub fn new(sample: &Sample<UltimatelyPeriodicWord<S>>) -> Self {
        // let positive = sample.positive_prefixes();
        // let negative = sample.negative_prefixes();
        // let product = positive.ts().product_with_transitions(&negative);
        todo!()
    }
}
