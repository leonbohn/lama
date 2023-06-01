use std::borrow::Borrow;

use automata::{
    congruence::CongruenceTrigger,
    output::Mapping,
    run::{Evaluate, Induces},
    words::{IsFinite, WordKind},
    BuchiCondition, Class, HasAlphabet, Map, RightCongruence, Run, Set, Str, Subword, Successor,
    Symbol, TriggerIterable, Word,
};
use itertools::Itertools;
use tracing::trace;

use crate::{
    acceptance::AcceptanceError,
    passive::{FiniteSample, OmegaSample},
};

use super::{
    ConflictConstraint, Constraint, EscapeSeparabilityConstraint, InducedSeparabilityConstraint,
};

/// Constraint, which ensures that the transition system can be endowed with a Büchi acceptance
/// condition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuchiConstraint<'a, S: Symbol>(pub &'a OmegaSample<S>);

/// Constraint, which ensures that the transition system can be endowed with a parity acceptance
/// condition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParityConstraint;

/// A constraint that checks whether the sample words are all separated, meaning that
/// positive and negative words never end up in the same state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReachabilityConstraint<S: Symbol>(pub ConflictConstraint<S>, pub FiniteSample<S>);

impl<S: Symbol> ReachabilityConstraint<S> {
    /// Creates a new constraint from the given sample.
    pub fn new<B: Borrow<FiniteSample<S>>>(sample: B) -> Self {
        let sample = sample.borrow();
        Self(
            ConflictConstraint::from_finite_sample(sample),
            sample.clone(),
        )
    }
}

impl<S: Symbol> Constraint<S> for ReachabilityConstraint<S> {
    type Output = Mapping<Class<S>, bool>;

    type Error = String;

    fn satisfied(&self, cong: &RightCongruence<S>) -> Result<Self::Output, Self::Error> {
        trace!("Verifying constructed conflict relation");
        self.0.satisfied(cong).map_err(|e| {
            format!(
                "Conflict constraint not satisfied because of words {} and {}",
                e.0, e.1
            )
        })?;

        let (positive_induced, negative_induced) = self.1.induced(cong);

        trace!("Verifying that positive and negative words do not reach the same state");
        for (pos_word, pos_ind) in &positive_induced {
            for (neg_word, neg_ind) in &negative_induced {
                if pos_ind == neg_ind {
                    return Err(format!(
                        "Words {} and {} are not separated, both reach {}",
                        pos_word, neg_word, neg_ind
                    ));
                }
            }
        }

        trace!("Computing resulting final states");
        Ok(Mapping::from_iter(
            positive_induced
                .into_iter()
                .map(|(_, reached)| (reached, true))
                .chain(
                    negative_induced
                        .into_iter()
                        .map(|(_, reached)| (reached, false)),
                ),
        ))
    }
}

impl<'a, S: Symbol> Constraint<S> for BuchiConstraint<'a, S> {
    type Output = Mapping<CongruenceTrigger<S>, bool>;

    type Error = String;

    fn satisfied(&self, cong: &RightCongruence<S>) -> Result<Self::Output, Self::Error> {
        let (positives, negatives) = EscapeSeparabilityConstraint(self.0)
            .satisfied(cong)
            .map_err(|e| {
                format!(
                    "Escape separability constraint not satisfied because of words {} and {}",
                    e.0, e.1
                )
            })?;

        let neg_union = negatives.iter().fold(Set::new(), |mut acc, set| {
            acc.extend(set.into_iter().map(|(c, a, _)| (c, a)));
            acc
        });

        trace!(
            "Computed unions of negative loops:\t{}",
            neg_union
                .iter()
                .map(|(c, a)| format!("({c}, {a})"))
                .join(", ")
        );

        for positive_induced in positives {
            let positive_induced_triggers: Set<_> =
                positive_induced.iter().map(|(p, a, _)| (p, a)).collect();
            if positive_induced_triggers.is_subset(&neg_union) {
                return Err(format!(
                    "Positive word {} is not separated from negative words",
                    positive_induced
                        .into_iter()
                        .map(|(p, a, _)| format!("({p}, {a})"))
                        .join(", ")
                ));
            }
        }

        let alphabet = cong.alphabet();
        let mut mapping = Map::new();
        for state in cong.states_canonical() {
            for sym in &alphabet {
                if neg_union.contains(&(state, sym)) {
                    mapping.insert((state.clone(), sym.clone()), false);
                } else {
                    mapping.insert((state.clone(), sym.clone()), true);
                }
            }
        }
        Ok(mapping.into())
    }
}
