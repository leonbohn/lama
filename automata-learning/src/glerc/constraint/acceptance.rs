use std::borrow::Borrow;

use automata::{
    congruence::CongruenceTrigger, output::Mapping, BuchiCondition, Class, HasAlphabet, Map,
    RightCongruence, Set, Str, Subword, Successor, Symbol, TriggerIterable, Word,
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
pub struct ParityConstraint<'a, S: Symbol>(pub &'a OmegaSample<S>);

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

        let (positive_induced, negative_induced) = self.1.annotated_induced(cong);

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
                .map(|(_, reached)| (reached.into_inner(), true))
                .chain(
                    negative_induced
                        .into_iter()
                        .map(|(_, reached)| (reached.into_inner(), false)),
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
            acc.extend(set.iter().map(|(c, a)| (c.clone(), a.clone())));
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
            let positive_induced_triggers: Set<_> = positive_induced
                .iter()
                .map(|(p, a)| (p.clone(), a.clone()))
                .collect();
            if positive_induced_triggers.is_subset(&neg_union) {
                return Err(format!(
                    "Positive word {} is not separated from negative words",
                    positive_induced
                        .iter()
                        .map(|(p, a)| format!("({p}, {a})"))
                        .join(", ")
                ));
            }
        }

        let alphabet = cong.alphabet();
        let mut mapping = Map::new();
        for (state, sym) in cong.all_potential_triggers(self.0.alphabet.clone()) {
            if neg_union.contains(&(state.clone(), sym.clone())) {
                mapping.insert((state.clone(), sym.clone()), false);
            } else {
                mapping.insert((state, sym), true);
            }
        }
        Ok(mapping.into())
    }
}

impl<'a, S: Symbol> Constraint<S> for ParityConstraint<'a, S> {
    type Output = Mapping<CongruenceTrigger<S>, usize>;

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

        let mut universe: Set<_> = cong.all_potential_triggers(self.0.alphabet.clone());

        let positives = positives
            .into_iter()
            .map(|set| set.into_iter().map(|(p, a)| (p, a)).collect())
            .collect();
        let negatives: Vec<_> = negatives
            .into_iter()
            .map(|set| set.into_iter().map(|(p, a)| (p, a)).collect())
            .collect();

        let (start, left, right) = if negatives.contains(&universe) {
            (false, negatives, positives)
        } else {
            (true, positives, negatives)
        };

        let zielonka_path = compute_zielonka_path(universe.clone(), left, right, start)
            .ok_or("Could not compute acceptance condition!")?;
        debug_assert!(!zielonka_path.is_empty(), "Path is empty!");
        debug_assert!(
            (0..zielonka_path.len() - 1).all(|i| zielonka_path[i].1 != zielonka_path[i + 1].1),
            "Path is not strictly alternating!"
        );

        trace!(
            "Computed Zielonka path\n{}",
            zielonka_path
                .iter()
                .map(|(set, classification)| {
                    format!(
                        "[{{{}}}: {classification}]",
                        set.iter().map(|(p, a)| format!("({p}, {a})")).join(", ")
                    )
                })
                .join(" | ")
        );

        let base_priority = if zielonka_path[0].1 { 0 } else { 1 };

        let mut mapping = Map::new();
        for (p, a) in universe {
            let index = zielonka_path
                .iter()
                .enumerate()
                .rev()
                .find_map(|(i, (set, _))| {
                    if set.contains(&(p.clone(), a.clone())) {
                        Some(base_priority + i)
                    } else {
                        None
                    }
                })
                .unwrap_or(base_priority);
            mapping.insert((p, a), index);
        }

        Ok(mapping.into())
    }
}

fn compute_zielonka_path<S: Symbol>(
    universe: Set<CongruenceTrigger<S>>,
    left: Vec<Set<CongruenceTrigger<S>>>,
    right: Vec<Set<CongruenceTrigger<S>>>,
    start: bool,
) -> Option<Vec<(Set<CongruenceTrigger<S>>, bool)>> {
    trace!(
        "Computing Zielonka Path for universe {}\nleft\t{}\nright\t{}",
        universe
            .iter()
            .map(|(p, a)| format!("({p}, {a})"))
            .join(", "),
        left.iter()
            .map(|set| format!(
                "{{{}}}",
                set.iter().map(|(p, a)| format!("({p}, {a})")).join(", ")
            ))
            .join(", "),
        right
            .iter()
            .map(|set| format!(
                "{{{}}}",
                set.iter().map(|(p, a)| format!("({p}, {a})")).join(", ")
            ))
            .join(", ")
    );
    let union = right.clone().into_iter().fold(Set::new(), |mut acc, set| {
        if set.is_subset(&universe) {
            acc.extend(set.into_iter());
        }
        acc
    });

    let displayed_union = union.iter().map(|(q, a)| format!("({q}, {a})")).join(", ");

    if union.is_empty() {
        trace!("Union is empty!");
        Some(vec![(universe, start)])
    } else if union == universe {
        trace!("Union equal to universe, {}", displayed_union);
        None
    } else {
        let tail = compute_zielonka_path(union, right, left, !start)?;
        debug_assert!(!tail.is_empty());
        debug_assert!(tail.first().unwrap().1 != start);

        let mut out = vec![(universe, start)];
        out.extend(tail);

        Some(out)
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    #[test]
    #[traced_test]
    fn parity_cons() {}
}
