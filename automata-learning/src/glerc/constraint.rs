use std::fmt::Debug;

use automata::{
    congruence::CongruenceTrigger,
    run::{EscapePrefix, Run},
    BuchiCondition, Class, Equivalent, RightCongruence, Set, Subword, Symbol, TriggerIterable,
    Word,
};
use itertools::Itertools;

use crate::acceptance::AcceptanceError;

use super::state::GlercInfo;

/// Represents a constraint that can be verified during the execution of the GLERC algorithm.
pub trait Constraint<S: Symbol, X> {
    type Output;

    /// Verifies that under the given information, the constraint is satisfied.
    fn satisfied<'s, W: Subword<S = S> + Run<RightCongruence<S>, <W as Word>::Kind, Induces = X>>(
        &self,
        info: &'s GlercInfo<'s, S, W>,
    ) -> Result<Self::Output, ConstraintError<'s, S, W>>;
}

/// A constraint that is always satisfied.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyConstraint;

/// A constraint that checks whether the sample words are all separated, meaning that
/// positive and negative words never end up in the same state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReachabilityConstraint;

/// Constraint for verifying that the escaping words/escape prefixes for positive and
/// negative words can be separated from each other. In other words this is violated
/// if a positive and a negative word escape from the same state with the same suffix.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EscapeSeparabilityConstraint;

/// Constraint, which ensures that the induced objects can be separated from each other.
/// In other words this is violated if the positive and negative words induce the same
/// object in the current congruence.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InducedSeparabilityConstraint;

/// Constraint, which ensures that the transition system can be endowed with a Büchi acceptance
/// condition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuchiConstraint;

/// Constraint, which ensures that the transition system can be endowed with a parity acceptance
/// condition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParityConstraint;

impl<S: Symbol, X> Constraint<S, X> for EmptyConstraint {
    type Output = ();

    fn satisfied<
        's,
        W: Subword<S = S> + Run<RightCongruence<S>, <W as Word>::Kind, Induces = X>,
    >(
        &self,
        _: &'s GlercInfo<'s, S, W>,
    ) -> Result<(), ConstraintError<'s, S, W>> {
        Ok(())
    }
}

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

impl<S: Symbol, X> Constraint<S, X> for EscapeSeparabilityConstraint {
    type Output = ();

    fn satisfied<
        's,
        W: Subword<S = S> + Run<RightCongruence<S>, <W as Word>::Kind, Induces = X>,
    >(
        &self,
        info: &'s GlercInfo<'s, S, W>,
    ) -> Result<(), ConstraintError<'s, S, W>> {
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

    fn satisfied<
        's,
        W: Subword<S = S> + Run<RightCongruence<S>, <W as Word>::Kind, Induces = X>,
    >(
        &self,
        info: &'s GlercInfo<'s, S, W>,
    ) -> Result<(), ConstraintError<'s, S, W>> {
        for (lword, lind) in &info.induced.0 {
            for (rword, rind) in &info.induced.1 {
                if lind == rind {
                    return Err(ConstraintError::SameInduced(*lword, *rword));
                }
            }
        }
        Ok(())
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

/// Encapsulates what can go wrong during an execution of GLERC.
/// The lifetime parameter `'s` is the lifetime of the sample.
/// The symbol type parameter `S` is the symbol type and `W` is the word type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintError<'s, S: Symbol, W: Subword> {
    /// A positive and a negative word escape from the same state with the same suffix.
    /// Contains a reference to the positive word and its escape prefix, followed by a
    /// reference to the negative word and its escape prefix.
    SameEscape(
        &'s W,
        &'s EscapePrefix<Class<S>, W>,
        &'s W,
        &'s EscapePrefix<Class<S>, W>,
    ),
    /// The contained positive and negative word induce the same object.
    SameInduced(&'s W, &'s W),
    /// The Computation of acceptance was unsuccessful, refer to [`AcceptanceError`] for details.
    Acceptance(AcceptanceError<'s, S, W>),
}

fn escape_consistent<'s, S: Symbol, W: Subword<S = S> + Eq>(
    set_x: &'s [(&'s W, EscapePrefix<Class<S>, W>)],
    set_y: &'s [(&'s W, EscapePrefix<Class<S>, W>)],
) -> Result<(), ConstraintError<'s, S, W>>
where
    W::SuffixType: PartialEq,
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

#[cfg(test)]
mod tests {

    use automata::{ts::Trivial, upw, Class, Growable, Pointed, RightCongruence};

    use crate::{
        glerc::{constraint::ReachabilityConstraint, state::GlercState, GlercSignal},
        sample::Sample,
    };

    use super::BuchiConstraint;

    #[test]
    fn reachability_from_induced() {
        let mut ts = RightCongruence::trivial();
        let q0 = ts.initial();
        let q1 = Class::from("b");
        assert!(ts.add_state(&q1));
        ts.add_transition(&q0, 'a', &q0);
        ts.add_transition(&q1, 'a', &q1);
        ts.add_transition(&q0, 'b', &q1);
        ts.add_transition(&q1, 'b', &q0);
    }

    #[test]
    fn dfa_learning() {
        let sample = Sample::from_iters(["a", "ab"], ["", "b", "aa"]);
        let mut glerc =
            GlercState::new(&sample, RightCongruence::trivial(), ReachabilityConstraint);
        assert!(matches!(glerc.step(), GlercSignal::MissingTransition(..)));
        assert!(matches!(glerc.step(), GlercSignal::FailedInsertion(..)));
        assert!(matches!(glerc.step(), GlercSignal::NewState(..)));
        assert!(matches!(glerc.step(), GlercSignal::MissingTransition(..)));
        assert!(matches!(glerc.step(), GlercSignal::SuccessfulInsertion(..)));
        assert!(matches!(glerc.step(), GlercSignal::MissingTransition(..)));
        assert!(matches!(glerc.step(), GlercSignal::SuccessfulInsertion(..)));
        assert!(matches!(glerc.step(), GlercSignal::MissingTransition(..)));
        assert!(matches!(glerc.step(), GlercSignal::FailedInsertion(..)));
        assert!(matches!(glerc.step(), GlercSignal::SuccessfulInsertion(..)));

        match glerc.step() {
            GlercSignal::Finished(result) => println!("{}", result),
            _ => unreachable!("Execution should be finished by now!"),
        }
    }

    #[test]
    fn dba_learning() {
        let sample = Sample::from_iters(
            [
                upw!("b"),
                upw!("bbbabbaba"),
                upw!("abbb"),
                upw!("babb"),
                upw!("bbab"),
                upw!("bbba"),
            ],
            [upw!("a"), upw!("ba"), upw!("bba")],
        );

        let glerc = GlercState::new(&sample, RightCongruence::trivial(), BuchiConstraint);
        let aut: automata::CongruenceDba = glerc.into();
        println!("{}", aut);
    }
}
