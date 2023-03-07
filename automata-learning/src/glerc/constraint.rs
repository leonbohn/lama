use std::{fmt::Debug, hash::Hash};

use automata::{
    run::{EscapePrefix, InitialRun, Run},
    AcceptanceCondition, Equivalent, FiniteKind, Pointed, ReachabilityCondition, Set, StateIndex,
    Subword, Word,
};
use itertools::{Either, Itertools};

use crate::{
    forcs::RightCongruence,
    sample::{Sample, SampleKind},
};

pub trait Constraint<Obj> {
    fn compute(&self, ts: &RightCongruence) -> Result<Obj, ConstraintError>;

    fn satisfied(&self, ts: &RightCongruence) -> bool {
        self.compute(ts).is_ok()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintError {
    SameEscape(String),
    SameInduced(String),
    AcceptanceCondition,
}

pub trait FromInduced<W>: AcceptanceCondition {
    fn from_induced<'w>(
        positive: &[(&'w W, Self::Induced)],
        negative: &[(&'w W, Self::Induced)],
    ) -> Result<Self, ConstraintError>
    where
        Self: std::marker::Sized;
}

fn escape_consistent<'w, Q: StateIndex, W: Subword + Eq>(
    set_x: &[(&'w W, EscapePrefix<Q, W>)],
    set_y: &[(&'w W, EscapePrefix<Q, W>)],
) -> Result<(), ConstraintError>
where
    W::SuffixType: PartialEq,
{
    if let Some(((positive_word, positive_prefix), (negative_word, negative_prefix))) = set_x
        .iter()
        .cartesian_product(set_y.iter())
        .find(|(x, y)| x.1.equivalent(&y.1))
    {
        Err(ConstraintError::SameEscape(format!(
            "{:?} and {:?} are not separable: {:?} ~ {:?}",
            positive_word, negative_word, positive_prefix, negative_prefix
        )))
    } else {
        Ok(())
    }
}

fn induced_consistent<'w, Ind: Eq + Debug, W: Word>(
    set_x: &[(&'w W, Ind)],
    set_y: &[(&'w W, Ind)],
) -> Result<(), ConstraintError> {
    if let Some(((positive_word, positive_induced), (negative_word, negative_induced))) = set_x
        .iter()
        .cartesian_product(set_y.iter())
        .find(|(x, y)| x.1 == y.1)
    {
        Err(ConstraintError::SameInduced(format!(
            "{:?} and {:?} are not separable: both induce {:?}",
            positive_word, negative_word, positive_induced
        )))
    } else {
        Ok(())
    }
}

impl<I, S> Constraint<I> for S
where
    I: FromInduced<S::SampleWord>,
    I::Induced: Eq + Hash,
    S: Sample,
    S::SampleWord: Run<RightCongruence, SampleKind<S>, Induces = I::Induced> + Eq + Hash,
    <S::SampleWord as Subword>::SuffixType: PartialEq,
{
    fn compute(&self, ts: &RightCongruence) -> Result<I, ConstraintError> {
        let ((positive_induced, positive_escaping), (negative_induced, negative_escaping)) =
            self.partition(ts);

        // check for esacpe consistency, the question mark bubbles up the error, if one occurs
        escape_consistent(&positive_escaping, &negative_escaping)?;

        I::from_induced(&positive_induced, &negative_induced)
    }
}

impl<S: Sample> Constraint<RightCongruence> for S
where
    S::SampleWord: Run<RightCongruence, SampleKind<S>>,
    <S::SampleWord as Run<RightCongruence, SampleKind<S>>>::Induces: Eq + Debug + Hash,
    <S::SampleWord as Subword>::SuffixType: PartialEq,
{
    fn compute(&self, ts: &RightCongruence) -> Result<RightCongruence, ConstraintError> {
        let ((positive_induced, positive_escaping), (negative_induced, negative_escaping)) =
            self.partition(ts);

        escape_consistent(&positive_escaping, &negative_escaping)?;
        induced_consistent(&positive_induced, &negative_induced)?;
        Ok(ts.to_owned())
    }
}

pub type MaybeReachabilityCondition<Q> = Result<ReachabilityCondition<Q>, ConstraintError>;

impl<Q, W> FromInduced<W> for ReachabilityCondition<Q>
where
    Q: StateIndex,
    W: Word<Kind = FiniteKind> + Hash + Eq,
{
    fn from_induced<'w>(
        positive: &[(&'w W, Self::Induced)],
        negative: &[(&'w W, Self::Induced)],
    ) -> Result<Self, ConstraintError>
    where
        Self: std::marker::Sized,
    {
        let pos = positive.iter().collect::<Set<_>>();
        if negative.iter().any(|q| pos.contains(q)) {
            Err(ConstraintError::AcceptanceCondition)
        } else {
            Ok(ReachabilityCondition::new(
                pos.into_iter().map(|(_, q)| q).cloned().collect(),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use automata::{ts::Trivial, Growable, Pointed};

    use crate::{forcs::RightCongruence, sample::VecSample};

    use super::{Constraint, MaybeReachabilityCondition};

    #[test]
    fn reachability_from_induced() {
        let mut ts = RightCongruence::trivial();
        let q0 = ts.initial();
        let q1 = "b".into();
        assert!(ts.add_state(&q1));
        ts.add_transition(&q0, 'a', &q0);
        ts.add_transition(&q1, 'a', &q1);
        ts.add_transition(&q0, 'b', &q1);
        ts.add_transition(&q1, 'b', &q0);

        let sample = VecSample(vec!["a", "b"], vec!["b"]);
        let condition: MaybeReachabilityCondition<_> = sample.compute(&ts);
        assert_eq!(condition, Err(super::ConstraintError::AcceptanceCondition));
    }
}
