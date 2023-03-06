use automata::{
    run::{EscapePrefix, Run},
    AcceptanceCondition, Pointed, ReachabilityCondition, Set, StateIndex, Subword,
    TransitionSystem,
};
use itertools::{Either, Itertools};

use crate::sample::{Sample, SampleKind};

pub trait Constraint<TS, For> {
    fn compute(&self, ts: &TS) -> Result<For, ConstraintError>;

    fn satisfied(&self, ts: &TS) -> bool {
        self.compute(ts).is_ok()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintError {
    SameEscape,
    AcceptanceCondition,
}

pub trait FromInduced: AcceptanceCondition {
    fn from_induced(
        positive: Vec<Self::Induced>,
        negative: Vec<Self::Induced>,
    ) -> Result<Self, ConstraintError>
    where
        Self: std::marker::Sized;
}

fn escape_consistent<Q: StateIndex, W: Subword>(
    set_x: Vec<EscapePrefix<Q, W>>,
    set_y: Vec<EscapePrefix<Q, W>>,
) -> Result<(), ConstraintError> {
    if set_x.into_iter().any(|x| set_y.iter().any(|y| *y == x)) {
        Err(ConstraintError::SameEscape)
    } else {
        Ok(())
    }
}

impl<TS, S, Acc> Constraint<TS, Acc> for S
where
    TS: TransitionSystem + Pointed,
    S: Sample,
    Acc: FromInduced<Kind = SampleKind<S>>,
    S::SampleWord: Run<TS, SampleKind<S>, Induces = Acc::Induced>, //AcceptanceCondition<Induced = <S::SampleWord as Run<TS, SampleKind<S>>>::Induces>,
{
    fn compute(&self, ts: &TS) -> Result<Acc, ConstraintError> {
        let (positive_induced, positive_escaping) = self
            .positive()
            .map(|(word, _)| word.run(ts, ts.initial()))
            .partition_map(|run| match run {
                Ok(induced) => Either::Left(induced),
                Err(escaping) => Either::Right(escaping),
            });
        let (negative_induced, negative_escaping) = self
            .negative()
            .map(|(word, _)| word.run(ts, ts.initial()))
            .partition_map(|run| match run {
                Ok(induced) => Either::Left(induced),
                Err(escaping) => Either::Right(escaping),
            });

        // check for esacpe consistency, the question mark bubbles up the error, if one occurs
        escape_consistent(positive_escaping, negative_escaping)?;

        Acc::from_induced(positive_induced, negative_induced)
    }
}

pub type MaybeReachabilityCondition<Q> = Result<ReachabilityCondition<Q>, ConstraintError>;

impl<Q: StateIndex> FromInduced for ReachabilityCondition<Q> {
    fn from_induced(
        positive: Vec<Self::Induced>,
        negative: Vec<Self::Induced>,
    ) -> MaybeReachabilityCondition<Q>
    where
        Self: std::marker::Sized,
    {
        let pos = positive.into_iter().collect::<Set<_>>();
        if negative.iter().any(|q| pos.contains(q)) {
            Err(ConstraintError::AcceptanceCondition)
        } else {
            Ok(ReachabilityCondition::new(pos))
        }
    }
}

#[cfg(test)]
mod tests {
    use automata::{Dfa, Growable, Pointed};

    use super::{Constraint, MaybeReachabilityCondition};

    #[test]
    fn reachability_from_induced() {
        let mut ts = Dfa::trivial();
        let q0 = ts.initial();
        let q1 = ts.add_state();
        ts.add_transition(q0, 'a', q0);
        ts.add_transition(q1, 'a', q1);
        ts.add_transition(q0, 'b', q1);
        ts.add_transition(q1, 'b', q0);

        let sample = (vec!["a", "b"], vec!["b"]);
        let condition: MaybeReachabilityCondition<_> = sample.compute(&ts);
        assert_eq!(condition, Err(super::ConstraintError::AcceptanceCondition));
    }
}
