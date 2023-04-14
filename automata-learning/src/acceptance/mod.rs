use automata::{ReachabilityCondition, Set, StateIndex, Word};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AcceptanceError<'s, S, W> {
    Other(&'s S, &'s W),
    BuchiProblem,
    ReachabilityProblem,
}

pub trait TryFromInduced {
    type Induced;
    fn from_induced<'w, W>(
        positive: &[(&'w W, Self::Induced)],
        negative: &[(&'w W, Self::Induced)],
    ) -> Result<Self, AcceptanceError<'w, W::S, W>>
    where
        Self: std::marker::Sized,
        W: Word + std::hash::Hash + Eq;
}

impl<Q: StateIndex> TryFromInduced for ReachabilityCondition<Q> {
    type Induced = Q;
    fn from_induced<'w, W: Word + std::hash::Hash + Eq>(
        positive: &[(&'w W, Self::Induced)],
        negative: &[(&'w W, Self::Induced)],
    ) -> Result<Self, AcceptanceError<'w, W::S, W>>
    where
        Self: std::marker::Sized,
    {
        let pos = positive.iter().collect::<Set<_>>();
        if negative.iter().any(|q| pos.contains(q)) {
            Err(AcceptanceError::ReachabilityProblem)
        } else {
            Ok(ReachabilityCondition::new(
                pos.into_iter().map(|(_, q)| q).cloned().collect(),
            ))
        }
    }
}
