use automata::{ReachabilityCondition, Set, State, Word};

/// Represents an error that can occur when trying to infer an acceptance condition.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AcceptanceError<'s, S, W> {
    /// Some miscellaneous problem occurred.
    Other(&'s S, &'s W),
    /// A problem when comping a Buchi condition was encountered.
    BuchiPositiveContained,
    /// Is returned when there was a problem with computing a reachability condition.
    ReachabilityProblem,
}

/// Implementors of this trait can be inferred from a set of positive and negative
/// induced objects. In the case of finite words and reachability conditions, for example,
/// this would amount to checking that no positive and negative induced object coincide, i.e.
/// that no positive and negative word reach the same state.
pub trait TryFromInduced {
    /// The type of induced object.
    type Induced;

    /// Performs the inference.
    fn from_induced<'w, W>(
        positive: &[(&'w W, Self::Induced)],
        negative: &[(&'w W, Self::Induced)],
    ) -> Result<Self, AcceptanceError<'w, W::S, W>>
    where
        Self: std::marker::Sized,
        W: Word + std::hash::Hash + Eq;
}

impl<Q: State> TryFromInduced for ReachabilityCondition<Q> {
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
