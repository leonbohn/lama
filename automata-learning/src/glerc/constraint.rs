use std::fmt::Debug;

use automata::{run::EscapePrefix, Class, Equivalent, RightCongruence, Subword, Symbol};
use itertools::Itertools;

use crate::acceptance::AcceptanceError;

pub trait Constraint {
    fn satisfied<'s, S: Symbol, W: Subword<S = S>>(
        &self,
        ts: &RightCongruence<S>,
    ) -> Result<(), ConstraintError<'s, S, W>>;
}

pub struct ReachabilityConstraint<'s, S>(&'s S);
pub struct BuchiConstraint<'s, S>(&'s S);
pub struct CoBuchiConstraint<'s, S>(&'s S);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EscapeSeparabilityConstraint;
pub struct InducedSeparabilityConstraint<'s, S>(&'s S);

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

impl Constraint for EscapeSeparabilityConstraint {
    fn satisfied<'s, S: Symbol, W: Subword<S = S>>(
        &self,
        _state: &RightCongruence<S>,
    ) -> Result<(), ConstraintError<'s, S, W>> {
        todo!()
    }
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
    use automata::{ts::Trivial, Class, Growable, Pointed, RightCongruence};

    use crate::sample::Sample;

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
    fn congruence_learning() {
        let _sample = Sample::from_parts(["a", "aa"], ["", "ab", "b"]);
        let mut ts = RightCongruence::trivial();
        let q0 = ts.initial();
        let q1 = Class::from("a");
        assert!(ts.add_state(&q1));
        ts.add_transition(&q0, 'a', &q1);
        ts.add_transition(&q1, 'a', &q1);
        ts.add_transition(&q1, 'b', &q0);
        ts.add_transition(&q0, 'b', &q0);
    }
}
