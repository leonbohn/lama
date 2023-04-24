mod acceptance;
mod myhillnerode;
mod separability;

use automata::{
    run::{EscapePrefix, Run},
    words::WordKind,
    Class, RightCongruence, Subword, Symbol, Word,
};

use crate::acceptance::AcceptanceError;

use super::state::GlercInfo;

pub use myhillnerode::MyhillNerodeConstraint;

/// Represents a constraint that can be verified during the execution of the GLERC algorithm.
///
/// The type parameter `S` is the (input) symbol type and `X` is the type of the induced
/// object.
pub trait Constraint<S: Symbol, X> {
    /// The type of the output produced by the constraint. If the constraint simply verifies
    /// that some conditions are met, then it may return `()` as output. In other cases, for
    /// example in a [`BuchiConstraint`], the output is a [`BuchiAcceptance`] condition.
    type Output;

    /// Verifies that under the given information, the constraint is satisfied.
    fn satisfied<'s, W: Subword<S = S> + Run<RightCongruence<S>, WordKind<W>, Induces = X>>(
        &self,
        info: &'s GlercInfo<'s, S, W>,
    ) -> Result<Self::Output, ConstraintError<'s, S, W>>;
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IterationConstraint;

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

#[cfg(test)]
mod tests {

    use automata::{ts::Trivial, upw, Class, Growable, Pointed, RightCongruence};
    use tracing_test::traced_test;

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