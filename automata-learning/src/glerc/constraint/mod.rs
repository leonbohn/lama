mod acceptance;
mod myhillnerode;
mod separability;

use automata::{
    run::{EscapePrefix, Induces},
    ts::IntoTransitions,
    words::WordKind,
    Class, RightCongruence, Subword, Symbol, Word,
};

use crate::acceptance::AcceptanceError;

pub use myhillnerode::MyhillNerodeConstraint;

use super::info::{GlercInfo, ProvidesGlercInfo};

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
    fn satisfied<'s, W: Subword<S = S> + Induces<RightCongruence<S>, WordKind<W>, Induces = X>>(
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
    /// Used for the [`RandomConstraint`], if the generated number prevents the insertion.
    Random,
    /// Is emitted when the Myhill-Nerode constraint is violated.
    // TODO: Add more information about the violation.
    MyhillNerodeInconsistent,
}

/// A constraint that is always satisfied.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyConstraint;

/// A constraint that can be used for the generation of random transition systems.
/// The given number is the number of states that the generated transition system should have,
/// it must be greater than 0.
/// The second given number presents a limit on the number of states that the generated
/// transition system may have. If the number of states of the generated transition system
/// exceeds this limit, then the constraint is immediately satisfied.
#[derive(Debug, Clone, PartialEq)]
pub struct RandomConstraint(pub f64, pub usize);

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

/// [`Constraint`], which is used for the computation of the progress right congruence of a FORC.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IterationConstraint;

impl<S: Symbol, X> Constraint<S, X> for RandomConstraint {
    type Output = ();

    fn satisfied<'s, W: Subword<S = S> + Induces<RightCongruence<S>, WordKind<W>, Induces = X>>(
        &self,
        info: &'s GlercInfo<'s, S, W>,
    ) -> Result<Self::Output, ConstraintError<'s, S, W>> {
        if info.cong.size() > self.1 {
            return Ok(());
        }
        if self.0 >= alea::f64_in_range(0.0, 1.0) {
            Ok(())
        } else {
            Err(ConstraintError::Random)
        }
    }
}

impl<S: Symbol, X> Constraint<S, X> for EmptyConstraint {
    type Output = ();

    fn satisfied<'s, W: Subword<S = S> + Induces<RightCongruence<S>, WordKind<W>, Induces = X>>(
        &self,
        info: &'s GlercInfo<'s, S, W>,
    ) -> Result<Self::Output, ConstraintError<'s, S, W>> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use automata::{ts::Trivial, upw, Class, Growable, Pointed, RightCongruence, Str};
    use tracing_test::traced_test;

    use crate::{
        glerc::{
            constraint::ReachabilityConstraint, provider::LengthLexicographicMissing,
            state::GlercState, GlercSignal,
        },
        sample::Sample,
    };

    use super::{BuchiConstraint, RandomConstraint};

    #[test]
    fn random_ts() {
        let sample = Sample::from_iters(["a", "ab"], ["", "b", "aa"]);
        let mut glerc: GlercState<
            char,
            Str<_>,
            RandomConstraint,
            LengthLexicographicMissing<char>,
        > = GlercState::new(
            LengthLexicographicMissing::from_iter(['a', 'b']),
            RightCongruence::trivial(),
            RandomConstraint(0.2, 10),
        );
        let ts: RightCongruence<_> = glerc.into();
        println!("{}", ts);
    }

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
