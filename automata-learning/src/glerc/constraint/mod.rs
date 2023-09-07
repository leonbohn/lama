mod acceptance;
pub use acceptance::*;

mod myhillnerode;
mod separability;

use std::fmt::{Debug, Display};

use automata::{
    ts::{IntoParts, IntoStates, IntoTransitions},
    Class, Pair, Predecessor, RightCongruence, Set, Str, Subword, Successor, Symbol, Transformer,
    TransitionSystem, UltimatelyPeriodicWord, Word,
};
use itertools::Itertools;
use tracing::trace;

use crate::{
    acceptance::AcceptanceError,
    passive::{FiniteSample, OmegaSample, Sample},
};

pub use myhillnerode::MyhillNerodeConstraint;

/// Represents a constraint that can be verified during the execution of the GLERC algorithm.
///
/// The type parameter `S` is the (input) symbol type and `X` is the type of the induced
/// object.
pub trait Constraint<S: Symbol> {
    /// The type of the output produced by the constraint. If the constraint simply verifies
    /// that some conditions are met, then it may return `()` as output. In other cases, for
    /// example in a [`BuchiConstraint`], the output is a [`BuchiAcceptance`] condition.
    type Output: Debug;

    /// The type of the error that is returned if the constraint is not satisfied.
    type Error: Debug;

    /// Verifies that under the given information, the constraint is satisfied.
    fn satisfied(&self, cong: &RightCongruence<S>) -> Result<Self::Output, Self::Error>;
}

/// A constraint that is always satisfied.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyConstraint;

/// Constraint for verifying that the escaping words/escape prefixes for positive and
/// negative words can be separated from each other. In other words this is violated
/// if a positive and a negative word escape from the same state with the same suffix.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EscapeSeparabilityConstraint<'a, W: Word>(&'a Sample<W>);

/// Constraint, which ensures that the induced objects can be separated from each other.
/// In other words this is violated if the positive and negative words induce the same
/// object in the current congruence.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InducedSeparabilityConstraint<'a, W: Word>(&'a Sample<W>);

/// [`Constraint`], which is used for the computation of the progress right congruence of a FORC.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IterationConstraint;

/// Constraint modeled by a pair of congruences A, B (note, that these are effectively deterministic transition
/// systems) and a set of conflicting pairs C ⊆ A × B. The constraint is then satisfied by a congruence T, if
/// there exists no pair (a, b) ∈ C such that (x, a) and (x, b) are reachable in T x A and T x B, respectively.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ConflictConstraint<S: Symbol> {
    left: RightCongruence<S>,
    right: RightCongruence<S>,
    conflicts: Set<(Class<S>, Class<S>)>,
}

impl<S: Symbol> ConflictConstraint<S> {
    /// Construct a [`ConflictConstraint`] from a sample of ultimately periodic words. This first computes DFAs for
    /// the prefixes of the positive and the negative parts of the sample. Call them P and N respectively. It then
    /// builds the product of P and N restricted to final states. A pair of states (p, n) is now added to the conflict
    /// relation C, if the state (p, n) in P x N can reach a cycle in the product. This is the Myhill-Nerode constraint
    /// for omega-languages.
    pub fn mn_from_omega_sample(sample: &OmegaSample<S>) -> Self {
        trace!("Constructing conflict constraint from omega sample");
        let time = std::time::Instant::now();
        let left = sample.positive_prefixes();
        let right = sample.negative_prefixes();

        trace!(
            "Took {} ms for constructing conflict constraint from omega sample.",
            time.elapsed().as_millis()
        );
        let time = std::time::Instant::now();
        let restricted_product = left
            .product(&right)
            .restrict(
                |Pair {
                     left: left_state,
                     right: right_state,
                 }| left.apply(left_state) && right.apply(right_state),
            )
            .into_ts();

        trace!(
            "Took {} ms for building restricted product.",
            time.elapsed().as_millis()
        );
        let time = std::time::Instant::now();
        let on_cycle: Set<_> = restricted_product
            .into_states()
            .filter(|state| restricted_product.is_on_cycle(*state))
            .collect();

        // build the product restricted to final states
        trace!(
            "Took {} ms for computing on-cycle states.",
            time.elapsed().as_millis()
        );
        let time = std::time::Instant::now();
        let conflicts = on_cycle
            .into_iter()
            .flat_map(|pair| restricted_product.reached_by(pair))
            .map(|pair| (pair.left().clone(), pair.right().clone()))
            .collect();

        trace!(
            "Took {} ms for computing conflicts.",
            time.elapsed().as_millis()
        );
        Self {
            left: left.into(),
            right: right.into(),
            conflicts,
        }
    }

    /// Construct a [`ConflictConstraint`] from a sample of finite words. The procedure is similar to [`ConflictConstraint::mn_from_omega_sample`], but
    /// it differs in the way that pairs (p, n) are chosen for the conflict relation. Here, a pair (p, n) is added to the conflict relation, if the
    /// it is reachable in the product P x N.
    pub fn from_finite_sample(sample: &FiniteSample<S>) -> Self {
        trace!("Constructing conflict constraint from omega sample");
        let time = std::time::Instant::now();

        let left = sample.positive_prefixes();
        let right = sample.negative_prefixes();

        trace!(
            "took {} ms, Building restricted product",
            time.elapsed().as_millis()
        );
        let time = std::time::Instant::now();
        let restricted_product = left
            .product(&right)
            .restrict(
                |Pair {
                     left: left_state,
                     right: right_state,
                 }| left.apply(left_state) && right.apply(right_state),
            )
            .into_ts();

        trace!(
            "took {} ms, Computing on-cycle states",
            time.elapsed().as_millis()
        );
        let time = std::time::Instant::now();
        let to_reach: Set<_> = restricted_product
            .into_states()
            .filter(|state| left.apply(state.left()) && right.apply(state.right()))
            .collect();

        // build the product restricted to final states
        trace!(
            "took {} ms, Computing conflicts",
            time.elapsed().as_millis()
        );
        let time = std::time::Instant::now();
        let conflicts = to_reach
            .into_iter()
            .flat_map(|pair| restricted_product.reached_by(pair))
            .map(|pair| (pair.left().clone(), pair.right().clone()))
            .collect();

        trace!("took {} ms", time.elapsed().as_millis());
        Self {
            left: left.into(),
            right: right.into(),
            conflicts,
        }
    }

    fn iteration_constraint(
        sample: &OmegaSample<S>,
        leading: &RightCongruence<S>,
        class: Class<S>,
    ) -> Self {
        todo!()
    }
}

impl<S: Symbol> Constraint<S> for EmptyConstraint {
    type Output = ();

    type Error = ();

    fn satisfied(&self, cong: &RightCongruence<S>) -> Result<Self::Output, Self::Error> {
        Ok(())
    }
}

impl<S: Symbol, O: Debug, E: Debug> Constraint<S> for fn(&RightCongruence<S>) -> Result<O, E> {
    type Output = O;

    type Error = E;

    fn satisfied(&self, cong: &RightCongruence<S>) -> Result<Self::Output, Self::Error> {
        (*self)(cong)
    }
}

impl<S: Symbol + Display> Constraint<S> for ConflictConstraint<S> {
    type Output = ();

    type Error = (Class<S>, Class<S>);

    fn satisfied(&self, cong: &RightCongruence<S>) -> Result<Self::Output, Self::Error> {
        let left_product = cong.product(&self.left);
        let right_product = cong.product(&self.right);
        trace!(
            "Seeing if conflict constraint is satisfied for\n{}\n{}\n{}",
            cong,
            left_product.into_ts(),
            right_product.into_ts()
        );

        let left_product_reachable = left_product.reachable_states();
        let right_product_reachable = right_product.reachable_states();
        trace!(
            "Left reachable: {}\nRight reachable: {}",
            left_product_reachable
                .iter()
                .map(|q| q.to_string())
                .join(", "),
            right_product_reachable
                .iter()
                .map(|q| q.to_string())
                .join(", ")
        );

        for left in left_product.reachable_states() {
            for right in right_product.reachable_states() {
                trace!("Reached {} and {}", left, right);
                if left.left() == right.left()
                    && self
                        .conflicts
                        .contains(&(left.right.clone(), right.right.clone()))
                {
                    return Err((left.right, right.right));
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use automata::{
        convert::ToDot, ts::Trivial, upw, word, Class, Growable, Pointed, RightCongruence, Str,
        Transformer,
    };
    use tracing_test::traced_test;

    use crate::{
        glerc::{state::GlercState, GlercSignal},
        passive::{dba_rpni, dfa_rpni, dpa_rpni, Sample},
    };

    use super::{BuchiConstraint, ConflictConstraint};

    #[test]
    fn reachability_from_induced() {
        let mut ts = RightCongruence::trivial();
        let q0 = ts.initial();
        let q1 = Class::from_display("b");
        assert!(ts.add_state(&q1));
        ts.add_transition(&q0, 'a', &q0);
        ts.add_transition(&q1, 'a', &q1);
        ts.add_transition(&q0, 'b', &q1);
        ts.add_transition(&q1, 'b', &q0);
    }

    #[test]
    #[ignore]
    fn dfa_learning() {
        let sample = Sample::from_iters(
            [word!("a"), word!("ab")],
            [word!(""), word!("b"), word!("aa")],
        );
        let dfa = dfa_rpni(&sample);
        println!("{}", dfa);
        dfa.display_rendered().unwrap();
        assert!(sample.consistent_with(dfa))
    }

    #[test]
    #[traced_test]
    fn omega_mn_constraint() {
        let sample = Sample::from_iters(
            [
                upw!("b"),
                upw!("bbbabbaba"),
                upw!("abbabaaaabababababaabb"),
                upw!("babb"),
                upw!("bbab"),
                upw!("bbba"),
            ],
            [upw!("a"), upw!("ba"), upw!("bba")],
        );
        let time = std::time::Instant::now();
        let cons = ConflictConstraint::mn_from_omega_sample(&sample);
        let duration = time.elapsed();
        println!("{:?}", cons);
        println!("Took {} ms", duration.as_millis())
    }

    #[test]
    #[traced_test]
    fn finite_mn_constraint() {
        let sample = Sample::from_iters(
            [
                word!(""),
                word!("aaa"),
                word!("baaa"),
                word!("abaa"),
                word!("aaba"),
                word!("aaab"),
                word!("b"),
            ],
            [
                word!("a"),
                word!("ba"),
                word!("aba"),
                word!("aab"),
                word!("baa"),
            ],
        );
        let constraint = ConflictConstraint::from_finite_sample(&sample);
        println!(
            "{}\n{}\n{}",
            constraint.left,
            constraint.right,
            constraint
                .conflicts
                .iter()
                .map(|(a, b)| format!("({}, {})", a, b))
                .collect::<Vec<_>>()
                .join(", ")
        );

        let mut glerc = GlercState::new(RightCongruence::trivial(), ['a', 'b'], constraint);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
    }

    #[test]
    fn small_finite_mn_constraint() {
        let sample = Sample::from_iters(
            [word!("a"), word!("ab"), word!("ba"), word!("aa")],
            [word!(""), word!("b"), word!("bb")],
        );
        let constraint = ConflictConstraint::from_finite_sample(&sample);
        println!(
            "Left constraint\n{}\nRight constraint\n{}\nConflicts: {}",
            constraint.left,
            constraint.right,
            constraint
                .conflicts
                .iter()
                .map(|(a, b)| format!("({}, {})", a, b))
                .collect::<Vec<_>>()
                .join(", ")
        );

        let mut glerc = GlercState::new(RightCongruence::trivial(), ['a', 'b'], constraint);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.step();
        println!("{:?}", res);
        let res = glerc.execute();
        println!("{}", res.learned_congruence);
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

        let aut = dba_rpni(&sample);
        println!("{}", aut);
    }

    #[test]
    fn dpa_learning_simple() {
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

        let aut = dpa_rpni(&sample);
        println!("{}", aut);
    }

    #[test]
    fn dpa_learning_layered() {
        let sample = Sample::from_iters(
            [
                upw!("abc"),
                upw!("bca"),
                upw!("cba"),
                upw!("ac"),
                upw!("ab"),
                upw!("c"),
            ],
            [upw!("bc"), upw!("b")],
        );

        let aut = dpa_rpni(&sample);
        assert!(aut.size() == 1);
        assert_eq!(aut.apply((Class::epsilon(), 'a')), 0);
        assert_eq!(aut.apply((Class::epsilon(), 'b')), 1);
        assert_eq!(aut.apply((Class::epsilon(), 'c')), 2);
    }
}
