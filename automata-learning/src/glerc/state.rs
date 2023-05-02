use std::hash::Hash;

use automata::{
    run::{EscapePrefix, InitialRun, Run},
    AcceptanceCondition, Combined, Growable, Shrinkable, Subword, Symbol, Word,
};
use tracing::trace;

use crate::sample::Sample;
use automata::{Class, RightCongruence};

use super::{
    constraint::Constraint,
    info::{GlercInfo, ProvidesGlercInfo},
    provider::ProvidesMissing,
    GlercOutput, GlercSignal,
};

#[derive(Clone, Debug)]
enum GlercIterationStatus<S: Symbol> {
    /// Indicates that a transition from the first element on symbol given by the second element
    /// should be inserted. The third element keeps track of the set of states and the fourth indicates
    /// the index of the state that is to be attempted as a target.
    TryInsertion(Class<S>, S, Vec<Class<S>>, usize),
    /// Indicates that the algorithm should try to find a missing transition next.
    FindMissing,
    /// The algorithm has finished.
    Finished,
}

/// Holds the current state of the GLERC algorithm.
/// - `S` is the type of the symbols of the words in the sample
/// - `W` is the type of the words in the sample
#[derive(Clone, Debug)]
pub struct GlercState<
    's,
    S: Symbol,
    W: Subword<S = S> + Run<RightCongruence<S>, <W as Word>::Kind>,
    C: Constraint<S, <W as Run<RightCongruence<S>, <W as Word>::Kind>>::Induces>,
    M: ProvidesMissing<S>,
> {
    timer: Result<std::time::Duration, std::time::Instant>,
    /// The congruence constructed thus far
    pub(crate) cong: RightCongruence<S>,
    /// The default congruence
    pub(crate) default: RightCongruence<S>,
    /// The current iteration
    pub(crate) iteration: usize,
    pub(crate) provider: M,
    /// The current status
    status: GlercIterationStatus<S>,
    /// Stores what the sample induces in the current congruence
    pub(crate) induced: InducedPair<'s, S, W>,
    /// Sotres the info on what escapes from the sample in the current congruence
    pub(crate) escaping: EscapePair<'s, S, W>,
    pub(crate) constraint: C,
}

/// Alias for the result of evaluating a configuration
pub(super) type EvalResult<'s, S, W> = <W as Run<RightCongruence<S>, <W as Word>::Kind>>::Induces;
/// Alias for the induced pair
pub(super) type InducedPair<'s, S, W> = (
    Vec<(&'s W, EvalResult<'s, S, W>)>,
    Vec<(&'s W, EvalResult<'s, S, W>)>,
);
/// Alias for the escaping pair
pub(super) type EscapePair<'s, S, W> = (
    Vec<(&'s W, EscapePrefix<Class<S>, W>)>,
    Vec<(&'s W, EscapePrefix<Class<S>, W>)>,
);

impl<'s, S, W, C, P> GlercState<'s, S, W, C, P>
where
    S: Symbol + 's,
    W: Subword<S = S> + Clone + 's + Hash,
    W: InitialRun<RightCongruence<S>, <W as Word>::Kind>,
    C: Constraint<S, <W as Run<RightCongruence<S>, <W as Word>::Kind>>::Induces>,
    P: ProvidesMissing<S> + ProvidesGlercInfo<S, W>,
{
    fn get_status(&self) -> GlercIterationStatus<S> {
        self.status.clone()
    }

    pub fn step(&mut self) -> GlercSignal<S> {
        trace!("Starting iteration {}", self.iteration);
        let (new_status, result) = match &self.get_status() {
            GlercIterationStatus::TryInsertion(from, on, states, index) => {
                trace!(
                    "Trying insertion of {} -> {} at index {}, stateslist: [{}]",
                    from,
                    on,
                    index,
                    states
                        .iter()
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                if let Some(target) = states.get(*index) {
                    self.cong.add_transition(from, on.clone(), target);
                    let success = {
                        let info = self.get_info();
                        self.constraint
                            .satisfied(&self.provider.build_for(&self.cong))
                            .is_ok()
                    };
                    trace!(
                        "Insertion {} -> {} {}",
                        from,
                        on,
                        if success { "succeeded" } else { "failed" }
                    );
                    if success {
                        (
                            GlercIterationStatus::FindMissing,
                            GlercSignal::SuccessfulInsertion(
                                from.clone(),
                                on.clone(),
                                target.clone(),
                            ),
                        )
                    } else {
                        self.cong.remove_transition(from.clone(), on.clone());
                        (
                            GlercIterationStatus::TryInsertion(
                                from.clone(),
                                on.clone(),
                                states.clone(),
                                index + 1,
                            ),
                            GlercSignal::FailedInsertion(from.clone(), on.clone(), target.clone()),
                        )
                    }
                    // Now we verify if the inserted transition leads to a consistent TS
                } else {
                    let new_state = from + on;
                    self.cong.add_state(&new_state);
                    self.cong.add_transition(from, on.clone(), &new_state);
                    (
                        GlercIterationStatus::FindMissing,
                        GlercSignal::NewState(from.clone(), on.clone(), new_state.clone()),
                    )
                }
            }
            GlercIterationStatus::FindMissing => {
                if let Some((q, a)) = self.next_missing() {
                    (
                        GlercIterationStatus::TryInsertion(
                            q.clone(),
                            a.clone(),
                            self.cong.states_canonical().cloned().collect(),
                            0,
                        ),
                        GlercSignal::MissingTransition(q, a),
                    )
                } else {
                    self.timer = if let Err(start) = self.timer {
                        Ok(start.elapsed())
                    } else {
                        self.timer
                    };
                    (
                        GlercIterationStatus::Finished,
                        GlercSignal::Finished(super::GlercOutput::new(
                            &self.cong,
                            self.timer.unwrap(),
                        )),
                    )
                }
            }
            GlercIterationStatus::Finished => (
                GlercIterationStatus::Finished,
                GlercSignal::Finished(GlercOutput::new(&self.cong, self.timer.unwrap())),
            ),
        };

        self.status = new_status;
        result
    }

    /// Creates a new state for the given sample and default congruence
    pub fn new(provider: P, default: RightCongruence<S>, constraint: C) -> Self {
        let cong = RightCongruence::empty_trivial();
        let mut out = Self {
            timer: Err(std::time::Instant::now()),
            cong,
            default,
            iteration: 0,
            status: GlercIterationStatus::FindMissing,
            induced: (Vec::new(), Vec::new()),
            escaping: (Vec::new(), Vec::new()),
            constraint,
            provider,
        };
        out
    }

    pub fn new_from_congruence(
        cong: RightCongruence<S>,
        provider: P,
        default: RightCongruence<S>,
        constraint: C,
    ) -> Self {
        let mut out = Self {
            timer: Err(std::time::Instant::now()),
            cong,
            default,
            iteration: 0,
            status: GlercIterationStatus::FindMissing,
            induced: (Vec::new(), Vec::new()),
            escaping: (Vec::new(), Vec::new()),
            constraint,
            provider,
        };
        out
    }

    fn get_info(&'s self) -> GlercInfo<'s, S, W> {
        self.provider.build_for(&self.cong)
    }

    /// Returns the next missing transition, if any
    pub fn next_missing(&self) -> Option<(Class<S>, S)> {
        self.provider.next_missing(&self.cong)
    }

    pub fn escaping(&'s self) -> EscapePair<S, W> {
        self.get_info().escaping
    }

    /// Returns the induced pair for the current congruence
    pub fn induced(&'s self) -> InducedPair<'s, S, W> {
        self.get_info().induced
    }

    pub fn constraint_output(&self) -> Option<C::Output> {
        self.constraint.satisfied(&self.get_info()).ok()
    }

    /// Runs the algorithm to completion, executing [`step`] until it returns [`GlercSignal::Finished`].
    pub fn execute(&mut self) -> GlercOutput<S> {
        let mut iteration = 0;
        loop {
            iteration += 1;
            match self.step() {
                GlercSignal::Finished(out) => return out,
                signal => {
                    trace!("Iteration {iteration}, signal: {:?}", signal)
                }
            }
        }
    }
}

impl<'s, S, W, C, Acc> From<GlercState<'s, S, W, C, &'s Sample<W>>>
    for Combined<RightCongruence<S>, Acc>
where
    Acc: AcceptanceCondition,
    S: Symbol + 's,
    W: Subword<S = S> + Clone + 's + Hash,
    W: InitialRun<RightCongruence<S>, <W as Word>::Kind>,
    C: Constraint<S, <W as Run<RightCongruence<S>, <W as Word>::Kind>>::Induces, Output = Acc>,
{
    fn from(value: GlercState<'s, S, W, C, &'s Sample<W>>) -> Self {
        let mut gs = value;
        let result = gs.execute();
        let acceptance_condition = gs
            .constraint_output()
            .expect("Constraint must be satisfied when the algorithm terminates!");

        Combined::from_parts(
            result.learned_congruence,
            Class::epsilon(),
            acceptance_condition,
        )
    }
}

impl<'s, S, W, C, P> From<GlercState<'s, S, W, C, P>> for RightCongruence<S>
where
    S: Symbol + 's,
    W: Subword<S = S> + Clone + 's + Hash,
    W: InitialRun<RightCongruence<S>, <W as Word>::Kind>,
    C: Constraint<S, <W as Run<RightCongruence<S>, <W as Word>::Kind>>::Induces, Output = ()>,
    P: ProvidesMissing<S> + ProvidesGlercInfo<S, W>,
{
    fn from(value: GlercState<'s, S, W, C, P>) -> Self {
        let mut gs = value;
        let result = gs.execute();
        result.learned_congruence
    }
}

#[cfg(test)]
mod tests {
    use automata::Growable;

    use crate::glerc::constraint::EmptyConstraint;

    use super::*;
    use automata::RightCongruence;

    macro_rules! induced_assert_eq {
        ($state:expr, $input:expr) => {
            assert_eq!(
                (
                    $state.induced().0.iter().cloned().map(|(_, x)| x).collect(),
                    $state.induced().1.iter().cloned().map(|(_, y)| y).collect()
                ),
                $input
            );
        };
    }

    #[test]
    fn test_missings() {
        let sample = Sample::from_iters(["a", "ab"], ["", "b", "aa"]);
        let mut glercstate =
            GlercState::new(&sample, RightCongruence::empty_trivial(), EmptyConstraint);
        let eps = Class::epsilon();
        let a = Class::from('a');
        let mut expected_induced = (vec![], vec![eps.clone()]);

        assert_eq!(glercstate.next_missing(), Some((eps.clone(), 'a')));
        induced_assert_eq!(glercstate, expected_induced);

        glercstate.cong.add_state(&a);
        glercstate.cong.add_transition(&eps, 'a', &a);
        expected_induced.0.push(a.clone());
        assert_eq!(glercstate.next_missing(), Some((eps.clone(), 'b')));
        induced_assert_eq!(glercstate, expected_induced);

        glercstate.cong.add_transition(&eps, 'b', &eps);
        expected_induced.1.push(eps.clone());
        assert_eq!(glercstate.next_missing(), Some((a.clone(), 'a')));
        induced_assert_eq!(glercstate, expected_induced);

        glercstate.cong.add_transition(&a, 'a', &eps);
        expected_induced.1.push(eps.clone());
        assert_eq!(glercstate.next_missing(), Some((a.clone(), 'b')));
        induced_assert_eq!(glercstate, expected_induced);

        glercstate.cong.add_transition(&a, 'b', &a);
        expected_induced.0.push(a.clone());
        assert_eq!(glercstate.next_missing(), None);
        induced_assert_eq!(glercstate, expected_induced);
    }
}
