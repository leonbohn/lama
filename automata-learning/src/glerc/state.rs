use std::{
    collections::{BTreeSet, VecDeque},
    hash::Hash,
};

use automata::{
    ts::IntoParts, AcceptanceCondition, Combined, Growable, Shrinkable, Subword, Symbol, Word,
};
use tracing::{debug, trace, warn};

use crate::passive::Sample;
use automata::{Class, RightCongruence};

use super::{constraint::Constraint, GlercOutput, GlercSignal};

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
pub struct GlercState<S: Symbol, C: Constraint<S>> {
    timer: Result<std::time::Duration, std::time::Instant>,
    /// The congruence constructed thus far
    pub(crate) cong: RightCongruence<S>,
    pub(crate) alphabet: BTreeSet<S>,
    queue: VecDeque<(Class<S>, S)>,
    /// The default congruence
    pub(crate) default: RightCongruence<S>,
    /// The current iteration
    pub(crate) iteration: usize,
    /// The current status
    status: GlercIterationStatus<S>,
    pub(crate) constraint: C,
}

impl<S, C> GlercState<S, C>
where
    S: Symbol,
    C: Constraint<S>,
{
    /// Creates a new state for the given sample and default congruence
    pub fn new<I: IntoIterator<Item = S>>(
        default: RightCongruence<S>,
        alphabet: I,
        constraint: C,
    ) -> Self {
        let cong = RightCongruence::empty_trivial();
        let alphabet = alphabet.into_iter().collect::<BTreeSet<_>>();
        let queue = alphabet
            .iter()
            .map(|a| (Class::epsilon(), a.clone()))
            .collect();

        Self {
            timer: Err(std::time::Instant::now()),
            cong,
            default,
            queue,
            alphabet,
            iteration: 0,
            status: GlercIterationStatus::FindMissing,
            constraint,
        }
    }

    fn get_status(&self) -> GlercIterationStatus<S> {
        self.status.clone()
    }

    /// Executes a single step of the GLERC algorithm, producing a [`GlercSignal`].
    pub fn step(&mut self) -> GlercSignal<S, C::Output> {
        self.iteration += 1;
        trace!("Starting iteration {}", self.iteration);
        let (new_status, result) = match &self.get_status() {
            GlercIterationStatus::TryInsertion(from, on, states, index) => {
                if let Some(target) = states.get(*index) {
                    debug!(
                        "Trying insertion of {} --{}--> {} at index {}, stateslist: [{}]",
                        from,
                        on,
                        target,
                        index,
                        states
                            .iter()
                            .map(|s| s.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                    self.cong.add_transition(from, on.clone(), target);
                    let res = self.constraint.satisfied(&self.cong);
                    if res.is_ok() {
                        trace!(
                            "Insertion successful!, leading to consistent TS\n{}",
                            self.cong
                        );
                        (
                            GlercIterationStatus::FindMissing,
                            GlercSignal::SuccessfulInsertion(
                                from.clone(),
                                on.clone(),
                                target.clone(),
                            ),
                        )
                    } else {
                        trace!(
                            "\tInsertion failed, constraint reported {:?}",
                            res.unwrap_err()
                        );
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
                    // First, we check whether insertion would lead to a TS that is larget than the default
                    if self.cong.0.size() >= self.default.0.size() {
                        warn!("Exceeded threshold, returning default");
                        self.cong = self.default.clone();
                        (
                            GlercIterationStatus::Finished,
                            GlercSignal::Finished(super::GlercOutput::new(
                                &self.cong,
                                self.timer.unwrap(),
                                self.constraint.satisfied(&self.cong).expect(
                                    "This must not fail, since we already checked it before.",
                                ),
                            )),
                        )
                    } else {
                        // We insert a new state since no state is left to try
                        let new_state = from + on;
                        self.cong.add_state(&new_state);
                        self.cong.add_transition(from, on.clone(), &new_state);

                        trace!(
                            "No fitting target found, inserted new state {} and extending queue.",
                            new_state
                        );
                        // also have to add the new state with all its symbols to the queue
                        self.extend_queue(&new_state);

                        (
                            GlercIterationStatus::FindMissing,
                            GlercSignal::NewState(from.clone(), on.clone(), new_state.clone()),
                        )
                    }
                }
            }
            GlercIterationStatus::FindMissing => {
                trace!("Finding missing transition target");
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
                            self.constraint
                                .satisfied(&self.cong)
                                .expect("This must not fail, since we already checked it before."),
                        )),
                    )
                }
            }
            GlercIterationStatus::Finished => {
                trace!("Finished!");
                (
                    GlercIterationStatus::Finished,
                    GlercSignal::Finished(GlercOutput::new(
                        &self.cong,
                        self.timer.unwrap(),
                        self.constraint
                            .satisfied(&self.cong)
                            .expect("This must not fail, since we already checked it before."),
                    )),
                )
            }
        };

        self.status = new_status;
        result
    }

    fn extend_queue(&mut self, state: &Class<S>) {
        for a in self.alphabet.iter() {
            self.queue.push_back((state.clone(), a.clone()));
        }
    }

    /// Returns the next missing transition, if any
    pub fn next_missing(&mut self) -> Option<(Class<S>, S)> {
        self.queue.pop_front()
    }

    /// Obtains the output of the constraint, if it is satisfied.
    pub fn constraint_output(&self) -> Option<C::Output> {
        self.constraint.satisfied(&self.cong).ok()
    }

    /// Runs the algorithm to completion, executing [`step`] until it returns [`GlercSignal::Finished`].
    pub fn execute(&mut self) -> GlercOutput<S, C::Output> {
        let mut iteration = 0;
        loop {
            match self.step() {
                GlercSignal::Finished(out) => return out,
                signal => {
                    trace!("Iteration {}, signal: {:?}", self.iteration, signal)
                }
            }
        }
    }
}

impl<S, C> From<GlercState<S, C>> for Combined<RightCongruence<S>, C::Output>
where
    S: Symbol,
    C: Constraint<S>,
{
    fn from(value: GlercState<S, C>) -> Self {
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

impl<S, C> From<GlercState<S, C>> for RightCongruence<S>
where
    S: Symbol,
    C: Constraint<S>,
{
    fn from(value: GlercState<S, C>) -> Self {
        let mut gs = value;
        let result = gs.execute();
        result.learned_congruence
    }
}

#[cfg(test)]
mod tests {}
