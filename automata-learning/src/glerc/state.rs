use std::hash::Hash;

use automata::{
    run::{EscapePrefix, InitialRun, Run},
    Growable, Subword, Symbol, TransitionSystem, Word,
};
use itertools::Itertools;

use crate::sample::Sample;
use automata::{Class, RightCongruence};

use super::GlercOutput;

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
pub struct GlercState<'s, S: Symbol, W: Subword<S = S> + Run<RightCongruence<S>, <W as Word>::Kind>>
{
    /// The congruence constructed thus far
    pub(crate) cong: RightCongruence<S>,
    /// The default congruence
    pub(crate) default: RightCongruence<S>,
    /// The sample
    pub(crate) sample: &'s Sample<W>,
    /// The current iteration
    pub(crate) iteration: usize,
    /// The current status
    status: GlercIterationStatus<S>,
    induced: InducedPair<'s, S, W>,
    escaping: EscapePair<S, W>,
}

/// Alias for the result of evaluating a configuration
type EvalResult<'s, S, W> = <W as Run<RightCongruence<S>, <W as Word>::Kind>>::Induces;
/// Alias for the induced pair
type InducedPair<'s, S, W> = (Vec<EvalResult<'s, S, W>>, Vec<EvalResult<'s, S, W>>);
/// Alias for the escaping pair
type EscapePair<S, W> = (
    Vec<EscapePrefix<Class<S>, W>>,
    Vec<EscapePrefix<Class<S>, W>>,
);

#[derive(Clone, Debug)]
pub struct GlercInfo<'s, S: Symbol, W: Subword<S = S> + Run<RightCongruence<S>, <W as Word>::Kind>>
{
    pub(crate) cong: &'s RightCongruence<S>,
    pub(crate) induced: InducedPair<'s, S, W>,
    pub(crate) escaping: EscapePair<S, W>,
}

impl<'s, S, W> GlercState<'s, S, W>
where
    S: Symbol + 's,
    W: Subword<S = S> + Clone + 's + Hash,
    W: InitialRun<RightCongruence<S>, <W as Word>::Kind>,
{
    /// Creates a new state for the given sample and default congruence
    pub fn new(sample: &'s Sample<W>, default: RightCongruence<S>) -> Self {
        let cong = RightCongruence::empty_trivial();
        let mut out = Self {
            cong,
            default,
            sample,
            iteration: 0,
            status: GlercIterationStatus::FindMissing,
            induced: (Vec::new(), Vec::new()),
            escaping: (Vec::new(), Vec::new()),
        };
        out.update_info();
        out
    }

    fn update_info(&mut self) {
        let positive = self
            .sample
            .positive_iter()
            .map(|word| word.initial_run(&self.cong))
            .partition_result();
        let negative = self
            .sample
            .negative_iter()
            .map(|word| word.initial_run(&self.cong))
            .partition_result();
        self.induced = (positive.0, negative.0);
        self.escaping = (positive.1, negative.1);
    }

    fn get_info(&'s mut self) -> GlercInfo<'s, S, W> {
        self.update_info();
        GlercInfo {
            cong: &self.cong,
            induced: self.induced.clone(),
            escaping: self.escaping.clone(),
        }
    }

    pub fn step(&mut self) -> GlercOutput<S> {
        let (new_status, result) = match &self.status {
            GlercIterationStatus::TryInsertion(from, on, states, index) => {
                if let Some(target) = states.get(*index) {
                    let mut temporary = self.cong.temporary();
                    let previous_target = temporary.add_transition(from, on.clone(), target);
                    assert_eq!(previous_target, None, "Transition already exists");
                    todo!()
                } else {
                    let new_state = from + on;
                    self.cong.add_state(&new_state);
                    self.cong.add_transition(from, on.clone(), &new_state);
                    (
                        GlercIterationStatus::FindMissing,
                        GlercOutput::NewState(from.clone(), on.clone(), new_state),
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
                        GlercOutput::MissingTransition(q, a),
                    )
                } else {
                    (GlercIterationStatus::Finished, GlercOutput::Finished)
                }
            }
            GlercIterationStatus::Finished => {
                (GlercIterationStatus::Finished, GlercOutput::Finished)
            }
        };
        self.update_info();

        self.status = new_status;
        result
    }

    /// Returns the next missing transition, if any
    pub fn next_missing(&self) -> Option<(Class<S>, S)> {
        self.sample
            .iter()
            .filter_map(|word| word.initial_run(&self.cong).err())
            .min()
            .map(|ep| ep.trigger())
    }

    pub fn escaping(&self) -> EscapePair<S, W> {
        self.escaping.clone()
    }

    /// Returns the induced pair for the current congruence
    pub fn induced(&self) -> InducedPair<'s, S, W> {
        self.induced.clone()
    }
}

#[cfg(test)]
mod tests {
    use automata::Growable;

    use super::*;
    use automata::RightCongruence;

    #[test]
    fn test_missings() {
        let sample = Sample::from_parts(["a", "ab"], ["", "b", "aa"]);
        let mut glercstate = GlercState::new(&sample, RightCongruence::empty_trivial());
        let eps = Class::epsilon();
        let a = Class::from('a');
        let mut expected_induced = (vec![], vec![eps.clone()]);

        assert_eq!(glercstate.next_missing(), Some((eps.clone(), 'a')));
        assert_eq!(glercstate.induced(), expected_induced);

        glercstate.cong.add_state(&a);
        glercstate.cong.add_transition(&eps, 'a', &a);
        expected_induced.0.push(a.clone());
        glercstate.update_info();
        assert_eq!(glercstate.next_missing(), Some((eps.clone(), 'b')));
        assert_eq!(glercstate.induced(), expected_induced);

        glercstate.cong.add_transition(&eps, 'b', &eps);
        expected_induced.1.push(eps.clone());
        glercstate.update_info();
        assert_eq!(glercstate.next_missing(), Some((a.clone(), 'a')));
        assert_eq!(glercstate.induced(), expected_induced);

        glercstate.cong.add_transition(&a, 'a', &eps);
        expected_induced.1.push(eps.clone());
        glercstate.update_info();
        assert_eq!(glercstate.next_missing(), Some((a.clone(), 'b')));
        assert_eq!(glercstate.induced(), expected_induced);

        glercstate.cong.add_transition(&a, 'b', &a);
        expected_induced.0.push(a.clone());
        glercstate.update_info();
        assert_eq!(glercstate.next_missing(), None);
        assert_eq!(glercstate.induced(), expected_induced);
    }
}
