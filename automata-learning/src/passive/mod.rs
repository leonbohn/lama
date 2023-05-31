mod sample;
use automata::{ts::Trivial, Class, RightCongruence, Symbol, Word, DBA, DFA, DPA};
pub use sample::*;
use tracing::trace;

use crate::glerc::{GlercState, ReachabilityConstraint};

/// Executes the RPNI algorithm on the given sample. This returns a DFA that is
/// composed of a right congruence as well as an acceptance condition, which marks
/// a classes as accepting if it is reached by a positive sample word.
pub fn dfa_rpni<S: Symbol>(sample: &FiniteSample<S>) -> DFA<Class<S>, S> {
    let constraint = ReachabilityConstraint::new(sample);

    let default_time_start = std::time::Instant::now();
    let default_structure = sample.default_structure();
    trace!(
        "Computed default structure in {}ms",
        default_time_start.elapsed().as_millis()
    );

    let executed =
        GlercState::new(default_structure, sample.alphabet.clone(), constraint).execute();

    trace!(
        "Execution finished, took {}ms",
        executed.execution_time_ms()
    );

    let (congruence, constraint_produced) = executed.learned();
    DFA::from_parts(
        congruence.extract_ts(),
        Class::epsilon(),
        constraint_produced,
    )
}

/// Executes a variant of the RPNI algorithm for omega-words, producing a DBA.
pub fn dba_rpni<S: Symbol>(sample: &OmegaSample<S>) -> DBA<S> {
    todo!()
}

/// Similar to [`dba_rpni`], but produces a DPA instead.
pub fn dpa_rpni<S: Symbol>(sample: &OmegaSample<S>) -> DPA<S> {
    todo!()
}
