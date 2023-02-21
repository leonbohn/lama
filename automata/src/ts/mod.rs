mod deterministic;
mod labels;

/// Implemented by objects which have a designated initial state.
pub trait Pointed<Id = u64> {
    /// Get the initial state of the automaton.
    fn initial(&self) -> &Id;
}
