#[allow(dead_code)]
mod constraint;
#[allow(dead_code)]
mod state;

use automata::Symbol;

use automata::Class;

pub enum GlercOutput<S: Symbol> {
    /// Indicates that the transition (q,a) is missing
    MissingTransition(Class<S>, S),
    /// Failed to insert a transition
    FailedInsertion(Class<S>, S, Class<S>),
    /// Successfully inserted a transition
    SuccessfulInsertion(Class<S>, S, Class<S>),
    /// Indicates that a new state was created
    NewState(Class<S>, S, Class<S>),
    /// Indicates that the algorithm has finished
    Finished,
}

#[cfg(test)]
mod tests {}
