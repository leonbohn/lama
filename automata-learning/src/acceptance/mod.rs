/// Represents an error that can occur when trying to infer an acceptance condition.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AcceptanceError<'s, S, W> {
    /// Some miscellaneous problem occurred.
    Other(&'s S, &'s W),
    /// A problem when comping a Buchi condition was encountered.
    BuchiPositiveContained,
    /// Is returned when there was a problem with computing a reachability condition.
    ReachabilityProblem,
}
