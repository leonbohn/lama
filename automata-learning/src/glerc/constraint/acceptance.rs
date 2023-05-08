use automata::{
    congruence::CongruenceTrigger,
    run::{Evaluate, Induces},
    words::{IsFinite, WordKind},
    BuchiCondition, Class, RightCongruence, Run, Set, Str, Subword, Successor, Symbol,
    TriggerIterable, Word,
};
use itertools::Itertools;
use tracing::trace;

use crate::acceptance::AcceptanceError;

use super::{
    BuchiConstraint, Constraint, EscapeSeparabilityConstraint, InducedSeparabilityConstraint,
    RpniConstraint,
};
