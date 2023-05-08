use automata::{
    run::{EscapePrefix, Evaluate, Induces},
    words::WordKind,
    Class, Equivalent, RightCongruence, Run, Subword, Successor, Symbol, Word,
};
use itertools::Itertools;

use super::{Constraint, EscapeSeparabilityConstraint, InducedSeparabilityConstraint};
