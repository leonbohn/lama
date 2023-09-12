use crate::{alphabet::Simple, Alphabet, MealyMachine, RightCongruence};

/// A priority mapping is essentially a [`crate::MealyMachine`], i.e. it reads
/// finite words and ouptuts a priority (which in this case is a `usize`).
pub type PriorityMapping<A = Simple> = RightCongruence<A, (), usize>;

#[derive(Debug, Clone, Copy)]
struct Annotation {
    pub(super) idempotent: Option<bool>,
    pub(super) good: Option<bool>,
}

pub struct AnnotatedCongruence<A: Alphabet = Simple>(RightCongruence<A, Annotation>);

/// A family of weak priority mappings (FWPM) is a pair (C, M) where C is a
/// right congruence relation and for each class c of C, M_c is a Mealy machine.
/// Each mealy machine M_c is called a component of the FWPM and the mapping
/// it computes (on non-empty words) is weak in the sense that M_c(xy) <= M_c(x)
/// for all x and y.
#[derive(Debug, Clone)]
pub struct FWPM<A: Alphabet = Simple> {
    cong: RightCongruence<A>,
    pms: Vec<PriorityMapping<A>>,
}

impl<A: Alphabet> FWPM<A> {}
