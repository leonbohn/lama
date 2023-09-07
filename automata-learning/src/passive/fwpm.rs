use automata::{Alphabet, MealyMachine, RightCongruence};

/// A family of weak priority mappings (FWPM) is a pair (C, M) where C is a
/// right congruence relation and for each class c of C, M_c is a Mealy machine.
/// Each mealy machine M_c is called a component of the FWPM and the mapping
/// it computes (on non-empty words) is weak in the sense that M_c(xy) <= M_c(x)
/// for all x and y.
#[derive(Debug, Clone)]
pub struct FWPM<A: Alphabet> {
    cong: RightCongruence<A>,
    mms: Vec<MealyMachine<A, usize>>,
}

impl<A: Alphabet> FWPM<A> {
    /// Creates a new FWPM from a right congruence and a sequence of Mealy machines.
    pub fn new(cong: RightCongruence<A>, mms: Vec<MealyMachine<A, usize>>) -> Self {
        Self { cong, mms }
    }
}
