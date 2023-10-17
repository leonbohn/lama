use automata::{
    congruence::FORC,
    prelude::{Indexes, MealyMachine, MooreMachine},
    Alphabet, Map, RightCongruence,
};
use itertools::Itertools;

use super::precise::PreciseDPA;

pub struct FWPM<'a, A: Alphabet> {
    leading: &'a RightCongruence<A>,
    pm: Map<usize, MooreMachine<A, usize>>,
}

impl<'a, A: Alphabet> FWPM<'a, A> {
    pub fn empty(leading: &'a RightCongruence<A>) -> Self {
        Self {
            leading,
            pm: Map::default(),
        }
    }

    pub fn leading(&self) -> &'a RightCongruence<A> {
        self.leading
    }

    pub fn insert_pm<I: Indexes<RightCongruence<A>>>(
        &mut self,
        index: I,
        pm: MooreMachine<A, usize>,
    ) {
        self.pm.insert(
            index
                .to_index(self.leading)
                .expect("Only valid indices can be used!"),
            pm,
        );
    }

    pub fn into_precise_dpa(self) -> PreciseDPA<A, { super::precise::PRECISE_DPA_COLORS }> {
        self.into()
    }

    /// Returns an iterator over the progress mealy machines, sorted by the index of the
    /// corresponding congruence class.
    pub fn pms(&self) -> impl Iterator<Item = (&MooreMachine<A, usize>, usize)> {
        self.pm
            .iter()
            .sorted_by(|x, y| x.0.cmp(y.0))
            .map(|(i, pm)| (pm, *i))
    }

    pub fn new(leading: &'a RightCongruence<A>, pm: Map<usize, MooreMachine<A, usize>>) -> Self {
        Self { leading, pm }
    }
}
