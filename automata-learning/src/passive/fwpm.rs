use automata::{
    congruence::FORC,
    prelude::{Indexes, MealyMachine, MooreMachine},
    Alphabet, Map, RightCongruence,
};
use itertools::Itertools;

use super::precise::PreciseDPA;

/// This structure represents a family of weak priority mappings (FWPM). It consists of a leading
/// congruence ~ and for each class of ~, a mapping that associates finite words with integers.
/// We asssume that these mappings are weak in the sense that for every word `w` and every prefix
/// `u` of `w`, the value assigned to `u` is greater or equal to the one that is assigned to `w`.
#[derive(Debug, Clone)]
pub struct FWPM<'a, A: Alphabet> {
    leading: &'a RightCongruence<A>,
    pm: Map<usize, MooreMachine<A, usize>>,
}

impl<'a, A: Alphabet> FWPM<'a, A> {
    /// Builds an empty [`FWPM`] with a given leading congruence and no mappings.
    pub fn empty(leading: &'a RightCongruence<A>) -> Self {
        Self {
            leading,
            pm: Map::default(),
        }
    }

    /// Returns a reference to the underlying right congruence.
    pub fn leading(&self) -> &'a RightCongruence<A> {
        self.leading
    }

    /// Inserts a mapping for some index. If a mapping was already present for this index, it is
    /// wrapped in a `Some` and returned. If no mapping was present, `None` is returned.
    pub fn insert_pm<I: Indexes<RightCongruence<A>>>(
        &mut self,
        index: I,
        pm: MooreMachine<A, usize>,
    ) -> Option<MooreMachine<A, usize>> {
        self.pm.insert(
            index
                .to_index(self.leading)
                .expect("Only valid indices can be used!"),
            pm,
        )
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
