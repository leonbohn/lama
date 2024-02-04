use std::fmt::Debug;

use owo_colors::OwoColorize;

use crate::{
    ts::transition_system::Indexes, Alphabet, Class, Color, Map, RightCongruence, Show, Void,
};

/// A family of right congruences (FORC) consists of a *leading* right congruence and for each
/// class of this congruence a *progress* right congruence.
#[derive(Clone, PartialEq, Eq)]
pub struct FORC<A: Alphabet, Q = Void, C = Void> {
    pub(crate) leading: RightCongruence<A>,
    pub(crate) progress: Map<usize, RightCongruence<A, Q, C>>,
}

impl<A: Alphabet, Q: Clone, C: Clone> FORC<A, Q, C> {
    /// Creates a new FORC with the given leading congruence and progress congruences.
    pub fn new(
        leading: RightCongruence<A>,
        progress: Map<usize, RightCongruence<A, Q, C>>,
    ) -> Self {
        Self { leading, progress }
    }

    /// Returns a reference to the leading right congruence.
    pub fn leading(&self) -> &RightCongruence<A> {
        &self.leading
    }

    /// Insert a new progress congruence for the given class.
    pub fn insert<X>(&mut self, class: X, congruence: RightCongruence<A, Q, C>)
    where
        X: Indexes<RightCongruence<A>>,
    {
        let idx = class
            .to_index(self.leading())
            .expect("Cannot add prc for class that does not exist!");
        self.progress.insert(idx, congruence);
    }

    /// Tries to obtain a reference to the progress right congruence for the given `class`.
    pub fn prc<X>(&self, class: X) -> Option<&RightCongruence<A, Q, C>>
    where
        X: Indexes<RightCongruence<A>>,
    {
        let idx = class.to_index(self.leading())?;
        self.progress.get(&idx)
    }

    /// Returns an iterator over the progress congruences.
    pub fn prc_iter(&self) -> impl Iterator<Item = (&'_ usize, &'_ RightCongruence<A, Q, C>)> + '_ {
        self.progress.iter()
    }

    /// Creates a new FORC from the given leading congruence and progress congruences.
    pub fn from_iter<I: IntoIterator<Item = (usize, RightCongruence<A, Q, C>)>>(
        leading: RightCongruence<A>,
        progress: I,
    ) -> Self {
        Self {
            leading,
            progress: progress.into_iter().collect(),
        }
    }
}

impl<A: Alphabet, Q: Clone + Debug, C: Clone + Debug> std::fmt::Debug for FORC<A, Q, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{}\n{:?}", "LEADING".bold(), self.leading())?;
        // for (c, rc) in self.prc_iter() {
        //     let class_name = self.leading.class_name(*c).unwrap();
        //     write!(
        //         f,
        //         "{} \"{}\"\n{:?}",
        //         "PRC FOR CLASS ".bold(),
        //         &class_name,
        //         rc
        //     )?;
        // }
        todo!()
    }
}
