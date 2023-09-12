use crate::{Alphabet, Class, Color, Map, RightCongruence};

/// A family of right congruences (FORC) consists of a *leading* right congruence and for each
/// class of this congruence a *progress* right congruence.
#[derive(Clone, PartialEq, Eq)]
pub struct FORC<A: Alphabet, Q: Color = (), C: Color = ()> {
    pub(crate) leading: RightCongruence<A>,
    pub(crate) progress: Map<Class<A::Symbol>, RightCongruence<A, Q, C>>,
}

impl<A: Alphabet, Q: Color, C: Color> FORC<A, Q, C> {
    /// Creates a new FORC with the given leading congruence and progress congruences.
    pub fn new(
        leading: RightCongruence<A>,
        progress: Map<Class<A::Symbol>, RightCongruence<A, Q, C>>,
    ) -> Self {
        Self { leading, progress }
    }

    /// Insert a new progress congruence for the given class.
    pub fn insert(&mut self, class: Class<A::Symbol>, congruence: RightCongruence<A, Q, C>) {
        self.progress.insert(class, congruence);
    }

    /// Tries to obtain a reference to the progress right congruence for the given `class`.
    pub fn prc<D>(&self, class: D) -> Option<&RightCongruence<A, Q, C>>
    where
        D: std::borrow::Borrow<Class<A::Symbol>>,
    {
        self.progress.get(class.borrow())
    }

    /// Creates a new FORC from the given leading congruence and progress congruences.
    pub fn from_iter<I: IntoIterator<Item = (Class<A::Symbol>, RightCongruence<A, Q, C>)>>(
        leading: RightCongruence<A>,
        progress: I,
    ) -> Self {
        Self {
            leading,
            progress: progress.into_iter().collect(),
        }
    }
}
