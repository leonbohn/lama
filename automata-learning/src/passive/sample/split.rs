use automata::{
    ts::transition_system::Indexes, Alphabet, Class, Color, InfiniteLength, Map, RightCongruence,
};

use super::{OmegaSample, Sample};

/// An [`OmegaSample`] restricted/split onto one [`Class`] of a [`RightCongruence`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassOmegaSample<'a, A: Alphabet, C: Color> {
    congruence: &'a RightCongruence<A>,
    class: Class<A::Symbol>,
    sample: Sample<A, InfiniteLength, C>,
}

impl<'a, A: Alphabet, C: Color> std::ops::Deref for ClassOmegaSample<'a, A, C> {
    type Target = Sample<A, InfiniteLength, C>;

    fn deref(&self) -> &Self::Target {
        &self.sample
    }
}

impl<'a, A: Alphabet, C: Color> std::ops::DerefMut for ClassOmegaSample<'a, A, C> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.sample
    }
}

impl<'a, A: Alphabet, C: Color> ClassOmegaSample<'a, A, C> {
    /// Creates a new [`ClassOmegaSample`] from a [`RightCongruence`], a [`Class`] and a [`Sample`].
    pub fn new(
        congruence: &'a RightCongruence<A>,
        class: Class<A::Symbol>,
        sample: Sample<A, InfiniteLength, C>,
    ) -> Self {
        Self {
            congruence,
            class,
            sample,
        }
    }

    /// Returns a reference to the underlying sample.
    pub fn sample(&self) -> &OmegaSample<A, C> {
        &self.sample
    }

    /// Gives a mutable reference to the underlying sample.
    pub fn sample_mut(&mut self) -> &mut OmegaSample<A, C> {
        &mut self.sample
    }

    /// Creates an empty [`ClassOmegaSample`] from a [`RightCongruence`], a [`Class`] and an alphabet.
    pub fn empty(congruence: &'a RightCongruence<A>, class: Class<A::Symbol>, alphabet: A) -> Self {
        Self {
            congruence,
            class,
            sample: Sample {
                alphabet,
                words: Map::default(),
            },
        }
    }
}

/// Represents a right congruence relation together with a collection of split samples, one
/// associated with each class of the congruence.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SplitOmegaSample<'a, A: Alphabet, C: Color> {
    congruence: &'a RightCongruence<A>,
    split: Map<usize, ClassOmegaSample<'a, A, C>>,
}

impl<'a, A: Alphabet, C: Color> SplitOmegaSample<'a, A, C> {
    /// Creates a new object from the given congruence and the split
    pub fn new(
        congruence: &'a RightCongruence<A>,
        split: Map<usize, ClassOmegaSample<'a, A, C>>,
    ) -> Self {
        Self { congruence, split }
    }

    /// Obtain a reference to the split sample for the given class/index.
    pub fn get<I: Indexes<RightCongruence<A>>>(
        &self,
        index: I,
    ) -> Option<&ClassOmegaSample<'a, A, C>> {
        index
            .to_index(self.congruence)
            .and_then(|idx| self.split.get(&idx))
    }

    /// Obtains an iterator over all classes in the split sample.
    pub fn classes(&self) -> impl Iterator<Item = &'_ Class<A::Symbol>> + '_ {
        self.split.values().map(|sample| &sample.class)
    }

    /// Returns a reference to the underlying congruence.
    pub fn cong(&self) -> &'a RightCongruence<A> {
        self.congruence
    }
}
