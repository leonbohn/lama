use std::{
    cell::RefCell,
    collections::{BTreeSet, VecDeque},
    fmt::Debug,
    hash::Hash,
};

use automata::{
    alphabet::{self, Symbol},
    ts::{FiniteState, HasStates, Pointed, Product, Sproutable},
    word::{Normalized, OmegaWord, RawSymbols},
    Acceptor, Alphabet, Class, Color, FiniteLength, HasLength, InfiniteLength, Length, Map,
    MooreMachine, RightCongruence, Set, Successor, Word, DFA,
};
use itertools::Itertools;
use tracing::trace;

use super::sprout::{omega_sprout_conflicts, prefix_consistency_conflicts};

/// Represents a finite sample, which is a pair of positive and negative instances.
#[derive(Clone, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Sample<A: Alphabet, L: Length, C: Color = bool> {
    pub alphabet: A,
    pub words: Map<Normalized<A::Symbol, L>, C>,
}

/// An `OmegaSample` is just a sample that contains infinite words.
pub type OmegaSample<A, C = bool> = Sample<A, InfiniteLength, C>;

impl<A: Alphabet, L: Length> Sample<A, L, bool> {
    /// Gives an iterator over all positive words in the sample.
    pub fn positive_words(&self) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words_with_color(true)
    }

    /// Gives an iterator over all negative words in the sample.
    pub fn negative_words(&self) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words_with_color(false)
    }
}

impl<A: Alphabet, L: Length, C: Color> Sample<A, L, C> {
    /// Gives an iterator over all words in the sample.
    pub fn words(&self) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words.keys()
    }

    /// Gives an iterator over all words in the sample with the associated color.
    pub fn words_with_color(
        &self,
        color: C,
    ) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words
            .iter()
            .filter_map(move |(w, c)| if *c == color { Some(w) } else { None })
    }
}

impl<A: Alphabet, C: Color> Sample<A, FiniteLength, C> {
    /// Create a new sample of finite words from the given alphabet and iterator over annotated words. The sample is given
    /// as an iterator over its symbols. The words are given as an iterator of pairs (word, color).
    pub fn new_finite<I: IntoIterator<Item = A::Symbol>, J: IntoIterator<Item = (I, C)>>(
        alphabet: A,
        words: J,
    ) -> Self {
        let words = words
            .into_iter()
            .map(|(word, color)| (Normalized::new_finite(word), color))
            .collect();
        Self { alphabet, words }
    }
}

impl<A: Alphabet, C: Color> OmegaSample<A, C> {
    /// Create a new sample of infinite words. The alphabet is given as something which implements [`RawSymbols`]. The words
    /// in the sample are given as an iterator yielding (word, color) pairs.
    pub fn new_omega<R: RawSymbols<A::Symbol>, J: IntoIterator<Item = (R, usize, C)>>(
        alphabet: A,
        words: J,
    ) -> Self {
        let words = words
            .into_iter()
            .map(|(word, loop_index, color)| {
                (
                    Normalized::new_omega(word, InfiniteLength(0, loop_index)),
                    color,
                )
            })
            .collect();
        Self { alphabet, words }
    }
}

impl<A: Alphabet> OmegaSample<A, bool> {
    /// Computes the [`RightCongruence`] underlying the sample.
    pub fn right_congruence(&self) -> RightCongruence<A> {
        omega_sprout_conflicts(
            self.alphabet.clone(),
            prefix_consistency_conflicts(&self.alphabet, self),
        )
    }
}

/// An [`OmegaSample`] restricted/split onto one [`Class`] of a [`RightCongruence`].
#[derive(Debug, Clone)]
pub struct ClassOmegaSample<'a, A: Alphabet, C: Color> {
    congruence: &'a RightCongruence<A>,
    class: Class<A::Symbol>,
    sample: Sample<A, InfiniteLength, C>,
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

    /// Creates an empty [`ClassOmegaSample`] from a [`RightCongruence`], a [`Class`] and an alphabet.
    pub fn empty(congruence: &'a RightCongruence<A>, class: Class<A::Symbol>, alphabet: A) -> Self {
        Self {
            congruence,
            class,
            sample: Sample {
                alphabet,
                words: Map::new(),
            },
        }
    }
}

impl<A: Alphabet, C: Color> OmegaSample<A, C> {
    /// Splits the sample into a map of [`ClassOmegaSample`]s, one for each class of the underlying [`RightCongruence`].
    pub fn split<'a>(
        &self,
        cong: &'a RightCongruence<A>,
    ) -> Map<usize, ClassOmegaSample<'a, A, C>> {
        debug_assert!(
            cong.size() > 0,
            "Makes only sense for non-empty congruences"
        );
        let initial = cong.initial();
        // take self as is for epsilon
        let mut out = Map::new();
        out.insert(
            initial,
            ClassOmegaSample::new(cong, Class::epsilon(), self.clone()),
        );
        let mut queue: VecDeque<_> = self
            .words
            .iter()
            .map(|(w, c)| (initial, w.normalized(), c))
            .collect();

        while let Some((state, word, color)) = queue.pop_front() {
            trace!("Processing word {:?} in state {}", word, state);
            let (sym, suffix) = word.pop_first();
            // unwrap okay because words are infinite
            if let Some(reached) = cong.successor_index(state, sym) {
                trace!("\tReached successor {reached}");
                if out
                    .entry(reached)
                    .or_insert_with(|| {
                        ClassOmegaSample::empty(
                            cong,
                            cong.state_color(reached),
                            self.alphabet.clone(),
                        )
                    })
                    .sample
                    .words
                    .insert(suffix.normalized(), color.clone())
                    .is_none()
                {
                    trace!("Added word {:?} to state {}", suffix, reached);
                    queue.push_back((reached, suffix.normalized(), color));
                }
            }
        }
        out
    }
}

impl<A, L, C> Debug for Sample<A, L, C>
where
    A: Alphabet + Debug,
    L: Length + Debug,
    C: Color,
    Normalized<A::Symbol, L>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Sample with alphabet {:?}", self.alphabet)?;
        for (word, color) in &self.words {
            write!(f, "\n\t{:?}\t{:?}", color, word)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use automata::{
        alphabet::Simple,
        normalized_upw, simple,
        ts::{finite::ReachedColor, Congruence, HasStates, Sproutable},
        upw, Acceptor, HasLength, Pointed, RightCongruence, Successor,
    };
    use itertools::Itertools;
    use tracing::info;
    use tracing_test::traced_test;

    use crate::Sample;

    use super::Normalized;

    #[test]
    #[traced_test]
    fn split_up_sample() {
        let alphabet = simple!('a', 'b');
        // represents congruence e ~ b ~ aa ~\~ a ~ ab
        let sample = Sample::new_omega(
            alphabet.clone(),
            vec![
                ("b", 0, true),
                ("abab", 3, true),
                ("abbab", 4, true),
                ("ab", 1, false),
                ("a", 0, false),
            ],
        );
        let cong = sample.right_congruence();
        println!("{:?}", cong);
        let split = sample.split(&cong);
        println!("{:?}", split.get(&0).unwrap());
        println!("{:?}", split.get(&1).unwrap());
    }

    #[test]
    fn omega_prefix_tree() {
        let mut w = normalized_upw!("aba", "b");
        let x = w.pop_front();

        let words = vec![
            normalized_upw!("aba", "b"),
            normalized_upw!("a"),
            normalized_upw!("ab"),
            normalized_upw!("bba"),
            normalized_upw!("b", "a"),
            normalized_upw!("b"),
            normalized_upw!("aa", "b"),
        ];

        let time_start = std::time::Instant::now();
        let cong = crate::prefixtree::prefix_tree(Simple::from_iter("ab".chars()), words);
        info!(
            "Construction of congruence took {}μs",
            time_start.elapsed().as_micros()
        );

        for (access, mr) in [("aaaa", "aaa"), ("baaa", "ba"), ("bbbbbbbbbb", "bbb")] {
            let expected_state_name = mr.chars().collect_vec().into();
            assert_eq!(
                cong.reached_color(&access),
                Some(ReachedColor(expected_state_name))
            );
        }

        let dfa = cong.map_colors(|_| true);
        for prf in ["aba", "ababbbbbb", "", "aa", "b", "bbabbab"] {
            assert!(dfa.accepts(&prf));
        }
    }
}
