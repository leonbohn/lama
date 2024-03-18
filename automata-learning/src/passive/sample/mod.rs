use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{BTreeSet, VecDeque},
    fmt::Debug,
    hash::Hash,
};

use automata::{prelude::*, word::LinearWord, Map};
use itertools::Itertools;
use tracing::{debug, trace};

use crate::{passive::sprout::iteration_consistency_conflicts, prefixtree::prefix_tree};

use super::sprout::{prefix_consistency_conflicts, sprout, SeparatesIdempotents};

mod split;
pub use split::{ClassOmegaSample, SplitOmegaSample};

mod omega;
pub use omega::{OmegaSampleParseError, PeriodicOmegaSample};

mod canonic_coloring;

mod characterize;

/// Represents a finite sample, which is a pair of positive and negative instances.
#[derive(Clone, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Sample<A: Alphabet, W: LinearWord<A::Symbol> + Hash, C: Color = bool> {
    pub alphabet: A,
    pub words: Map<W, C>,
}

/// Type alias for samples over the alphabet `A`, containing finite words which are classified with color `C`,
/// which defaults to `bool`.
pub type FiniteSample<A = CharAlphabet, C = bool> = Sample<A, Vec<<A as Alphabet>::Symbol>, C>;
/// Type alias for samples over alphabet `A` which contain infinite/omega words that are classified with `C`,
/// which defaults to `bool`.
pub type OmegaSample<A = CharAlphabet, C = bool> =
    Sample<A, ReducedOmegaWord<<A as Alphabet>::Symbol>, C>;

impl<A: Alphabet> OmegaSample<A> {
    pub fn prefix_tree(&self) -> RightCongruence<A> {
        prefix_tree(self.alphabet().clone(), self.words())
    }
}

impl<A: Alphabet, W: LinearWord<A::Symbol>> Sample<A, W, bool> {
    /// Gives an iterator over all positive words in the sample.
    pub fn positive_words(&self) -> impl Iterator<Item = &'_ W> + '_ {
        self.words_with_color(true)
    }

    /// Gives an iterator over all negative words in the sample.
    pub fn negative_words(&self) -> impl Iterator<Item = &'_ W> + '_ {
        self.words_with_color(false)
    }
}

impl<A: Alphabet, W: LinearWord<A::Symbol>, C: Color> Sample<A, W, C> {
    pub fn into_joined(self, other: Sample<A, W, C>) -> Sample<A, W, C> {
        let words = self.words.into_iter().chain(other.words).collect();
        Sample {
            alphabet: self.alphabet,
            words,
        }
    }

    pub fn append(&mut self, other: Sample<A, W, C>) {
        self.words.extend(other.words);
    }

    pub fn as_joined(&self, other: &Sample<A, W, C>) -> Sample<A, W, C>
    where
        W: Clone,
    {
        let words = self
            .words
            .iter()
            .chain(other.words.iter())
            .map(|(w, c)| (w.clone(), c.clone()))
            .collect();
        Sample {
            alphabet: self.alphabet.clone(),
            words,
        }
    }

    /// Returns a reference to the underlying alphabet.
    pub fn alphabet(&self) -> &A {
        &self.alphabet
    }

    /// Gives an iterator over all words in the sample.
    pub fn words(&self) -> impl Iterator<Item = &'_ W> + '_ {
        self.words.keys()
    }

    /// Returns an iterator over all pairs (w, c) of words w with their classification c that
    /// are present in the sample.
    pub fn entries(&self) -> impl Iterator<Item = (&'_ W, &'_ C)> + '_ {
        self.words.iter()
    }

    /// Classifying a word returns the color that is associated with it.
    pub fn classify<V>(&self, word: &V) -> Option<C>
    where
        V: Hash + Eq,
        W: Borrow<V>,
    {
        self.words.get(word).cloned()
    }

    /// Checks whether a word is contained in the sample.
    pub fn contains(&self, word: &W) -> bool {
        self.words.contains_key(word)
    }

    /// Gives an iterator over all words in the sample with the associated color.
    pub fn words_with_color(&self, color: C) -> impl Iterator<Item = &'_ W> + '_ {
        self.words
            .iter()
            .filter_map(move |(w, c)| if *c == color { Some(w) } else { None })
    }
}

impl<A: Alphabet, C: Color> FiniteSample<A, C> {
    /// Create a new sample of finite words from the given alphabet and iterator over annotated words. The sample is given
    /// as an iterator over its symbols. The words are given as an iterator of pairs (word, color).
    pub fn new_finite<I: IntoIterator<Item = A::Symbol>, J: IntoIterator<Item = (I, C)>>(
        alphabet: A,
        words: J,
    ) -> Self {
        let words = words
            .into_iter()
            .map(|(word, color)| (word.into_iter().collect(), color))
            .collect();
        Self { alphabet, words }
    }

    /// Returns the maximum length of any finite word in the sample. Gives back `0` if no word exists in the sample.
    pub fn max_word_len(&self) -> usize {
        self.words().map(|w| w.len()).max().unwrap_or(0)
    }
}

impl<A, W, C> Debug for Sample<A, W, C>
where
    A: Alphabet + Debug,
    W: LinearWord<A::Symbol> + Debug,
    C: Color + Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Sample with alphabet {:?} and {} words",
            self.alphabet,
            self.words.len()
        )?;
        for (word, color) in &self.words {
            write!(f, "\n\t{:?}\t{:?}", color, word)?;
        }
        Ok(())
    }
}

/// Macro for creating an alphabet. For now, this is limited to creating [`CharAlphabet`]s. Invocation is
/// done as `alphabet!(simple 'a', 'b', 'c')` to create such an alphabet with the symbols 'a', 'b' and 'c'.
#[macro_export]
macro_rules! sample {
    ($alph:expr; pos $($pos:expr),+; neg $($neg:expr),+) => {
        $crate::passive::Sample::new_omega($alph, [$($pos),+].into_iter().map(|p| ($crate::passive::ReducedOmegaWord::try_from(p).unwrap(), true)).chain([$($neg),+].into_iter().map(|n| ($crate::passive::ReducedOmegaWord::try_from(n).unwrap(), false))).collect::<automata::Map<_, bool>>())
    };
}

#[cfg(test)]
mod tests {
    use automata::{prelude::*, word::LinearWord};
    use itertools::Itertools;
    use tracing::info;

    use crate::passive::Sample;

    use super::ReducedOmegaWord;

    #[test]
    fn parse_sample() {
        let sample_str = r#"omega
        alphabet: a, b
        positive:
        a
        b,a
        aab
        baa
        negative:
        b
        ab
        abb"#;

        let sample = match Sample::try_from(sample_str) {
            Ok(s) => s,
            Err(e) => panic!("Error parsing sample: {:?}", e),
        };

        assert_eq!(sample.alphabet, alphabet!(simple 'a', 'b'));
        assert_eq!(sample.positive_size(), 4);
        assert_eq!(sample.negative_size(), 3);
        assert_eq!(sample.classify(&upw!("ab")), Some(false));
    }

    #[test]
    fn to_periodic_sample() {
        let alphabet = alphabet!(simple 'a', 'b');
        // represents congruence e ~ b ~ aa ~\~ a ~ ab
        let sample = Sample::new_omega_from_pos_neg(
            alphabet,
            [upw!("ab", "b"), upw!("a", "b"), upw!("bbbbbb")],
            [upw!("aa")],
        );
        let periodic_sample = sample.to_periodic_sample();
        assert_eq!(periodic_sample.positive_size(), 1);
        assert_eq!(periodic_sample.negative_size(), 1);
        assert!(periodic_sample.contains(PeriodicOmegaWord::new("b")));
        assert!(periodic_sample.contains(PeriodicOmegaWord::new("a")));
        assert_eq!(
            periodic_sample.classify(PeriodicOmegaWord::new("bb")),
            Some(true)
        );
    }

    #[test]
    fn split_up_sample() {
        let alphabet = alphabet!(simple 'a', 'b');
        // represents congruence e ~ b ~ aa ~\~ a ~ ab
        let sample = Sample::new_omega(
            alphabet.clone(),
            vec![
                (upw!("b"), true),
                (upw!("abab"), true),
                (upw!("abbab"), true),
                (upw!("ab"), false),
                (upw!("a"), false),
            ],
        );
        let cong = sample.infer_right_congruence();
        let split = sample.split(&cong);

        for w in ["b"] {
            assert!(split.get(0).unwrap().contains(&upw!(w)))
        }
    }

    #[test]
    #[ignore]
    fn omega_prefix_tree() {
        let mut w = upw!("aba", "b");
        let x = w.pop_first();

        let words = vec![
            upw!("aba", "b"),
            upw!("a"),
            upw!("ab"),
            upw!("bba"),
            upw!("b", "a"),
            upw!("b"),
            upw!("aa", "b"),
        ];

        let time_start = std::time::Instant::now();
        let cong = crate::prefixtree::prefix_tree(CharAlphabet::from_iter("ab".chars()), words);
        info!(
            "Construction of congruence took {}μs",
            time_start.elapsed().as_micros()
        );

        for (access, mr) in [("aaaa", "aaa"), ("baaa", "ba"), ("bbbbbbbbbb", "bbb")] {
            let expected_state_name = mr.chars().collect_vec().into();
            assert_eq!(cong.reached_state_color(access), Some(expected_state_name));
        }

        let dfa = cong.map_state_colors(|_| true).collect_dfa();
        for prf in ["aba", "ababbbbbb", "", "aa", "b", "bbabbab"] {
            assert!(dfa.accepts(prf));
        }
    }
}
