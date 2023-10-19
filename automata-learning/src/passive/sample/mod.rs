use std::{
    cell::RefCell,
    collections::{BTreeSet, VecDeque},
    fmt::Debug,
    hash::Hash,
};

use automata::{prelude::*, Map};
use itertools::Itertools;
use tracing::{debug, trace};

use crate::passive::sprout::iteration_consistency_conflicts;

use super::sprout::{prefix_consistency_conflicts, sprout, SeparatesIdempotents};

mod split;
pub use split::{ClassOmegaSample, SplitOmegaSample};

mod omega;
pub use omega::{OmegaSample, OmegaSampleParseError, PeriodicOmegaSample};

mod canonic_coloring;

/// Represents a finite sample, which is a pair of positive and negative instances.
#[derive(Clone, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Sample<A: Alphabet, L: Length, C: Color = bool> {
    pub alphabet: A,
    pub words: Map<Normalized<A::Symbol, L>, C>,
}

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

    /// Classifying a word returns the color that is associated with it.
    pub fn classify<W: Into<Normalized<A::Symbol, L>>>(&self, word: W) -> Option<C> {
        let word = word.into();
        self.words.get(&word).cloned()
    }

    /// Checks whether a word is contained in the sample.
    pub fn contains(&self, word: &Normalized<A::Symbol, L>) -> bool {
        self.words.contains_key(word)
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

    /// Returns the maximum length of any finite word in the sample. Gives back `0` if no word exists in the sample.
    pub fn max_word_len(&self) -> usize {
        self.words().map(|w| w.length().0).max().unwrap_or(0)
    }
}

impl<A, L, C> Debug for Sample<A, L, C>
where
    A: Alphabet + Debug,
    L: Length + Debug,
    C: Color + Debug,
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

#[macro_export]
macro_rules! sample {
    ($alph:expr; pos $($pos:expr),+; neg $($neg:expr),+) => {
        $crate::passive::Sample::new_omega($alph, [$($pos),+].into_iter().map(|p| ($crate::passive::Normalized::try_from(p).unwrap(), true)).chain([$($neg),+].into_iter().map(|n| ($crate::passive::Normalized::try_from(n).unwrap(), false))).collect::<automata::Map<_, bool>>())
    };
}

#[cfg(test)]
mod tests {
    use automata::{npw, nupw, prelude::*, ts::finite::ReachedColor};
    use itertools::Itertools;
    use tracing::info;
    use tracing_test::traced_test;

    use crate::passive::Sample;

    use super::Normalized;

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
        assert_eq!(sample.classify(nupw!("ab")), Some(false));
    }

    #[test]
    fn to_periodic_sample() {
        let alphabet = alphabet!(simple 'a', 'b');
        // represents congruence e ~ b ~ aa ~\~ a ~ ab
        let sample = Sample::new_omega_from_pos_neg(
            alphabet,
            [nupw!("ab", "b"), nupw!("a", "b"), nupw!("bbbbbb")],
            [nupw!("aa")],
        );
        let periodic_sample = sample.to_periodic_sample();
        assert_eq!(periodic_sample.positive_size(), 1);
        assert_eq!(periodic_sample.negative_size(), 1);
        assert!(periodic_sample.contains(npw!("b")));
        assert!(periodic_sample.contains(npw!("a")));
        assert_eq!(periodic_sample.classify(npw!("bb")), Some(true));
    }

    #[test]
    #[traced_test]
    fn split_up_sample() {
        let alphabet = alphabet!(simple 'a', 'b');
        // represents congruence e ~ b ~ aa ~\~ a ~ ab
        let sample = Sample::new_omega(
            alphabet.clone(),
            vec![
                (("b", 0), true),
                (("abab", 3), true),
                (("abbab", 4), true),
                (("ab", 1), false),
                (("a", 0), false),
            ],
        );
        let cong = sample.infer_right_congruence();
        let split = sample.split(&cong);

        for w in ["b"] {
            assert!(split.get(0).unwrap().contains(&nupw!(w)))
        }

        println!("{:?}", split.get(0).unwrap());
        println!("{:?}", split.get(1).unwrap());
    }

    #[test]
    fn omega_prefix_tree() {
        let mut w = nupw!("aba", "b");
        let x = w.pop_front();

        let words = vec![
            nupw!("aba", "b"),
            nupw!("a"),
            nupw!("ab"),
            nupw!("bba"),
            nupw!("b", "a"),
            nupw!("b"),
            nupw!("aa", "b"),
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

        let dfa = cong.map_state_colors(|_| true).into_dfa();
        for prf in ["aba", "ababbbbbb", "", "aa", "b", "bbabbab"] {
            assert!(dfa.accepts(&prf));
        }
    }
}
