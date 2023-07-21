use std::{
    cell::RefCell,
    collections::{BTreeSet, VecDeque},
    fmt::Debug,
    hash::Hash,
};

use automata::{
    alphabet::{self, Symbol},
    ts::{FiniteState, HasStates, Pointed, Product, Sproutable},
    word::{OmegaWord, RawSymbols},
    Acceptor, Alphabet, Color, FiniteLength, HasLength, InfiniteLength, Length, Map, MooreMachine,
    RightCongruence, Set, Successor, Word, DFA,
};
use itertools::Itertools;
use tracing::trace;

pub use normalization::Normalized;
#[macro_use]
#[allow(missing_docs)]
mod normalization {
    use automata::{
        alphabet::Symbol, word::RawSymbols, FiniteLength, HasLength, InfiniteLength, Length, Word,
    };
    use itertools::Itertools;

    #[derive(Clone, Eq, PartialEq, Hash)]
    pub struct Normalized<S: Symbol, L: Length> {
        pub word: Vec<S>,
        pub length: L,
    }

    #[macro_export]
    macro_rules! normalized_upw {
        ($recur:expr) => {
            $crate::passive::sample::Normalized::new_omega(
                $recur,
                automata::InfiniteLength($recur.len(), 0),
            )
        };
        ($base:expr, $recur:expr) => {
            $crate::passive::sample::Normalized::new_omega(
                $base.chars().chain($recur.chars()).collect_vec(),
                automata::InfiniteLength($base.len() + $recur.len(), $base.len()),
            )
        };
    }

    impl<S: Symbol, L: Length> Word for Normalized<S, L>
    where
        Normalized<S, L>: std::fmt::Debug,
    {
        type Symbol = S;

        fn nth(&self, position: usize) -> Option<Self::Symbol> {
            self.word.get(position).copied()
        }
    }

    impl<S: Symbol, L: Length> HasLength for Normalized<S, L> {
        type Length = L;

        fn length(&self) -> Self::Length {
            self.length
        }
    }

    impl<S: Symbol, L: Length> Normalized<S, L> {
        pub fn first(&self) -> Option<S> {
            self.word.first()
        }
    }

    impl<S: Symbol> Normalized<S, FiniteLength> {
        pub fn new_finite<I: IntoIterator<Item = S>>(word: I) -> Self {
            let word = word.into_iter().collect_vec();
            let length = FiniteLength(word.len());
            Self { word, length }
        }
    }

    fn deduplicate<S: Symbol>(input: Vec<S>) -> Vec<S> {
        assert!(!input.is_empty());
        for i in (1..=(input.len() / 2)) {
            // for a word w of length n, if th first n-i symbols of w are equal to the
            // last n-i symbols of w, then w is periodic with period i
            if input.len() % i == 0 && input[..input.len() - i] == input[i..] {
                return input.into_iter().take(i).collect_vec();
            }
        }
        input.raw_to_vec()
    }

    impl<S: Symbol> Normalized<S, InfiniteLength> {
        pub fn new_omega<R: RawSymbols<S>>(raw: R, length: InfiniteLength) -> Self {
            // first we "roll in the periodic part" by moving the loop index to
            // the front and popping symbols from the back.
            let mut loop_index = length.loop_index();
            let mut raw = raw.to_vec();

            for i in (0..loop_index).rev() {
                if raw.get(i) == raw.last() {
                    raw.pop();
                    loop_index = i;
                } else {
                    break;
                }
            }

            let mut repr = raw[..loop_index].to_vec();
            repr.extend(deduplicate(raw[loop_index..].to_vec()));

            Self {
                length: InfiniteLength(raw.len(), loop_index),
                word: repr,
            }
        }

        pub fn initial_segment(&self) -> &[S] {
            &self.word[..self.length.loop_index()]
        }

        pub fn repeating_segment(&self) -> &[S] {
            &self.word[self.length.loop_index()..]
        }

        pub fn pop_front(&mut self) -> S {
            debug_assert!(!self.word.is_empty());
            if self.length.loop_index() > 0 {
                let out = self.word.remove(0);
                self.length.set_loop_index(self.length.loop_index() - 1);
                out
            } else {
                let out = self.word[0];
                self.word.rotate_left(1);
                out
            }
        }
    }

    impl<S: Symbol> std::fmt::Debug for Normalized<S, FiniteLength> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_fmt(format_args!(
                "{}",
                self.word.iter().map(|s| format!("{:?}", s)).join("")
            ))
        }
    }

    impl<S: Symbol> std::fmt::Debug for Normalized<S, InfiniteLength> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_fmt(format_args!(
                "{}({})⁰",
                self.initial_segment()
                    .iter()
                    .map(|s| format!("{:?}", s))
                    .join(""),
                self.repeating_segment()
                    .iter()
                    .map(|s| format!("{:?}", s))
                    .join("")
            ))
        }
    }

    #[cfg(test)]
    mod tests {
        use automata::InfiniteLength;
        use tracing::trace;

        use super::deduplicate;

        #[test]
        fn deduplication() {
            let input = vec![1, 2, 3, 1, 2, 3];
            assert_eq!(deduplicate(input), vec![1, 2, 3]);

            let input = vec![1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2];
            assert_eq!(deduplicate(input), vec![1, 2]);

            let input = vec![1, 2, 3, 1, 2, 3, 1, 2, 3];
            assert_eq!(deduplicate(input), vec![1, 2, 3]);
        }

        #[test]
        fn normalizing_upws() {
            let normalized = super::Normalized::new_omega("aaaaaaa", InfiniteLength(8, 3));
            assert_eq!(normalized.word, vec!['a']);
        }
    }
}

/// Represents a finite sample, which is a pair of positive and negative instances.
#[derive(Clone, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Sample<A: Alphabet, L: Length, C: Color = bool> {
    pub alphabet: A,
    pub words: Map<Normalized<A::Symbol, L>, C>,
}

pub type OmegaSample<A, C = bool> = Sample<A, InfiniteLength, C>;

impl<A: Alphabet, L: Length> Sample<A, L, bool> {
    pub fn positive_words(&self) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words_with_color(true)
    }
    pub fn negative_words(&self) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words_with_color(false)
    }
}

impl<A: Alphabet, L: Length, C: Color> Sample<A, L, C> {
    pub fn words(&self) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words.iter().map(|(w, _)| w)
    }

    pub fn words_with_color(
        &self,
        color: C,
    ) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words
            .iter()
            .filter_map(move |(w, c)| if *c == color { Some(w) } else { None })
    }
}

mod finite_words {
    use automata::{Alphabet, Color, FiniteLength};

    use crate::Sample;

    use super::Normalized;

    impl<A: Alphabet, C: Color> Sample<A, FiniteLength, C> {
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
}

impl<A: Alphabet, C: Color> OmegaSample<A, C> {
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

#[derive(Debug, Clone)]
pub struct SplitOmegaSample<'a, A: Alphabet, C: Color> {
    congruence: &'a RightCongruence<A>,
    sample: Sample<A, InfiniteLength, C>,
}

impl<'a, A: Alphabet, C: Color> SplitOmegaSample<'a, A, C> {
    pub fn new(congruence: &'a RightCongruence<A>, sample: Sample<A, InfiniteLength, C>) -> Self {
        Self { congruence, sample }
    }
    pub fn empty(congruence: &'a RightCongruence<A>, alphabet: A) -> Self {
        Self {
            congruence,
            sample: Sample {
                alphabet,
                words: Map::new(),
            },
        }
    }
}
impl<A: Alphabet, C: Color> OmegaSample<A, C> {
    pub fn split<'a>(
        &self,
        cong: &'a RightCongruence<A>,
    ) -> Map<usize, SplitOmegaSample<'a, A, C>> {
        debug_assert!(
            cong.size() > 0,
            "Makes only sense for non-empty congruences"
        );
        let initial = cong.initial();
        // take self as is for epsilon
        let mut out = Map::new();
        out.insert(initial, SplitOmegaSample::new(cong, self.clone()));
        let mut queue: VecDeque<_> = self
            .words
            .iter()
            .map(|(w, c)| (initial, w.offset(0), c))
            .collect();

        while let Some((state, word, color)) = queue.pop_front() {
            // unwrap okay because words are infinite
            if let Some(reached) = cong.successor_index(state, word.first().unwrap()) {
                let suffix = word.offset(1);
                out.entry(reached)
                    .or_insert_with(|| SplitOmegaSample::empty(cong, self.alphabet.clone()))
                    .sample
                    .words
                    .insert(
                        Normalized::new_omega(suffix.raw_to_vec(), suffix.length()),
                        color.clone(),
                    );
                todo!("Fix rawpresentation call above, that does not work. we need to somehow collect the word");
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
        ts::{finite::ReachedColor, Congruence, HasStates, Sproutable},
        upw, Acceptor, HasLength, Pointed, RightCongruence, Successor,
    };
    use itertools::Itertools;
    use tracing::info;
    use tracing_test::traced_test;

    use super::Normalized;

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

        assert_eq!(cong.hs_size(), 18);

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
