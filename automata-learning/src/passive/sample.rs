use std::{
    cell::RefCell,
    collections::{BTreeSet, VecDeque},
    fmt::Debug,
    hash::Hash,
};

use automata::{
    alphabet::{self, HasUniverse, Symbol},
    ts::{HasStates, Pointed, Product, Sproutable},
    word::RawWithLength,
    Acceptor, Alphabet, Color, FiniteLength, InfiniteLength, Length, Map, MooreMachine,
    RightCongruence, Set, Successor, Word, DFA,
};
use itertools::Itertools;
use tracing::trace;

pub use normalization::Normalized;
#[allow(missing_docs)]
mod normalization {
    use automata::{alphabet::Symbol, FiniteLength, HasLength, InfiniteLength, Length, Word};
    use itertools::Itertools;

    #[derive(Clone, Eq, PartialEq, Hash)]
    pub struct Normalized<S: Symbol, L: Length> {
        pub word: Vec<S>,
        pub length: L,
    }

    impl<S: Symbol, L: Length> Word for Normalized<S, L> {
        type Raw = Vec<S>;

        type Symbol = S;

        fn nth(&self, position: usize) -> Option<Self::Symbol> {
            self.word.get(position).copied()
        }

        fn rawpresentation(&self) -> &Self::Raw {
            &self.word
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
            self.word.first().copied()
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
        input.to_vec()
    }

    impl<S: Symbol> Normalized<S, InfiniteLength> {
        pub fn new_omega<I: IntoIterator<Item = S>>(word: I, length: InfiniteLength) -> Self {
            // first we "roll in the periodic part" by moving the loop index to
            // the front and popping symbols from the back.
            let mut word = word.into_iter().collect_vec();
            let mut loop_index = length.loop_index();

            for i in (0..loop_index).rev() {
                if &word[i] == word.last().unwrap() {
                    word.pop();
                    loop_index = i;
                } else {
                    break;
                }
            }

            Self {
                length: InfiniteLength(word.len(), loop_index),
                word: deduplicate(word),
            }
        }

        pub fn initial_segment(&self) -> &[S] {
            &self.word[..self.length.loop_index()]
        }

        pub fn repeating_segment(&self) -> &[S] {
            &self.word[self.length.loop_index()..]
        }

        pub fn pop_front(&mut self) -> S {
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
            let input = [1, 1, 1, 1, 1, 1, 1, 1];
            let normalized = super::Normalized::new_omega(input, InfiniteLength(8, 3));
            assert_eq!(normalized.word, vec![1]);
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

impl<A: Alphabet, L: Length, C: Color> Sample<A, L, C> {
    pub fn words(&self) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words.iter().map(|(w, _)| w)
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

impl<A: Alphabet, C: Color> Sample<A, InfiniteLength, C> {
    pub fn new_omega<I: IntoIterator<Item = A::Symbol>, J: IntoIterator<Item = (I, usize, C)>>(
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

pub struct ConflictRelation<A: Alphabet> {
    dfas: [MooreMachine<A, usize>; 2],
    conflicts: Vec<(usize, usize)>,
}

fn conflict_relation_consistent<A: Alphabet + HasUniverse>(
    cong: RightCongruence<A>,
    c: ConflictRelation<A>,
) -> bool {
    let left = (&cong).product(&c.dfas[0]);
    let right = (&cong).product(&c.dfas[1]);

    if left.reachable_state_indices().any(|left_index| {
        right
            .reachable_state_indices()
            .any(|right_index| c.conflicts.contains(&(left_index.1, right_index.1)))
    }) {
        return false;
    }
    true
}

fn prefix_tree<A: Alphabet + FromIterator<A::Symbol>>(
    words: Vec<Normalized<A::Symbol, InfiniteLength>>,
) -> RightCongruence<A> {
    fn build_accepting_loop<A: Alphabet>(
        tree: &mut RightCongruence<A>,
        state: usize,
        access: Vec<A::Symbol>,
        loop_segment: &[A::Symbol],
    ) {
        let mut current = state;
        for symbol in loop_segment {
            let mut new_access = access.clone();
            new_access.push(*symbol);
            let next = tree.add_state(new_access);
            tree.add_edge(current, A::expression(*symbol), next, ());
            current = next;
        }
        tree.add_edge(current, A::expression(loop_segment[0]), state, ());
    }
    let alphabet = A::from_iter(words.iter().flat_map(|w| w.word.clone()).unique());
    let mut tree = RightCongruence::new(alphabet);
    let root = tree.initial();
    let mut queue = VecDeque::from_iter([(root, vec![], words.to_vec())]);

    while let Some((state, access, words)) = queue.pop_front() {
        debug_assert!(!words.is_empty());
        if words.len() == 1 {
            build_accepting_loop(&mut tree, state, access, words[0].repeating_segment());
        } else {
            let mut map: Map<_, Set<_>> = Map::new();
            for mut word in words {
                let sym = word.pop_front();
                map.entry(sym).or_default().insert(word);
            }

            for (sym, new_words) in map {
                debug_assert!(!new_words.is_empty());
                let new_access = access
                    .iter()
                    .cloned()
                    .chain(std::iter::once(sym))
                    .collect_vec();
                let successor = tree.add_state(new_access.clone());
                tree.add_edge(state, A::expression(sym), successor, ());
                queue.push_back((successor, new_access, new_words.into_iter().collect()));
            }
        }
    }

    tree
}

#[cfg(test)]
mod tests {
    use automata::{alphabet::Simple, upw, HasLength, RightCongruence};
    use itertools::Itertools;

    use super::{prefix_tree, Normalized};

    #[test]
    fn omega_prefix_tree() {
        let words = vec![upw!("ab", "c"), upw!("a"), upw!("b"), upw!("ab")];

        let cong: RightCongruence<Simple> = prefix_tree(
            words
                .into_iter()
                .map(|word| {
                    let length = word.length();
                    Normalized::new_omega(word.raw_as_vec(), length)
                })
                .collect_vec(),
        );
        println!("{:?}", cong);
    }
}
