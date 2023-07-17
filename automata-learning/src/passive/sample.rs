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
    Acceptor, Alphabet, Color, FiniteLength, HasLength, InfiniteLength, Length, Map, MooreMachine,
    RightCongruence, Set, Successor, Word, DFA,
};
use itertools::Itertools;
use tracing::trace;

pub use normalization::Normalized;
#[macro_use]
#[allow(missing_docs)]
mod normalization {
    use automata::{alphabet::Symbol, FiniteLength, HasLength, InfiniteLength, Length, Word};
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
                $recur.chars(),
                automata::InfiniteLength($recur.len(), 0),
            )
        };
        ($base:expr, $recur:expr) => {
            $crate::passive::sample::Normalized::new_omega(
                $base.chars().chain($recur.chars()),
                automata::InfiniteLength($base.len() + $recur.len(), $base.len()),
            )
        };
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

            let mut repr = word[..loop_index].to_vec();
            repr.extend(deduplicate(word[loop_index..].to_vec()));

            Self {
                length: InfiniteLength(word.len(), loop_index),
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

fn prefix_tree<A: Alphabet + HasUniverse>(
    alphabet: A,
    words: Vec<Normalized<A::Symbol, InfiniteLength>>,
) -> RightCongruence<A> {
    debug_assert!(words.iter().all(|word| !word.word.is_empty()));
    fn build_accepting_loop<A: Alphabet>(
        tree: &mut RightCongruence<A>,
        state: usize,
        access: Vec<A::Symbol>,
        loop_segment: &[A::Symbol],
    ) {
        let mut access = access;
        let mut current = state;
        for symbol in &loop_segment[..loop_segment.len() - 1] {
            access.push(*symbol);
            trace!("adding state {:?}", access);
            let next = tree.add_state(access.clone());
            tree.add_edge(current, A::expression(*symbol), next, ());
            current = next;
        }
        tree.add_edge(
            current,
            A::expression(loop_segment[loop_segment.len() - 1]),
            state,
            (),
        );
    }
    let mut tree = RightCongruence::new(alphabet.clone());
    let root = tree.initial();

    let sink = tree.add_state(vec![]);
    for sym in alphabet.universe() {
        tree.add_edge(sink, A::expression(*sym), sink, ());
    }

    let mut queue = VecDeque::from_iter([(root, vec![], words.to_vec())]);

    while let Some((state, access, words)) = queue.pop_front() {
        debug_assert!(!words.is_empty());
        debug_assert!(words.iter().all(|word| !word.word.is_empty()));
        if words.len() == 1 && words[0].length().loop_index() == 0 {
            build_accepting_loop(&mut tree, state, access, words[0].repeating_segment());
        } else {
            let mut map: Map<_, Set<_>> = Map::new();
            for mut word in words {
                let sym = word.pop_front();
                debug_assert!(
                    !word.word.is_empty(),
                    "popping front lead to empty representation"
                );

                map.entry(sym).or_default().insert(word);
            }

            for &sym in alphabet.universe() {
                if let Some(new_words) = map.remove(&sym) {
                    debug_assert!(!new_words.is_empty());
                    let new_access = access
                        .iter()
                        .cloned()
                        .chain(std::iter::once(sym))
                        .collect_vec();
                    trace!("Adding state {:?}", new_access);
                    let successor = tree.add_state(new_access.clone());
                    tree.add_edge(state, A::expression(sym), successor, ());
                    queue.push_back((successor, new_access, new_words.into_iter().collect()));
                } else {
                    tree.add_edge(state, A::expression(sym), sink, ());
                }
            }
        }
    }

    tree
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

    use super::{prefix_tree, Normalized};

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
        let cong = prefix_tree(Simple::from_iter("ab".chars()), words);
        info!(
            "Construction of congruence took {}μs",
            time_start.elapsed().as_micros()
        );

        assert_eq!(cong.size(), 19);

        for (access, mr) in [("aaaa", "aaa"), ("baaa", "ba"), ("bbbbbbbbbb", "bbb")] {
            let expected_state_name = mr.chars().collect_vec();
            assert_eq!(
                cong.induced((&access), cong.initial()),
                Some(ReachedColor(expected_state_name))
            );
        }

        let dfa = cong.map_colors(|_| true);
        for prf in ["aba", "ababbbbbb", "", "aa", "b", "bbabbab"] {
            assert!(dfa.accepts(&prf));
        }
    }
}
