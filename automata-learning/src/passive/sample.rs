use std::{cell::RefCell, collections::BTreeSet, hash::Hash};

use automata::{
    alphabet::Symbol, ts::HasStates, word::RawWithLength, Acceptor, Alphabet, Color, FiniteLength,
    InfiniteLength, Length, Map, Word, DFA,
};
use itertools::Itertools;
use tracing::trace;

#[derive(Clone, Eq, PartialEq)]
pub struct Normalized<S: Symbol, L: Length> {
    pub word: Vec<S>,
    pub length: L,
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
    pub fn new_infinite<I: IntoIterator<Item = S>>(word: I, length: InfiniteLength) -> Self {
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
}

impl<S: Symbol> std::fmt::Debug for Normalized<S, FiniteLength> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "\"{}\"",
            self.word.iter().map(|s| format!("{:?}", s)).join("")
        ))
    }
}

impl<S: Symbol> std::fmt::Debug for Normalized<S, InfiniteLength> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "\"{}\"",
            self.word.iter().map(|s| format!("{:?}", s)).join("")
        ))
    }
}

/// Represents a finite sample, which is a pair of positive and negative instances.
#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Sample<A: Alphabet, L: Length, C: Color = bool> {
    pub alphabet: A,
    pub words: Map<RawWithLength<A::Symbol, L>, C>,
}

#[cfg(test)]
mod tests {
    use automata::InfiniteLength;
    use tracing::trace;

    use crate::passive::sample::deduplicate;

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
        let normalized = super::Normalized::new_infinite(input, InfiniteLength(8, 3));
        assert_eq!(normalized.word, vec![1]);
    }
}
