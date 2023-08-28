use crate::{
    alphabet::Symbol, word::RawSymbols, FiniteLength, HasLength, InfiniteLength, Length, Word,
};
use itertools::Itertools;

use super::OmegaWord;

#[macro_export]
macro_rules! nupw {
    ($recur:expr) => {
        $crate::word::Normalized::new_omega($recur, automata::InfiniteLength($recur.len(), 0))
    };
    ($base:expr, $recur:expr) => {
        $crate::word::Normalized::new_omega(
            $base.chars().chain($recur.chars()).collect_vec(),
            automata::InfiniteLength($base.len() + $recur.len(), $base.len()),
        )
    };
}
#[macro_export]
macro_rules! npw {
    ($recur:expr) => {
        $crate::word::NormalizedPeriodic::new($recur.chars().collect_vec())
    };
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Normalized<S: Symbol, L: Length> {
    pub word: Vec<S>,
    pub length: L,
}

impl<S: Symbol, L: Length> Normalized<S, L> {
    pub fn new(word: Vec<S>, length: L) -> Self {
        Self { word, length }
    }

    pub fn first(&self) -> Option<S> {
        self.word.first()
    }
}

impl<S: Symbol> Normalized<S, InfiniteLength> {
    pub fn is_periodic(&self) -> bool {
        self.length.loop_index() == 0
    }
}

impl<S: Symbol, L: Length> Word for Normalized<S, L>
where
    Normalized<S, L>: std::fmt::Debug,
{
    type Symbol = S;

    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        self.length()
            .calculate_raw_position(position)
            .and_then(|pos| self.word.get(pos.position()).cloned())
    }
}
impl<S: Symbol, L: Length> HasLength for Normalized<S, L> {
    type Length = L;

    fn length(&self) -> Self::Length {
        self.length
    }
}

impl From<(&str, usize)> for Normalized<char, InfiniteLength> {
    fn from(value: (&str, usize)) -> Self {
        let (word, loop_index) = value;
        let word = word.chars().collect_vec();
        let length = InfiniteLength(word.len(), loop_index);
        Self { word, length }
    }
}

impl<S: Symbol> Normalized<S, FiniteLength> {
    pub fn new_finite<I: IntoIterator<Item = S>>(word: I) -> Self {
        let word = word.into_iter().collect_vec();
        let length = FiniteLength(word.len());
        Self { word, length }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct NormalizedPeriodic<S: Symbol> {
    pub word: Vec<S>,
}

impl<S: Symbol> NormalizedPeriodic<S> {
    pub fn new(word: Vec<S>) -> Self {
        Self {
            word: deduplicate(word),
        }
    }
}

impl<S: Symbol> TryFrom<Normalized<S, InfiniteLength>> for NormalizedPeriodic<S> {
    type Error = ();

    fn try_from(value: Normalized<S, InfiniteLength>) -> Result<Self, Self::Error> {
        if value.is_periodic() {
            debug_assert!(
                deduplicate(value.word.clone()) == value.word,
                "word is no deduplicated"
            );
            Ok(Self::new(value.word))
        } else {
            Err(())
        }
    }
}

impl<S: Symbol> Word for NormalizedPeriodic<S> {
    type Symbol = S;

    fn nth(&self, position: usize) -> Option<Self::Symbol> {
        self.word.get(position % self.word.len()).cloned()
    }
}

impl<S: Symbol> HasLength for NormalizedPeriodic<S> {
    type Length = InfiniteLength;

    fn length(&self) -> Self::Length {
        InfiniteLength(self.word.len(), 0)
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

impl<S: Symbol> std::fmt::Debug for NormalizedPeriodic<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}⁰",
            self.word.iter().map(|s| format!("{:?}", s)).join("")
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::InfiniteLength;
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
