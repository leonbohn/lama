use std::{fmt::Debug, marker::PhantomData};

use impl_tools::autoimpl;
use itertools::Itertools;

use crate::{prelude::Symbol, Alphabet, Show};

use super::{FiniteWord, LinearWord};

pub trait OmegaWord<S>: LinearWord<S> {
    type Spoke<'this>: FiniteWord<S>
    where
        Self: 'this;
    type Cycle<'this>: FiniteWord<S>
    where
        Self: 'this;

    fn normalized(&self) -> Reduced<S>
    where
        S: Symbol,
    {
        Reduced::ultimately_periodic(self.spoke(), self.cycle())
    }

    fn spoke(&self) -> Self::Spoke<'_>;
    fn cycle(&self) -> Self::Cycle<'_>;

    fn loop_index(&self) -> usize {
        self.spoke().len()
    }
    fn cycle_length(&self) -> usize {
        self.cycle().len()
    }
}

impl<S: Symbol, W: OmegaWord<S>> OmegaWord<S> for &W {
    type Spoke<'this> = W::Spoke<'this>
    where
        Self: 'this;

    type Cycle<'this> = W::Cycle<'this>
    where
        Self: 'this;

    fn spoke(&self) -> Self::Spoke<'_> {
        W::spoke(self)
    }

    fn cycle(&self) -> Self::Cycle<'_> {
        W::cycle(self)
    }
}

fn deduplicate_inplace<S: Eq>(input: &mut Vec<S>) {
    assert!(!input.is_empty());

    for i in 1..=(input.len() / 2) {
        // for a word w of length n, if th first n-i symbols of w are equal to the
        // last n-i symbols of w, then w is periodic with period i
        if input.len() % i == 0 && input[..input.len() - i] == input[i..] {
            input.truncate(i);
            return;
        }
    }
}

fn deduplicate<S: Eq>(input: Vec<S>) -> Vec<S> {
    let mut input = input;
    deduplicate_inplace(&mut input);
    input
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Periodic<S> {
    representation: Vec<S>,
}

impl<S: Symbol> TryFrom<Reduced<S>> for Periodic<S> {
    type Error = ();
    fn try_from(value: Reduced<S>) -> Result<Self, Self::Error> {
        if value.loop_index() > 0 {
            Err(())
        } else {
            Ok(Self {
                representation: value.word,
            })
        }
    }
}

impl<S: Symbol> From<Periodic<S>> for Reduced<S> {
    fn from(value: Periodic<S>) -> Self {
        Self::periodic(value.representation)
    }
}
impl<S: Symbol> From<&Periodic<S>> for Reduced<S> {
    fn from(value: &Periodic<S>) -> Self {
        Self::periodic(value.representation.clone())
    }
}

impl<S: Symbol> From<&Reduced<S>> for Reduced<S> {
    fn from(value: &Reduced<S>) -> Self {
        Self::ultimately_periodic(value.spoke(), value.cycle())
    }
}

impl<S: Symbol> Periodic<S> {
    pub fn new<W: FiniteWord<S>>(word: W) -> Self {
        let mut representation = word.to_vec();
        deduplicate_inplace(&mut representation);
        Self { representation }
    }
}

impl<S: Symbol> LinearWord<S> for Periodic<S> {
    fn nth(&self, position: usize) -> Option<S> {
        todo!()
    }
}
impl<S: Symbol> OmegaWord<S> for Periodic<S> {
    fn loop_index(&self) -> usize {
        0
    }

    type Spoke<'this> = &'this [S] where Self:'this;

    type Cycle<'this> = &'this [S]
    where
        Self: 'this;

    fn spoke(&self) -> Self::Spoke<'_> {
        self.representation[..self.loop_index()].as_ref()
    }

    fn cycle(&self) -> Self::Cycle<'_> {
        self.representation[self.loop_index()..].as_ref()
    }
}

/// Represents a reduced omega word. For ultimately periodic words, this means we
/// try to roll the prefix part into the looping part and deduplicate the looping
/// part. For periodic words, we just deduplicate the looping part.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Reduced<S> {
    pub(crate) word: Vec<S>,
    pub(crate) loop_index: usize,
}

impl<S: Symbol> Show for Reduced<S> {
    fn show(&self) -> String {
        format!(
            "{},({})",
            self.word[0..self.loop_index]
                .iter()
                .map(|chr| chr.show())
                .join(""),
            self.word[self.loop_index..]
                .iter()
                .map(|chr| chr.show())
                .join("")
        )
    }
    fn show_collection<'a, I>(iter: I) -> String
    where
        Self: 'a,
        I: IntoIterator<Item = &'a Self>,
        I::IntoIter: DoubleEndedIterator,
    {
        unimplemented!()
    }
}

impl<S: Symbol> LinearWord<S> for Reduced<S> {
    fn nth(&self, position: usize) -> Option<S> {
        if position >= self.word.len() {
            let loop_position = (position - self.loop_index) % self.cycle_length();
            self.word.nth(self.loop_index + loop_position)
        } else {
            self.word.nth(position)
        }
    }
}
impl<S: Symbol> OmegaWord<S> for Reduced<S> {
    fn loop_index(&self) -> usize {
        self.loop_index
    }

    type Spoke<'this> = &'this [S] where Self:'this;

    type Cycle<'this> = &'this [S] where Self:'this;

    fn spoke(&self) -> Self::Spoke<'_> {
        self.word[..self.loop_index].as_ref()
    }

    fn cycle(&self) -> Self::Cycle<'_> {
        self.word[self.loop_index..].as_ref()
    }
}

impl<S: Symbol> Reduced<S> {
    pub fn periodic<W: FiniteWord<S>>(representation: W) -> Self {
        let representation = deduplicate(representation.to_vec());
        Self {
            word: representation,
            loop_index: 0,
        }
    }

    pub fn pop_front(&mut self) -> S {
        assert!(self.loop_index < self.word.len());
        if self.loop_index() > 0 {
            let mut it = self.word.iter().cloned();
            let out = it.next().expect("infinite word!");
            self.word = it.collect();
            self.loop_index -= 1;
            out
        } else {
            let out = *self.cycle().first().unwrap();
            self.word.rotate_left(1);
            out
        }
    }

    pub fn ultimately_periodic<Spoke: FiniteWord<S>, Cycle: FiniteWord<S>>(
        spoke: Spoke,
        cycle: Cycle,
    ) -> Self {
        assert!(!cycle.is_empty());

        // see how far we can roll the the loop index back.
        let roll_in = (0..spoke.len())
            .take_while(|i| spoke.nth_back(*i) == cycle.nth_back(*i % cycle.len()))
            .max()
            .map(|x| x + 1)
            .unwrap_or(0);

        let mut loop_representation = cycle.to_vec();
        loop_representation.rotate_right(roll_in % cycle.len());
        deduplicate_inplace(&mut loop_representation);

        let mut representation = spoke.to_vec();
        representation.truncate(spoke.len().saturating_sub(roll_in));
        let loop_index = representation.len();

        representation.extend(deduplicate(cycle.to_vec()));
        Self {
            word: representation,
            loop_index,
        }
    }

    /// Returns a reference to the underlying raw representation.
    pub fn raw_word(&self) -> &[S] {
        &self.word
    }

    /// Gives the first symbol of the word.
    pub fn first(&self) -> Option<S> {
        self.word.first()
    }
}

impl TryFrom<&str> for Reduced<char> {
    type Error = ReducedParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.split_once(',') {
            None => {
                if value.is_empty() {
                    Err(ReducedParseError::Empty)
                } else {
                    Ok(Self::periodic(value.trim()))
                }
            }
            Some((initial, repeating)) => {
                if repeating.is_empty() {
                    Err(ReducedParseError::EmptyLoop)
                } else {
                    if repeating.contains(',') {
                        return Err(ReducedParseError::TooManyCommas);
                    }
                    let initial = initial.trim();
                    let repeating = repeating.trim();
                    Ok(Self::ultimately_periodic(initial, repeating))
                }
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Epsilon();

impl<S: Symbol> LinearWord<S> for Epsilon {
    fn nth(&self, position: usize) -> Option<S> {
        None
    }
}

impl<S: Symbol> FiniteWord<S> for Epsilon {
    type Symbols<'this> = std::iter::Empty<S>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        std::iter::empty()
    }

    fn to_vec(&self) -> Vec<S> {
        vec![]
    }

    fn len(&self) -> usize {
        0
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Repeating<W>(pub W);

impl<S: Symbol, W: FiniteWord<S>> LinearWord<S> for Repeating<W> {
    fn nth(&self, position: usize) -> Option<S> {
        self.0.nth(position % self.0.len())
    }
}

impl<S: Symbol, W: FiniteWord<S>> OmegaWord<S> for Repeating<W> {
    type Spoke<'this> = Epsilon
    where
        Self: 'this;

    type Cycle<'this> = &'this W
    where
        Self: 'this;

    fn spoke(&self) -> Self::Spoke<'_> {
        Epsilon()
    }

    fn cycle(&self) -> Self::Cycle<'_> {
        &self.0
    }
}

/// Represents the types of errors that can occur when parsing a reduced word from a string.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ReducedParseError {
    /// The word is empty.
    Empty,
    /// The looping part of the word is empty.
    EmptyLoop,
    /// The word contains too many commas, when it should contain at most one.
    TooManyCommas,
}

impl std::fmt::Display for ReducedParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReducedParseError::Empty => write!(f, "Word is empty"),
            ReducedParseError::EmptyLoop => write!(f, "Looping part of word is empty"),
            ReducedParseError::TooManyCommas => write!(f, "Too many commas in the word"),
        }
    }
}

impl<S: Show> Debug for Reduced<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.loop_index == 0 {
            write!(f, "({})𝜔", self.word.iter().map(|sym| sym.show()).join(""))
        } else {
            write!(
                f,
                "{}, ({})𝜔",
                self.word[..self.loop_index]
                    .iter()
                    .map(|sym| sym.show())
                    .join(""),
                self.word[self.loop_index..]
                    .iter()
                    .map(|sym| sym.show())
                    .join("")
            )
        }
    }
}

impl<S: Show> Debug for Periodic<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({})𝜔",
            self.representation.iter().map(|sym| sym.show()).join("")
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        word::{omega::deduplicate, Reduced},
        InfiniteLength,
    };

    use super::deduplicate_inplace;

    #[test]
    fn parse_reduced() {
        let repr = "abab";
        let nupw = super::Reduced::try_from(repr).unwrap();
        let mut start = vec!['a', 'b', 'a', 'b'];
        deduplicate_inplace(&mut start);
        assert_eq!(start, vec!['a', 'b']);
        assert_eq!(nupw.word, vec!['a', 'b']);
    }

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
        let reduced = super::Reduced::ultimately_periodic("aaaaaaa", "aaaaaaaaa");
        assert_eq!(reduced.word, vec!['a']);
        assert_eq!(
            Reduced::ultimately_periodic("aaaaaaaaa", "a").word,
            vec!['a']
        );
    }
}
