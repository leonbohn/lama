use std::{fmt::Debug, marker::PhantomData};

use impl_tools::autoimpl;
use itertools::Itertools;

use crate::{prelude::Symbol, ts::Deterministic, Alphabet, Show};

use super::{FiniteWord, LinearWord};

/// An omega word is an infinite word that can be indexed by a `usize`. We assume that all
/// omega words can be represented as a concatenation of a finite prefix which we call spoke
/// and a finite loop which we call cycle. The loop index is the length of the spoke.
///
/// # Normalization
/// Omega words can be brought into a normalized form, where the prefix/spoke is rolled into
/// the loop as far as possible. Specifically, this means that the word `w = abca(caca)^𝜔` can be
/// transformed into the equivalent omega word `ab(caca)^𝜔`. Subsequently, we normalize the
/// looping/cycling part by attempting to find a factor such that the loop is some repetition
/// of the factor. In this instance, that would mean to normalize `w` into `ab(ca)^𝜔`.
///
/// Normalization is always possible and leads to a minimal representation of an omega word.
/// This representation is unique and can be computed in polynomial time.
///
/// # Equality testing
/// Most implementors of `OmegaWord` should also implement `PartialEq`/`Eq`. Note, that this
/// represents syntactical/structural equality. To test for syntactic equality (i.e. equality
/// of the represented word itself), the method [`OmegaWord::equals()`] should be used. This
/// method first normalizes the words in question and subsequently compares the loop and spoke.
///
/// # Example
/// ```
/// use automata::prelude::*;
/// let word = upw!("abc", "def"); // represents abc(def)^𝜔 = abcdefdefdef...
/// assert_eq!(word.loop_index(), 3);
/// assert_eq!(word.cycle_length(), 3);
/// ```
pub trait OmegaWord<S>: LinearWord<S> {
    /// The type of finite word representing the spoke, i.e. the finite prefix of the word
    /// before the loop index.  
    type Spoke<'this>: FiniteWord<S>
    where
        Self: 'this;
    /// The type of finite word that represents the cycle of th omega word, i.e. the
    /// finite loop that is repeated infinitely often.
    type Cycle<'this>: FiniteWord<S>
    where
        Self: 'this;

    /// Returns a normalized ultimately periodic word that is equal to `self`. This is done by
    /// first folding the prefix into the loop as far as possible and then deduplicating the
    /// loop.
    ///
    /// The representation returned by this method is unique and minimal for `self`. It is computed
    /// in polynomial time.
    ///
    /// # Example
    /// ```
    /// use automata::prelude::*;
    /// let word = upw!("abac", "acac");
    /// assert!(word.spoke().equals("ab"));
    /// assert!(word.cycle().equals("ac"));
    /// ```
    fn normalized(&self) -> ReducedOmegaWord<S>
    where
        S: Symbol,
    {
        ReducedOmegaWord::ultimately_periodic(self.spoke(), self.cycle())
    }

    /// Tests whether `self` is *semantically* equal to `other`. To see the difference compared to syntactic
    /// equality, consider the exampe below.
    /// ```
    /// use automata::prelude::*;
    /// let word = upw!("a"); // represents the periodic omega word a^𝜔 = aaa...
    /// let offset1 = word.offset(1); // the word obtained by skipping the first symbol of `word`
    /// let offset2 = word.offset(2);
    ///
    /// assert!(offset1 != offset2); // two different offsets are syntactically distinct
    /// assert!(offset1.equals(offset2)); // but they are semantically equal
    /// ```
    fn equals<W: OmegaWord<S>>(&self, other: W) -> bool
    where
        S: Symbol,
    {
        self.normalized() == other.normalized()
    }

    /// Returns the spoke of the word, i.e. the finite prefix of the word before the loop index.
    fn spoke(&self) -> Self::Spoke<'_>;
    /// Returns the cycle of the word, i.e. the finite loop that is repeated infinitely often.
    fn cycle(&self) -> Self::Cycle<'_>;

    /// Returns the loop index of the word, i.e. the length of the spoke. This can be zero if
    /// the word is periodic.
    fn loop_index(&self) -> usize {
        self.spoke().len()
    }

    /// Gives the length of the cycle of the word, i.e. the length of the loop that is repeated
    /// infinitely often. Note that if the word is not normalized, then there might be a shorter
    /// representation of the cycle that is repeated, e.g. a cycle of `aaa` could also be
    /// represented by the cycle `a`.
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

/// A periodic omega word has no finite prefix and consists only of a finite loop that is
/// repeated infinitely often. Note, that the loop cannot be empty.
/// TODO: make non-empty word into a new type
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct PeriodicOmegaWord<S> {
    representation: Vec<S>,
}

impl<S: Symbol> TryFrom<ReducedOmegaWord<S>> for PeriodicOmegaWord<S> {
    type Error = ();
    fn try_from(value: ReducedOmegaWord<S>) -> Result<Self, Self::Error> {
        if value.loop_index() > 0 {
            Err(())
        } else {
            Ok(Self {
                representation: value.word,
            })
        }
    }
}

impl<S: Symbol> From<PeriodicOmegaWord<S>> for ReducedOmegaWord<S> {
    fn from(value: PeriodicOmegaWord<S>) -> Self {
        Self::periodic(value.representation)
    }
}
impl<S: Symbol> From<&PeriodicOmegaWord<S>> for ReducedOmegaWord<S> {
    fn from(value: &PeriodicOmegaWord<S>) -> Self {
        Self::periodic(value.representation.clone())
    }
}

impl<S: Symbol> From<&ReducedOmegaWord<S>> for ReducedOmegaWord<S> {
    fn from(value: &ReducedOmegaWord<S>) -> Self {
        Self::ultimately_periodic(value.spoke(), value.cycle())
    }
}

impl<S: Symbol> PeriodicOmegaWord<S> {
    /// Creates a new periodic omega word from a finite word. The word must not be empty and
    /// it is deduplicated.
    ///
    /// # Example
    /// ```
    /// use automata::prelude::*;
    /// let word = upw!("abcabcabc");
    /// assert_eq!(word.cycle_length(), 3);
    /// assert_eq!(word.loop_index(), 0);
    /// assert!(word.spoke().is_empty());
    /// assert!(word.cycle().equals("abc"));
    /// ```
    pub fn new<W: FiniteWord<S>>(word: W) -> Self {
        let mut representation = word.to_vec();
        deduplicate_inplace(&mut representation);
        Self { representation }
    }

    /// Gives a reference to the underlying representation of the word.
    pub fn representation(&self) -> &[S] {
        &self.representation
    }
}

impl<S: Symbol> LinearWord<S> for PeriodicOmegaWord<S> {
    fn nth(&self, position: usize) -> Option<S> {
        todo!()
    }
}
impl<S: Symbol> OmegaWord<S> for PeriodicOmegaWord<S> {
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
///
/// Crucially, an instance of this struct will always be in reduced form. Specifically,
/// this means that there is no shorter representation of the omega word in the form
/// of a finite spoke and finite, non-empty cycle.
///
/// The reduced representation can be computed in polynomial time and it is unique.
/// We can compute it by calling [`ReducedOmegaWord::normalized()`] and it is possible to
/// verify whether a word is normalized through the [`ReducedOmegaWord::is_normalized()`]
/// method.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ReducedOmegaWord<S> {
    pub(super) word: Vec<S>,
    pub(super) loop_index: usize,
}

impl<S: Symbol> Show for ReducedOmegaWord<S> {
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

impl<S: Symbol> LinearWord<S> for ReducedOmegaWord<S> {
    fn nth(&self, position: usize) -> Option<S> {
        if position >= self.word.len() {
            let loop_position = (position - self.loop_index) % self.cycle_length();
            self.word.nth(self.loop_index + loop_position)
        } else {
            self.word.nth(position)
        }
    }
}
impl<S: Symbol> OmegaWord<S> for ReducedOmegaWord<S> {
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

impl<S: Symbol> ReducedOmegaWord<S> {
    /// Returns `true` if and only if `self` is already normalized. This is done by
    /// computing the normalization of `self` and then comparing structural equality.
    ///
    /// # Example
    /// ```
    /// use automata::prelude::*;
    /// let non_normalized = ReducedOmegaWord::from_raw_parts(vec!['a', 'a'], 0);
    /// assert!(!non_normalized.is_normalized()); // the constructed word is not normalized
    /// let normalized = non_normalized.normalized();
    /// assert!(normalized.is_normalized()); // the normalization is normalized
    /// assert!(normalized != non_normalized); // they are not syntactically equal
    /// assert!(normalized.equals(non_normalized)); // but they are semantically equal
    /// ```
    pub fn is_normalized(&self) -> bool {
        self.normalized() == *self
    }

    /// Creates a new instance from the given representation and loop index.
    pub fn from_raw_parts(repr: Vec<S>, loop_index: usize) -> Self {
        Self {
            word: repr,
            loop_index,
        }
    }

    /// Creates a new reduced omega word from a finite word. The input is deduplicated.
    pub fn periodic<W: FiniteWord<S>>(representation: W) -> Self {
        let representation = deduplicate(representation.to_vec());
        Self {
            word: representation,
            loop_index: 0,
        }
    }

    /// Removes the first symbol from the word. If the spoke is not empty (i.e. the loop index
    /// is greater than zero), we simply pop the first symbol of the prefix and decrease the
    /// loop index by one. Otherwise, if the spoke is empty, then the loop is rotated
    /// by one symbol.
    pub fn pop_front(&mut self) -> S {
        assert!(self.loop_index < self.word.len());
        if self.loop_index() > 0 {
            let mut it = self.word.iter().cloned();
            let out = it.next().expect("infinite word!");
            self.word = it.collect();
            self.loop_index -= 1;
            out
        } else {
            let out = *self
                .cycle()
                .first()
                .expect("this is an infinite word, loop must be non-empty");
            self.word.rotate_left(1);
            out
        }
    }

    /// Creates a new reduced omega word from a finite word representing the spoke and a finite
    /// word representing the cycle. The spoke must not be empty.
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

impl TryFrom<&str> for ReducedOmegaWord<char> {
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

/// Represents the omega iteration of a (non-empty) finite word.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct OmegaIteration<W>(W);

impl<W> OmegaIteration<W> {
    /// Iterate the given finite word `from`, panics if the word is empty.
    pub fn new<S: Symbol>(from: W) -> Self
    where
        W: FiniteWord<S>,
    {
        assert!(!from.is_empty(), "Cannot iterate an empty word");
        Self(from)
    }
}

impl<S: Symbol, W: FiniteWord<S>> LinearWord<S> for OmegaIteration<W> {
    fn nth(&self, position: usize) -> Option<S> {
        self.0.nth(position % self.0.len())
    }
}

impl<S: Symbol, W: FiniteWord<S>> OmegaWord<S> for OmegaIteration<W> {
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

impl<S: Show> Debug for ReducedOmegaWord<S> {
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

impl<S: Show> Debug for PeriodicOmegaWord<S> {
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
        word::{omega::deduplicate, ReducedOmegaWord},
        InfiniteLength,
    };

    use super::deduplicate_inplace;

    #[test]
    fn parse_reduced() {
        let repr = "abab";
        let nupw = super::ReducedOmegaWord::try_from(repr).unwrap();
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
        let reduced = super::ReducedOmegaWord::ultimately_periodic("aaaaaaa", "aaaaaaaaa");
        assert_eq!(reduced.word, vec!['a']);
        assert_eq!(
            ReducedOmegaWord::ultimately_periodic("aaaaaaaaa", "a").word,
            vec!['a']
        );
    }
}
