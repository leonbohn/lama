use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt::{Debug, Display},
    hash::Hash,
    ops::AddAssign,
};

use crate::{
    congruence::CongruenceTransition,
    run::{InducedPath, InfinitySet, ReachedState},
    ts::{InputOf, IntoTransitions, Path, StateOf, TransitionOf},
    Class, RightCongruence, Set, State, Successor, Symbol, Value,
};
mod append;
pub use append::Append;

mod prepend;
use impl_tools::autoimpl;
pub use prepend::Prepend;

mod finite;
mod infinite;
mod subword;

pub use finite::Str;
pub use infinite::{PeriodicWord, UltimatelyPeriodicWord};
pub use subword::Subword;
use tracing::trace;

/// Auxiliary type alias that simplifies referring to the object that running a word of
/// type `W` in a transition system of type `TS` induces.
///
/// # Examples
/// - If `W` is a word of [`FiniteLength`], then `WordInduces<W, TS>` is simply the state
/// reached in `TS` after reading `W`.
/// - If `W` is of [`InfiniteLength`], then this is instead the infinity set of the run, which
/// is the set of triggers that are used infinitely often.
pub type WordInduces<W, TS> = <<W as HasLength>::Len as Length>::Induces<StateOf<TS>, InputOf<TS>>;

/// Auxiliary type alias that allows easy access to the length of an input/word.
pub type LengthOf<W> = <W as HasLength>::Len;

/// Wrapper type for positions, this makes it a bit more explicit that this may not be an accurate
/// position along a run and is rather used for representing positions in the raw representation
/// of a word.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy)]
pub struct RawPosition(usize);

impl From<RawPosition> for usize {
    fn from(value: RawPosition) -> Self {
        value.0
    }
}

impl From<usize> for RawPosition {
    fn from(value: usize) -> Self {
        RawPosition(value)
    }
}

impl<I: Into<i32>> AddAssign<I> for RawPosition {
    fn add_assign(&mut self, rhs: I) {
        self.0 += rhs.into() as usize
    }
}

/// Abstracts the concept of length, allowing us to work with finite and infinite words in a
/// somewhat similar fashion.
pub trait Length: Eq + Ord + Hash + Debug + Display + Copy {
    /// The type of object that is induced by inputs of this length. This type is naturally
    /// parameterized on the state and symbol type.
    type Induces<Q: State, S: Symbol>: Value + From<InducedPath<Q, S, Self>>;
    /// Returns true if the length is finite.
    fn is_finite(&self) -> bool;
    /// Heavily used in the computation of a run (which is done by [`crate::run::Cane`]). For
    /// finite words, this method always either returns `None` (if the given `position` is out
    /// of bounds) or `Some(position)`. More interestingly, if the length is infinite, this
    /// method takes the reset point/loop index of the input into account.
    ///
    /// # Examples
    /// If we have an ultimately periodic word w = uv, then the loop index of w is l = |u|, which
    /// is the point to which we reset after going through the loop once. Thus for i < |uv|,
    /// `calculate_raw_position` returns `Some(i)`. Now if `i >= |uv|`, then the method returns
    /// `Some(j)`, where $ j = i + ((i - |u|) \bmod |v|) $. In other words whenever i exceeds or
    /// equals |uv|, we reset the position to the loop index l = |u|, i.e. the first position
    /// after the finite prefix.
    fn calculate_raw_position<I: Into<usize>>(&self, position: I) -> Option<RawPosition>;
    /// Returns `true` if `position` is the last position (wrt. the length defined by `self`).
    /// This method will always return `false` for infinite words and `true` for objects of
    /// [`FiniteLength`] n if `position` is larger or equal to n.
    fn is_end<I: Into<usize>>(&self, position: I) -> bool {
        self.calculate_raw_position(position.into()).is_none()
    }
}

/// Wrapper for things/words that have a finite length, which is simply an `usize`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[autoimpl(Deref using self.0)]
pub struct FiniteLength(pub usize);

impl Length for FiniteLength {
    type Induces<Q: State, S: Symbol> = ReachedState<Q>;
    fn is_finite(&self) -> bool {
        true
    }
    fn calculate_raw_position<I: Into<usize>>(&self, position: I) -> Option<RawPosition> {
        let position = position.into();
        if position >= **self {
            None
        } else {
            Some(position.into())
        }
    }
}

impl Display for FiniteLength {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn deduplicate_and_check<S>(u: &[S]) -> Option<&[S]>
where
    S: Eq,
{
    let n = u.len();
    let mut i = 1;
    while i < n {
        if n % i == 0 && u[i..] == u[..n - i] {
            return Some(&u[..i]);
        }
        i += 1;
    }
    None
}

fn normalize<'a, S>(length: &mut InfiniteLength, raw: &'a [S]) -> Cow<'a, [S]>
where
    S: Value,
{
    if length.is_ultimately_periodic() {
        // we need to do the shift, i.e. move the split position as far to the left
        // as possible and delete trailing symbols.
        let mut i = length.loop_index();
        while i > 0 && raw.get(i - 1) == raw.get(length.last_raw_position()) {
            i -= 1;
            length.0 -= 1;
            length.1 -= 1;
        }
    }

    if let Some(deduped) = deduplicate_and_check(length.suffix_of(raw)) {
        length.0 = length.1 + deduped.len();
        length
            .prefix_of(raw)
            .iter()
            .chain(deduped.iter())
            .cloned()
            .collect()
    } else {
        Cow::Borrowed(raw)
    }
}

/// Wrapper type for things of infinite length. The first field is the length of
/// the raw representation, while the second field is the loop index.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct InfiniteLength(pub usize, pub usize);
impl Length for InfiniteLength {
    type Induces<Q: State, S: Symbol> = InfinitySet<Q, S>;
    fn is_finite(&self) -> bool {
        false
    }
    fn calculate_raw_position<I: Into<usize>>(&self, position: I) -> Option<RawPosition> {
        let position = position.into();
        if position >= self.0 {
            Some(self.1.into())
        } else {
            Some(position.into())
        }
    }
}

impl InfiniteLength {
    /// Returns the loop index, which is the position that the looping part resets to.
    /// If you imagine an infinite, ultimately periodic word $ w = uv^\omega $, then the
    /// loop index is simply $ |u| $.
    pub fn loop_index(&self) -> usize {
        self.1
    }

    /// Get the raw index of the last position.
    pub fn last_raw_position(&self) -> usize {
        self.0 - 1
    }

    /// Returns true if the word is periodic, or equivalently the loop index is equal to zero.
    pub fn is_periodic(&self) -> bool {
        self.loop_index() == 0
    }

    /// Returns true if the word is ultimately periodic, but not periodic.
    pub fn is_ultimately_periodic(&self) -> bool {
        self.loop_index() > 0
    }

    /// Auxiliary method which considers `raw` as a word with [`Length`] self and extracts
    /// the prefix part. For a periodic word, this prefix is empty, while for an ultimately
    /// periodic word $ uv^\omega $, it corresponds to $ u $.
    pub fn prefix_of<'a, S>(&self, raw: &'a [S]) -> &'a [S] {
        &raw[..self.1]
    }

    /// Operates similarly to [`prefix_of`], but returns the looping part instead.
    pub fn suffix_of<'a, S>(&self, raw: &'a [S]) -> &'a [S] {
        &raw[self.1..self.0]
    }
}
impl Display for InfiniteLength {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{},{}", self.0, self.1)
    }
}

/// Implementors of this trait have a [`Length`] associated with them, for example words.
pub trait HasLength {
    /// The type of [`Length`] that is associated with `self`.
    type Len: Length;
    /// Returns the [`Length`] of `self`.
    fn length(&self) -> Self::Len;
}

impl<HL: HasLength> HasLength for &HL {
    type Len = HL::Len;
    fn length(&self) -> Self::Len {
        HL::length(*self)
    }
}

impl<HL: HasLength> HasLength for &mut HL {
    type Len = HL::Len;
    fn length(&self) -> Self::Len {
        HL::length(*self)
    }
}

/// Abstracts a word over some given alphabet. The type parameter `S` is the alphabet, and `Kind` is a marker type which indicates whether the word is finite or infinite.
#[autoimpl(for<T: trait> &T, &mut T)]
pub trait Word: Debug + Eq + std::hash::Hash + HasLength {
    /// The type of the symbols making up the word.
    type S: Symbol;

    /// Returns the symbol at the given index, or `None` if the index is out of bounds.
    fn nth<I: Into<usize>>(&self, index: I) -> Option<Self::S>;

    /// Returns the alphabet of `self`, that is the [`Set`] of [`Symbols`] which appear
    /// in `self`.
    fn alphabet(&self) -> Set<Self::S>;
}

/// A trait which allows iterating over the symbols of a word. For an infinite word, this is an infinite iterator.
pub trait SymbolIterable: Word + Copy {
    /// The iterator type.
    type SymbolIter: Iterator<Item = Self::S>;

    /// Returns an iterator over the symbols of the word.
    fn symbol_iter(self) -> Self::SymbolIter;
}

impl HasLength for String {
    type Len = FiniteLength;
    fn length(&self) -> Self::Len {
        FiniteLength(self.len())
    }
}

impl Word for String {
    type S = char;

    fn nth<I: Into<usize>>(&self, index: I) -> Option<Self::S> {
        self.chars().nth(index.into())
    }

    fn alphabet(&self) -> Set<Self::S> {
        self.chars().collect()
    }
}

impl HasLength for &str {
    type Len = FiniteLength;
    fn length(&self) -> Self::Len {
        FiniteLength(self.len())
    }
}

impl Word for &str {
    type S = char;

    fn nth<I: Into<usize>>(&self, index: I) -> Option<Self::S> {
        self.chars().nth(index.into())
    }

    fn alphabet(&self) -> Set<Self::S> {
        self.chars().collect()
    }
}

impl<S> HasLength for Vec<S> {
    type Len = FiniteLength;
    fn length(&self) -> Self::Len {
        FiniteLength(self.len())
    }
}

impl<S: Symbol> Word for Vec<S> {
    type S = S;

    fn nth<I: Into<usize>>(&self, index: I) -> Option<Self::S> {
        self.get(index.into()).cloned()
    }

    fn alphabet(&self) -> Set<Self::S> {
        self.iter().cloned().collect()
    }
}

/// Used to extract the transitions from a word viewed as a transition system.
/// Is an iterator that outputs the transitions as follows:
///
/// For a finite word w = abaab, it would emit transitions
///     ε -a-> a, a -b-> ab, ab -a-> aba, ...
///
/// Infinite words like w = uv^ω will produce transitions that mimick a finite path
/// on the symbols of u, to which a loop on the symbols of v is attached
#[derive(Debug, Clone)]
pub struct WordTransitions<W: Subword> {
    word: W,
    pos: usize,
}

impl<S: Symbol> Iterator for WordTransitions<&UltimatelyPeriodicWord<S>> {
    type Item = CongruenceTransition<S>;

    fn next(&mut self) -> Option<Self::Item> {
        let loop_back_point = self.word.base_length() + self.word.recur_length();

        trace!(
            "Pos is {}/{}, base {} and recur {}",
            self.pos,
            loop_back_point,
            self.word.base_length(),
            self.word.recur_length()
        );
        let ret = match self.pos.cmp(&loop_back_point) {
            std::cmp::Ordering::Less => Some((
                self.word.prefix(self.pos).into(),
                self.word
                    .nth(self.pos)
                    .expect("Was checked via base length"),
                self.word.prefix(self.pos + 1).into(),
            )),
            std::cmp::Ordering::Equal => Some((
                self.word.prefix(self.pos).into(),
                self.word
                    .nth(self.pos)
                    .expect("Should also be covered by length!"),
                self.word.prefix(self.word.base_length() + 1).into(),
            )),
            std::cmp::Ordering::Greater => None,
        };
        self.pos += 1;
        ret
    }
}

impl<W: Subword> WordTransitions<W> {
    /// Creates a new [`WordTransitions`] object.
    pub fn new(word: W) -> Self {
        Self { word, pos: 0 }
    }
}

impl<S: Symbol> Iterator for WordTransitions<&Str<S>> {
    type Item = CongruenceTransition<S>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = if self.pos < *self.word.length() {
            trace!("Pos is {}", self.pos);
            Some((
                self.word.prefix(self.pos).into(),
                self.word
                    .nth(self.pos)
                    .expect("Was checked via base length"),
                self.word.prefix(self.pos + 1).into(),
            ))
        } else {
            None
        };
        self.pos += 1;
        ret
    }
}

/// A macro for constructing an ultimately periodic word from string(s).
/// # Usage
/// - `upw!(v)` to create the periodic word $ v^\omega $
/// - `upw!(u, v)` to create the ultimately periodic word $ uv^\omega $.
#[macro_export]
macro_rules! upw {
    ($cyc:expr) => {
        $crate::words::UltimatelyPeriodicWord::from($crate::words::PeriodicWord::from($cyc))
    };
    ($base:expr, $cyc:expr) => {
        $crate::words::UltimatelyPeriodicWord::from((
            $crate::words::Str::from($base),
            $crate::words::PeriodicWord::from($cyc),
        ))
    };
}

/// A macro for constructing a finite word from a string.
#[macro_export]
macro_rules! word {
    ($symbols:expr) => {
        $crate::words::Str::from($symbols)
    };
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::ts::IntoParts;

    use super::*;
    #[test]
    fn symbol_iterability() {
        let word = Str::<usize>::from(vec![1, 3, 3, 7]);
        let mut iter = word.symbol_iter();
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(7));
        assert_eq!(iter.next(), None);

        let word = UltimatelyPeriodicWord(Str::epsilon(), PeriodicWord::from(vec![1, 3, 3, 7]));
        let mut iter = word.symbol_iter();
        assert_eq!(iter.next(), Some(1usize));
    }

    #[test]
    #[traced_test]
    fn word_as_ts() {
        let word = upw!("aa", "bb");
        let ts = word.into_ts();
        println!("{}", ts);
    }

    #[test]
    fn deduplicate_test() {
        let raw = [0, 1, 2, 0, 1, 2, 0, 1, 2];
        assert_eq!(deduplicate_and_check(&raw), Some(&raw[0..3]));
        let raw = [0, 0, 0, 0, 0];
        assert_eq!(deduplicate_and_check(&raw), Some(&raw[..1]));
    }
}
