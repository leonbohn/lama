use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::{
    congruence::CongruenceTransition,
    run::{InducedPath, InfinitySet, ReachedState},
    ts::{IntoTransitions, Path, StateOf, TransitionOf},
    Boundedness, Class, FiniteKind, RightCongruence, Set, State, Successor, Symbol, Value,
};
mod append;
pub use append::Append;

mod prepend;
use impl_tools::autoimpl;
pub use prepend::Prepend;

mod finite;
mod infinite;
mod represent;
mod subword;

pub use finite::Str;
pub use infinite::{PeriodicWord, UltimatelyPeriodicWord};
pub use represent::{Repr, Representable};
pub use subword::Subword;
use tracing::trace;

pub trait Length: Eq + Ord + Hash + Debug + Display + Copy {
    type Induces<Q: State, S: Symbol>: Eq + From<InducedPath<Q, S, Self>>;
    fn is_finite(&self) -> bool;
    fn calculate_raw_position(&self, position: usize) -> Option<usize>;
    fn is_end(&self, position: usize) -> bool {
        self.calculate_raw_position(position).is_none()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[autoimpl(Deref using self.0)]
pub struct FiniteLength(pub usize);

impl Length for FiniteLength {
    type Induces<Q: State, S: Symbol> = ReachedState<Q>;
    fn is_finite(&self) -> bool {
        true
    }
    fn calculate_raw_position(&self, position: usize) -> Option<usize> {
        if position >= **self {
            None
        } else {
            Some(position)
        }
    }
}

impl Display for FiniteLength {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct InfiniteLength(pub usize, pub usize);
impl Length for InfiniteLength {
    type Induces<Q: State, S: Symbol> = InfinitySet<Q, S>;
    fn is_finite(&self) -> bool {
        false
    }
    fn calculate_raw_position(&self, position: usize) -> Option<usize> {
        if position >= self.0 {
            Some(self.1)
        } else {
            Some(position)
        }
    }
}

impl InfiniteLength {
    pub fn loop_index(&self) -> usize {
        self.1
    }
}
impl Display for InfiniteLength {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{},{}", self.0, self.1)
    }
}

pub trait HasLength {
    type Len: Length;
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
    fn nth(&self, index: usize) -> Option<Self::S>;

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

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.chars().nth(index)
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

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.chars().nth(index)
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

    fn nth(&self, index: usize) -> Option<Self::S> {
        self.get(index).cloned()
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
}
