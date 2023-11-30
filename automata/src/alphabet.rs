use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
    hash::Hash,
    rc::Rc,
};

use hoars::{ALPHABET, MAX_APS, VARS};
use itertools::Itertools;

use crate::{word::FiniteWord, Map, Show};

/// A symbol of an alphabet, which is also the type of the symbols in a word. We consider different types
/// of alphabets:
/// - [`Simple`] alphabets, which are just a set of symbols.
/// - [`Propositional`] alphabets, where a symbol is a valuation of all propositional variables.
pub trait Symbol: PartialEq + Eq + Debug + Copy + Ord + PartialOrd + Hash + Show {}
impl<S: PartialEq + Eq + Debug + Copy + Ord + PartialOrd + Hash + Show> Symbol for S {}

/// An expression is used to label edges of a [`crate::ts::TransitionSystem`]. For [`Simple`]
/// alphabets, an expression is simply a single symbol, whereas for a [`Propositional`] alphabet, an expression
/// is a propositional formula over the atomic propositions. See [`Propositional`] for more details.
pub trait Expression<S: Symbol>: Hash + Clone + Debug + Eq + Ord + Show {
    /// Type of iterator over the concrete symbols matched by this expression.
    type SymbolsIter<'this>: Iterator<Item = S>
    where
        Self: 'this;
    /// Returns an iterator over the [`Symbol`]s that match this expression.
    fn symbols(&self) -> Self::SymbolsIter<'_>;

    /// Checks whether the given [`Symbol`] matches the expression `self`. For [`Simple`] alphabets, this just
    /// means that the expression equals the given symbol. For a [`Propositional`] alphabet, this means that
    /// the expression is satisfied by the given symbol, an example of this is illustrated in [`Propositional`].
    fn matches(&self, symbol: S) -> bool;

    fn for_each<F: Fn(S)>(&self, f: F) {
        self.symbols().for_each(f)
    }
}

/// Type alias that can be used to extract the underlying alphabet of some object implementing [`HasAlphabet`].
pub type AlphabetOf<A> = <A as HasAlphabet>::Alphabet;

/// An alphabet abstracts a collection of [`Symbol`]s and complex [`Expression`]s over those.
#[impl_tools::autoimpl(for<T: trait + ?Sized> &T)]
pub trait Alphabet: Clone {
    /// The type of symbols in this alphabet.
    type Symbol: Symbol;
    /// The type of expressions in this alphabet.
    type Expression: Expression<Self::Symbol>;

    /// This method is used for an optimization: If we have a [`Simple`] alphabet, then an edge list essentially
    /// boils down to a map from `Self::Symbol` to an edge. For more complicated alphabets, this may not always
    /// be so easy. To allow for an optimization (i.e. just lookup the corresponding edge in a [`crate::Map`]),
    /// we force alphabets to implement this method.
    fn search_edge<X>(
        map: &Map<Self::Expression, X>,
        sym: Self::Symbol,
    ) -> Option<(&Self::Expression, &X)>;

    /// Type for an iterator over all possible symbols in the alphabet. For [`Propositional`] alphabets,
    /// this may return quite a few symbols (exponential in the number of atomic propositions).
    type Universe<'this>: Iterator<Item = Self::Symbol>
    where
        Self: 'this;

    /// Returns an iterator over all possible symbols in the alphabet. May return a huge number of symbols
    /// if the alphabet is [`Propositional`].
    fn universe(&self) -> Self::Universe<'_>;

    /// Returns true if the given symbol is present in the alphabet.
    fn contains(&self, symbol: Self::Symbol) -> bool;

    /// Checks whether the given expression matches the given symbol. For [`Simple`] alphabets, this just
    /// means that the expression equals the given symbol. For a [`Propositional`] alphabet, this means that
    /// the expression is satisfied by the given symbol, an example of this is illustrated in [`Propositional`].
    fn matches(&self, expression: &Self::Expression, symbol: Self::Symbol) -> bool;

    /// Creates an expression from a single symbol.
    fn expression(symbol: Self::Symbol) -> Self::Expression;
}

/// Abstracts posessing an [`Alphabet`], which can then be accessed via [`HasAlphabet::alphabet`].
#[impl_tools::autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait HasAlphabet {
    /// The type of alphabet posessed by the object
    type Alphabet: Alphabet;

    /// Returns a reference to the alphabet posessed by the object.
    fn alphabet(&self) -> &Self::Alphabet;
}

/// Helper trait for extracting the [`Symbol`] type from an an object which implements [`HasAlphabet`].
pub type SymbolOf<A> = <<A as HasAlphabet>::Alphabet as Alphabet>::Symbol;
/// Helper trait for extracting the [`Expression`] type from an an object which implements [`HasAlphabet`].
pub type ExpressionOf<A> = <<A as HasAlphabet>::Alphabet as Alphabet>::Expression;

/// A simple alphabet is an alphabet where a [`Symbol`] is just a single character.
///
/// # Example
/// Assume we have a simple alphabet over the symbols 'a' and 'b'. Then a **symbol** would be just one of these
/// characters, e.g. 'a'. This is used to label transitions in a [`crate::ts::TransitionSystem`].
/// Now an **expression** would also be just a single character, e.g. 'a'. Then such an expression is
/// matched by a symbol if the expression equals the symbol.
#[derive(Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct Simple(Vec<char>);

/// A special type of [`Alphabet`], which has no symbols.
#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct Empty;

impl std::fmt::Display for Empty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "->")
    }
}

impl Alphabet for Empty {
    type Symbol = Empty;

    type Expression = Empty;

    fn search_edge<X>(
        _map: &Map<Self::Expression, X>,
        _sym: Self::Symbol,
    ) -> Option<(&Self::Expression, &X)> {
        todo!()
    }

    type Universe<'this> = std::iter::Empty<Empty>
    where
        Self: 'this;

    fn universe(&self) -> Self::Universe<'_> {
        std::iter::empty()
    }

    fn contains(&self, _symbol: Self::Symbol) -> bool {
        true
    }

    fn matches(&self, _expression: &Self::Expression, _symbol: Self::Symbol) -> bool {
        true
    }

    fn expression(_symbol: Self::Symbol) -> Self::Expression {
        Empty
    }
}

impl Show for Empty {
    fn show(&self) -> String {
        todo!()
    }

    fn show_collection<'a, I: IntoIterator<Item = &'a Self>>(iter: I) -> String
    where
        Self: 'a,
    {
        todo!()
    }
}
impl Expression<Empty> for Empty {
    type SymbolsIter<'this> = std::iter::Empty<Empty> where Self: 'this;

    fn symbols(&self) -> Self::SymbolsIter<'_> {
        std::iter::empty()
    }

    fn matches(&self, _: Empty) -> bool {
        true
    }
}

/// Helper macro for creating a [`Simple`] alphabet. Is called simply with a list of symbols
/// that are separated by commata.
#[macro_export]
macro_rules! alphabet {
    (simple $($c:literal),*) => {
        $crate::alphabet::Simple::new(vec![$($c),*])
    };
}

impl From<Vec<char>> for Simple {
    fn from(value: Vec<char>) -> Self {
        Self(value)
    }
}

impl FromIterator<char> for Simple {
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
        Self(iter.into_iter().unique().collect())
    }
}

impl Simple {
    /// Creates a new [`Simple`] alphabet from an iterator over the symbols.
    pub fn new<I>(symbols: I) -> Self
    where
        I: IntoIterator<Item = char>,
    {
        Self(symbols.into_iter().collect())
    }
}

impl Show for char {
    fn show(&self) -> String {
        self.to_string()
    }

    fn show_collection<'a, I: IntoIterator<Item = &'a Self>>(iter: I) -> String
    where
        Self: 'a,
    {
        format!(
            "\"{}\"",
            iter.into_iter().map(|sym| sym.to_string()).join("")
        )
    }
}
impl Expression<char> for char {
    type SymbolsIter<'this> = std::iter::Once<char> where Self: 'this;
    fn symbols(&self) -> Self::SymbolsIter<'_> {
        std::iter::once(*self)
    }

    fn for_each<F: Fn(char)>(&self, f: F) {
        (f)(*self)
    }

    fn matches(&self, symbol: char) -> bool {
        self == &symbol
    }
}

impl Alphabet for Simple {
    type Symbol = char;

    type Expression = char;

    type Universe<'this> = std::iter::Cloned<std::slice::Iter<'this, char>>
        where
            Self: 'this;

    fn matches(&self, expression: &Self::Expression, symbol: Self::Symbol) -> bool {
        expression == &symbol
    }

    fn expression(symbol: Self::Symbol) -> Self::Expression {
        symbol
    }

    fn universe(&self) -> Self::Universe<'_> {
        self.0.iter().cloned()
    }

    fn contains(&self, symbol: Self::Symbol) -> bool {
        self.0.contains(&symbol)
    }

    #[inline(always)]
    fn search_edge<X>(
        map: &Map<Self::Expression, X>,
        sym: Self::Symbol,
    ) -> Option<(&Self::Expression, &X)> {
        map.get_key_value(&sym)
    }
}

/// An alphabet of fixed arity, uses const generics. This is more seen as a test
/// since the performance gains (at least for simple operations like runs) is
/// negligible.
#[derive(Clone, Debug)]
pub struct Fixed<S: Symbol, const N: usize>([S; N]);

impl Expression<usize> for usize {
    type SymbolsIter<'this> = std::iter::Once<usize> where Self: 'this;

    fn symbols(&self) -> Self::SymbolsIter<'_> {
        std::iter::once(*self)
    }

    fn matches(&self, symbol: usize) -> bool {
        *self == symbol
    }
}

impl<S: Symbol, const N: usize> Fixed<S, N> {
    /// Create a new [`Fixed`] alphabet from a slice of length `N`.
    pub fn from(symbols: [S; N]) -> Self {
        Self(symbols)
    }
}

impl<S: Symbol + Expression<S>, const N: usize> Alphabet for Fixed<S, N> {
    type Symbol = S;

    type Expression = S;

    fn search_edge<X>(
        map: &Map<Self::Expression, X>,
        sym: Self::Symbol,
    ) -> Option<(&Self::Expression, &X)> {
        map.get_key_value(&sym)
    }

    type Universe<'this> = std::iter::Cloned<std::slice::Iter<'this, S>>
    where
        Self: 'this;

    fn universe(&self) -> Self::Universe<'_> {
        self.0.iter().cloned()
    }

    fn contains(&self, symbol: Self::Symbol) -> bool {
        self.0.contains(&symbol)
    }

    fn matches(&self, expression: &Self::Expression, symbol: Self::Symbol) -> bool {
        expression == &symbol
    }

    fn expression(symbol: Self::Symbol) -> Self::Expression {
        symbol
    }
}

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct InvertibleChar(char, bool);

impl InvertibleChar {
    pub fn mul(&self, word: &mut VecDeque<char>) {
        if self.1 {
            word.push_front(self.0)
        } else {
            word.push_back(self.0)
        }
    }

    pub fn is_inverted(&self) -> bool {
        self.1
    }
}

impl Expression<InvertibleChar> for InvertibleChar {
    type SymbolsIter<'this> = std::iter::Once<InvertibleChar> where Self: 'this;

    fn symbols(&self) -> Self::SymbolsIter<'_> {
        std::iter::once(*self)
    }

    fn matches(&self, symbol: InvertibleChar) -> bool {
        *self == symbol
    }
}

impl Show for InvertibleChar {
    fn show(&self) -> String {
        format!("{}{}", self.0, if self.1 { "\u{0303}" } else { "" })
    }

    fn show_collection<'a, I: IntoIterator<Item = &'a Self>>(iter: I) -> String
    where
        Self: 'a,
        I::IntoIter: DoubleEndedIterator,
    {
        format!("'{}'", iter.into_iter().rev().map(|c| c.show()).join(""))
    }
}

#[derive(Clone, Debug)]
pub struct Directional(Vec<InvertibleChar>);

impl Directional {
    pub fn from_iter<I: IntoIterator<Item = char>>(iter: I) -> Self {
        let mut v = vec![];
        for sym in iter {
            v.push(InvertibleChar(sym, false));
            v.push(InvertibleChar(sym, true));
        }
        Self(v)
    }
    pub fn from_alphabet<A: std::borrow::Borrow<Simple>>(alphabet: A) -> Self {
        Self::from_iter(alphabet.borrow().universe())
    }
}

impl Alphabet for Directional {
    type Symbol = InvertibleChar;

    type Expression = InvertibleChar;

    fn search_edge<X>(
        map: &Map<Self::Expression, X>,
        sym: Self::Symbol,
    ) -> Option<(&Self::Expression, &X)> {
        todo!()
    }

    type Universe<'this> = std::iter::Cloned<std::slice::Iter<'this, InvertibleChar>>
    where
        Self: 'this;

    fn universe(&self) -> Self::Universe<'_> {
        self.0.iter().cloned()
    }

    fn contains(&self, symbol: Self::Symbol) -> bool {
        self.0.contains(&symbol)
    }

    fn matches(&self, expression: &Self::Expression, symbol: Self::Symbol) -> bool {
        *expression == symbol
    }

    fn expression(symbol: Self::Symbol) -> Self::Expression {
        symbol
    }
}

#[cfg(test)]
mod tests {
    use super::{Directional, Simple};
    use crate::Alphabet;
    use itertools::Itertools;

    #[test]
    fn bialphabet() {
        let alph = Simple::from_iter(['a', 'b', 'c']);
        let bi = Directional::from_alphabet(alph);
        println!("{:?}", bi.universe().collect_vec())
    }
}
