use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::Map;

/// A symbol of an alphabet, which is also the type of the symbols in a word. We consider different types
/// of alphabets:
/// - [`Simple`] alphabets, which are just a set of symbols.
/// - [`Propositional`] alphabets, where a symbol is a valuation of all propositional variables.
pub trait Symbol: PartialEq + Eq + Debug + Copy + Ord + PartialOrd + Hash {
    /// We do not want to force symbols to implement `Display` so we use this method for turning a single
    /// symbol into a `String`.
    fn show(&self) -> String;
}
impl<S: PartialEq + Eq + Debug + Copy + Ord + PartialOrd + Hash + Display> Symbol for S {
    fn show(&self) -> String {
        self.to_string()
    }
}

/// An expression is used to label edges of a [`crate::ts::TransitionSystem`]. For [`Simple`]
/// alphabets, an expression is simply a single symbol, whereas for a [`Propositional`] alphabet, an expression
/// is a propositional formula over the atomic propositions. See [`Propositional`] for more details.
pub trait Expression<S: Symbol>: Hash + Clone + Debug + Eq + Ord {
    /// Type of iterator over the concrete symbols matched by this expression.
    type SymbolsIter: Iterator<Item = S>;
    /// Returns an iterator over the [`Symbol`]s that match this expression.
    fn symbols(&self) -> Self::SymbolsIter;

    /// Checks whether the given [`Symbol`] matches the expression `self`. For [`Simple`] alphabets, this just
    /// means that the expression equals the given symbol. For a [`Propositional`] alphabet, this means that
    /// the expression is satisfied by the given symbol, an example of this is illustrated in [`Propositional`].
    fn matches(&self, symbol: S) -> bool;
}

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
    type Universe<'this>: Iterator<Item = &'this Self::Symbol>
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

/// A propositional alphabet is an alphabet where a [`Symbol`] is a valuation of all propositional variables.
///
/// # Example
/// Assume we have a propositional alphabet over the atomic propositions `a`, `b` and `c`.
///
/// Then a **symbol** in this alphabet is a valuation of these variables, e.g. `a & !b & c`. This is used to label
/// transitions in a [`crate::ts::TransitionSystem`].
///
/// An **expression** on the other hand is used to label edges and it is a boolean expression over
/// the atomic propositions, e.g. `(a | b) & c`. Such an expression is matched by
/// a symbol if the symbol satisfies the expression, i.e. if the expression evaluates to `true` under the given
/// valuation. The expression from above, for example, would be matched by the symbol given above (`a & !b & c`),
/// but not by the symbols `a & b & !c` or `!a & !b & c`.
#[derive(Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct Propositional {
    aps: Vec<String>,
}

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

    type Universe<'this> = std::iter::Empty<&'this Empty>
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

impl Expression<Empty> for Empty {
    type SymbolsIter = std::iter::Empty<Empty>;

    fn symbols(&self) -> Self::SymbolsIter {
        std::iter::empty()
    }

    fn matches(&self, _: Empty) -> bool {
        true
    }
}

/// Helper macro for creating a [`Simple`] alphabet. Is called simply with a list of symbols
/// that are separated by commata.
#[macro_export]
macro_rules! simple {
    ($($c:literal),*) => {
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
        Self(iter.into_iter().collect())
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

impl Expression<char> for char {
    type SymbolsIter = std::iter::Once<char>;
    fn symbols(&self) -> Self::SymbolsIter {
        std::iter::once(*self)
    }

    fn matches(&self, symbol: char) -> bool {
        self == &symbol
    }
}

impl Alphabet for Simple {
    type Symbol = char;

    type Expression = char;

    type Universe<'this> = std::slice::Iter<'this, char>
        where
            Self: 'this;

    fn matches(&self, expression: &Self::Expression, symbol: Self::Symbol) -> bool {
        expression == &symbol
    }

    fn expression(symbol: Self::Symbol) -> Self::Expression {
        symbol
    }

    fn universe(&self) -> Self::Universe<'_> {
        self.0.iter()
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
