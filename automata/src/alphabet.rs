use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
    hash::Hash,
};

/// A symbol of an alphabet, which is also the type of the symbols in a word. We consider different types
/// of alphabets:
/// - [`Simple`] alphabets, which are just a set of symbols.
/// - [`Propositional`] alphabets, where a symbol is a valuation of all propositional variables.
pub trait Symbol: PartialEq + Debug + Copy + PartialOrd + Hash {}
impl<T: PartialEq + Debug + Copy + PartialOrd + Hash> Symbol for T {}

/// An expression is used to label [`crate::ts::Edge`]s of a [`crate::ts::TransitionSystem`]. For [`Simple`]
/// alphabets, an expression is simply a single symbol, whereas for a [`Propositional`] alphabet, an expression
/// is a propositional formula over the atomic propositions. See [`Propositional`] for more details.
pub trait Expression<S: Symbol> {
    /// Type of iterator over the concrete symbols matched by this expression.
    type SymbolsIter: Iterator<Item = S>;
    /// Returns an iterator over the [`Symbol`]s that match this expression.
    fn symbols(&self) -> Self::SymbolsIter;

    /// Checks whether the given [`Symbol`] matches the expression `self`. For [`Simple`] alphabets, this just
    /// means that the expression equals the given symbol. For a [`Propositional`] alphabet, this means that
    /// the expression is satisfied by the given symbol, an example of this is illustrated in [`Propositional`].
    fn matches(&self, symbol: S) -> bool;
}

pub trait HasUniverse: Alphabet {
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
}

/// An alphabet abstracts a collection of [`Symbol`]s and complex [`Expression`]s over those.
pub trait Alphabet {
    /// The type of symbols in this alphabet.
    type Symbol: Symbol;
    /// The type of expressions in this alphabet.
    type Expression: Expression<Self::Symbol>;

    /// Checks whether the given expression matches the given symbol. For [`Simple`] alphabets, this just
    /// means that the expression equals the given symbol. For a [`Propositional`] alphabet, this means that
    /// the expression is satisfied by the given symbol, an example of this is illustrated in [`Propositional`].
    fn matches(&self, expression: &Self::Expression, symbol: Self::Symbol) -> bool;
}

/// Abstracts posessing an [`Alphabet`], which can then be accessed via [`HasAlphabet::alphabet`].
pub trait HasAlphabet {
    /// The type of alphabet posessed by the object
    type Alphabet: Alphabet;

    /// Returns a reference to the alphabet posessed by the object.
    fn alphabet(&self) -> &Self::Alphabet;
}

/// Helper trait for extracting the [`Symbol`] type from an an object which implements [`HasAlphabet`].
pub type SymbolOf<A> = <<A as HasAlphabet>::Alphabet as Alphabet>::Symbol;

/// A propositional alphabet is an alphabet where a [`Symbol`] is a valuation of all propositional variables.
///
/// # Example
/// Assume we have a propositional alphabet over the atomic propositions `a`, `b` and `c`.
///
/// Then a **symbol** in this alphabet is a valuation of these variables, e.g. `a & !b & c`. This is used to label
/// [`crate::ts::Transition`]s in a [`crate::ts::TransitionSystem`].
///
/// An **expression** on the other hand is used to label [`crate::ts::Edge`]s and it is a boolean expression over
/// the atomic propositions, e.g. `(a | b) & c`. Such an expression is [`super::Alphabet::matches`](matched) by
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
/// characters, e.g. 'a'. This is used to label [`crate::ts::Transition`]s in a [`crate::ts::TransitionSystem`].
/// Now an **expression** would also be just a single character, e.g. 'a'. Then such an expression is
/// [`super::Alphabet::matches`](matched) by a symbol if the expression equals the symbol.
#[derive(Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct Simple(Vec<char>);

impl FromIterator<char> for Simple {
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl Simple {
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

    fn matches(&self, expression: &Self::Expression, symbol: Self::Symbol) -> bool {
        expression == &symbol
    }
}

impl HasUniverse for Simple {
    type Universe<'this> = std::slice::Iter<'this, char>
        where
            Self: 'this;

    fn universe(&self) -> Self::Universe<'_> {
        self.0.iter()
    }

    fn contains(&self, symbol: Self::Symbol) -> bool {
        self.0.contains(&symbol)
    }
}
