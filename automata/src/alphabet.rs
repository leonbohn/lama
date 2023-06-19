use std::{borrow::Borrow, hash::Hash};

pub trait Symbol: PartialEq + Copy + PartialOrd + Hash {}
impl<S: PartialEq + Copy + PartialOrd + Hash> Symbol for S {}

pub trait Expression<S: Symbol> {
    type SymbolsIter: Iterator<Item = S>;
    fn symbols(&self) -> Self::SymbolsIter;

    fn matches(&self, symbol: S) -> bool;
}

pub trait Alphabet {
    type Symbol: Symbol;
    type Expression: Expression<Self::Symbol>;
    type Universe<'this>: Iterator<Item = &'this Self::Symbol>
    where
        Self: 'this;

    fn universe(&self) -> Self::Universe<'_>;
    fn contains(&self, symbol: Self::Symbol) -> bool;

    fn matches(&self, expression: &Self::Expression, symbol: Self::Symbol) -> bool;
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub struct Propositional {
    aps: Vec<String>,
}

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

    type Universe<'this> = std::slice::Iter<'this, char>
        where
            Self: 'this;

    fn universe(&self) -> Self::Universe<'_> {
        self.0.iter()
    }

    fn contains(&self, symbol: Self::Symbol) -> bool {
        self.0.contains(&symbol)
    }

    fn matches(&self, expression: &Self::Expression, symbol: Self::Symbol) -> bool {
        expression == &symbol
    }
}
