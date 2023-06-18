use std::borrow::Borrow;

pub trait Expression<S> {
    type SymbolsIter: Iterator<Item = S>;
    fn symbols(&self) -> Self::SymbolsIter;

    fn matches(&self, symbol: S) -> bool;
}

pub trait Alphabet {
    type Symbol: PartialEq + Copy;
    type Expression: Expression<Self::Symbol>;
    type Universe<'this>: Iterator<Item = &'this Self::Symbol>
    where
        Self: 'this;

    fn universe(&self) -> Self::Universe<'_>;
    fn contains(&self, symbol: Self::Symbol) -> bool;

    fn matches<E>(&self, expression: &E, symbol: Self::Symbol) -> bool
    where
        E: Borrow<Self::Expression>;
}

pub struct Propositional {
    aps: Vec<String>,
}

pub struct Simple {
    symbols: Vec<char>,
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
        self.symbols.iter()
    }

    fn contains(&self, symbol: Self::Symbol) -> bool {
        self.symbols.contains(&symbol)
    }

    fn matches<E>(&self, expression: &E, symbol: Self::Symbol) -> bool
    where
        E: Borrow<Self::Expression>,
    {
        expression.borrow() == &symbol
    }
}
