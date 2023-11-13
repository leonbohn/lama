use impl_tools::autoimpl;

use crate::prelude::Symbol;

use super::{omega::Reduced, Concat, LinearWord};

pub trait FiniteWord<S>: LinearWord<S> {
    type Symbols<'this>: Iterator<Item = S>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_>;

    fn append<W: LinearWord<S>>(self, suffix: W) -> Concat<Self, W>
    where
        Self: Sized,
    {
        Concat(self, suffix)
    }

    fn prepend<W: FiniteWord<S>>(self, prefix: W) -> Concat<W, Self>
    where
        Self: Sized,
    {
        Concat(prefix, self)
    }

    fn to_vec(&self) -> Vec<S>;

    /// Constructs a [`NormalizedPeriodic`] object, which is the normalized periodic word that is
    /// represented by iterating `self` over and over again.
    fn omega_power(&self) -> Reduced<S>
    where
        S: Symbol,
    {
        Reduced::periodic(self)
    }

    fn len(&self) -> usize;

    fn nth_back(&self, pos: usize) -> Option<S> {
        self.nth(self.len() - pos - 1)
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<S: Symbol, Fw: FiniteWord<S> + ?Sized> FiniteWord<S> for &Fw {
    type Symbols<'this> = Fw::Symbols<'this> where Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        (*self).symbols()
    }

    fn len(&self) -> usize {
        (*self).len()
    }
    fn to_vec(&self) -> Vec<S> {
        (*self).to_vec()
    }
}

impl LinearWord<char> for str {
    fn nth(&self, position: usize) -> Option<char> {
        self.chars().nth(position)
    }
}

impl FiniteWord<char> for str {
    type Symbols<'this> = std::str::Chars<'this>;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.chars()
    }

    fn len(&self) -> usize {
        self.len()
    }
    fn to_vec(&self) -> Vec<char> {
        self.chars().collect()
    }
}

impl LinearWord<char> for String {
    fn nth(&self, position: usize) -> Option<char> {
        self.chars().nth(position)
    }
}

impl FiniteWord<char> for String {
    fn to_vec(&self) -> Vec<char> {
        self.chars().collect()
    }

    fn len(&self) -> usize {
        self.len()
    }

    type Symbols<'this> = std::str::Chars<'this>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.chars()
    }
}

impl<S: Symbol> LinearWord<S> for Vec<S> {
    fn nth(&self, position: usize) -> Option<S> {
        if position < self.len() {
            Some(self[position])
        } else {
            None
        }
    }
}
impl<S: Symbol> FiniteWord<S> for Vec<S> {
    type Symbols<'this> = std::iter::Cloned<std::slice::Iter<'this, S>>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.iter().cloned()
    }

    fn to_vec(&self) -> Vec<S> {
        self.clone()
    }

    fn len(&self) -> usize {
        self.len()
    }
}

impl<S: Symbol> LinearWord<S> for [S] {
    fn nth(&self, position: usize) -> Option<S> {
        if position < self.len() {
            Some(self[position])
        } else {
            None
        }
    }
}
impl<S: Symbol> FiniteWord<S> for [S] {
    type Symbols<'this> = std::iter::Cloned<std::slice::Iter<'this, S>>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.iter().cloned()
    }

    fn to_vec(&self) -> Vec<S> {
        self.to_vec()
    }

    fn len(&self) -> usize {
        self.len()
    }
}
