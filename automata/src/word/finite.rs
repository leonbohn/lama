use impl_tools::autoimpl;

use crate::prelude::Symbol;

use super::{omega::Reduced, LinearWord};

#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait FiniteWord<S>: LinearWord<S> {
    type Symbols<'this>: Iterator<Item = S>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_>;

    /// Constructs a [`NormalizedPeriodic`] object, which is the normalized periodic word that is
    /// represented by iterating `self` over and over again.
    fn omega_power(&self) -> Reduced<S>
    where
        S: Symbol,
    {
        Reduced::periodic(self)
    }

    fn len(&self) -> usize {
        self.representation_length()
    }

    fn nth_back(&self, pos: usize) -> Option<S> {
        self.nth(self.len() - pos - 1)
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl LinearWord<char> for str {
    fn nth(&self, position: usize) -> Option<char> {
        self.chars().nth(position)
    }

    fn representation_vec(&self) -> Vec<char> {
        self.chars().collect()
    }

    fn representation_length(&self) -> usize {
        self.len()
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
}

impl LinearWord<char> for String {
    fn nth(&self, position: usize) -> Option<char> {
        self.chars().nth(position)
    }

    fn representation_vec(&self) -> Vec<char> {
        self.chars().collect()
    }

    fn representation_length(&self) -> usize {
        self.len()
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

    fn representation_vec(&self) -> Vec<S> {
        self.clone()
    }

    fn representation_length(&self) -> usize {
        self.len()
    }
}
impl<S: Symbol> FiniteWord<S> for Vec<S> {
    type Symbols<'this> = std::iter::Cloned<std::slice::Iter<'this, S>>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.iter().cloned()
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

    fn representation_vec(&self) -> Vec<S> {
        self.to_vec()
    }

    fn representation_length(&self) -> usize {
        self.len()
    }
}
impl<S: Symbol> FiniteWord<S> for [S] {
    type Symbols<'this> = std::iter::Cloned<std::slice::Iter<'this, S>>
    where
        Self: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.iter().cloned()
    }
}
