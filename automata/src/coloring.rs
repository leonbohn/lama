use std::{collections::HashSet, hash::Hash};

/// Abstracts the finiteness of the type `X`.
pub trait Finite<X> {
    /// The type of an Iterator over the the finite universe of `X`.
    type Elements: Iterator<Item = X>;
    ///
    fn universe() -> Self::Elements;
}

impl Finite<u32> for u32 {
    type Elements = std::ops::RangeInclusive<u32>;

    fn universe() -> Self::Elements {
        0..=(u32::MAX)
    }
}

/// Holds the color (i.e. label) of either a transition or of a state.
/// The type C is the label, which by default is a u32.
#[derive(Debug, Clone)]
pub struct Color<C = u32>(C);

/// Encodes a total coloring of `X`. Panics if no color is present.
pub trait Coloring {
    /// The type of the input.
    type Input;
    /// The type of the output.
    type Output;
    /// Returns the color assigned to the input.
    fn color(&self, of: &Self::Input) -> Self::Output;
}

impl<Id: Eq + Hash> Coloring for HashSet<Id> {
    type Input = Id;

    type Output = bool;

    fn color(&self, of: &Self::Input) -> Self::Output {
        self.contains(of)
    }
}
