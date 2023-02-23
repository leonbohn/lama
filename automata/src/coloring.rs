pub trait Finite<X> {
    type Elements: Iterator<Item = X>;
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
    type Input;
    type Output;
    fn color(&self, of: &Self::Input) -> Self::Output;
}
