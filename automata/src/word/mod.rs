use crate::{length::HasLength, Alphabet, FiniteLength, InfiniteLength, Length};

mod representable;
pub use representable::{AsPosition, Representable};

pub mod subword;

pub type FiniteWord<A> = Word<A, FiniteLength>;
pub type InfiniteWord<A> = Word<A, InfiniteLength>;

pub struct Word<A: Alphabet, L> {
    symbols: Vec<A::Symbol>,
    length: L,
}

impl<A: Alphabet, L: Length> HasLength for Word<A, L> {
    type Len = L;

    fn length(&self) -> Self::Len {
        self.length
    }
}
