use crate::{
    alphabet::{Alphabet, Symbol},
    ts::Path,
    word::RawWithLength,
    Color, FiniteLength,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Partial<'a, 'b, R, A: Alphabet, C> {
    word: &'b R,
    position: usize,
    path: Path<'a, A, C>,
}

impl<'a, 'b, R, A: Alphabet, C: Color> Partial<'a, 'b, R, A, C> {
    pub fn new(word: &'b R, position: usize, path: Path<'a, A, C>) -> Self {
        Self {
            word,
            position,
            path,
        }
    }

    pub fn colors(&self) -> RawWithLength<Vec<C>, FiniteLength>
    where
        C: Clone + Symbol,
    {
        RawWithLength::new(
            self.path.color_sequence().map(Clone::clone).collect(),
            FiniteLength::new(self.position),
        )
    }
}
