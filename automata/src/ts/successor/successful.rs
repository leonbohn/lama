use crate::{
    alphabet::{Alphabet, Symbol},
    length::HasLength,
    ts::Path,
    word::RawWithLength,
    Color,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Successful<'a, 'b, R, A: Alphabet, C> {
    word: &'b R,
    path: Path<'a, A, C>,
}

impl<'a, 'b, R, A: Alphabet, C: Color> Successful<'a, 'b, R, A, C> {
    pub fn new(word: &'b R, path: Path<'a, A, C>) -> Self {
        Self { word, path }
    }

    pub fn colors(&self) -> RawWithLength<Vec<C>, R::Length>
    where
        R: HasLength,
        C: Clone + Symbol,
    {
        RawWithLength::new(
            self.path.color_sequence().map(Clone::clone).collect(),
            self.word.length(),
        )
    }
}
