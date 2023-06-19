use super::Representable;

pub struct Suffix<'a, R: Representable> {
    representable: &'a R,
    offset: usize,
}

impl<'a, R: Representable> Representable for Suffix<'a, R> {
    type Alphabet = R::Alphabet;
    fn nth<P: super::AsPosition>(
        &self,
        position: P,
    ) -> Option<<Self::Alphabet as crate::Alphabet>::Symbol> {
        self.representable
            .nth(*position.as_position() + self.offset)
    }
}

impl<'a, R: Representable> Suffix<'a, R> {
    pub fn new(representable: &'a R, offset: usize) -> Self {
        Self {
            representable,
            offset,
        }
    }
}

pub struct Prefix<'a, R: Representable> {
    representable: &'a R,
    length: usize,
}

impl<'a, R: Representable> Representable for Prefix<'a, R> {
    type Alphabet = R::Alphabet;
    fn nth<P: super::AsPosition>(
        &self,
        position: P,
    ) -> Option<<Self::Alphabet as crate::Alphabet>::Symbol> {
        if *position.as_position() < self.length {
            self.representable.nth(position)
        } else {
            None
        }
    }
}

impl<'a, R: Representable> Prefix<'a, R> {
    pub fn new(representable: &'a R, length: usize) -> Self {
        Self {
            representable,
            length,
        }
    }
}
