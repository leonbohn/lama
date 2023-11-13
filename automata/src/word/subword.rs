use crate::{length::HasLength, FiniteLength, Length};

use super::LinearWord;

/// A suffix of a [`Word`] which skips the first `offset` symbols.
#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub struct Offset<'a, S, W: LinearWord<S>> {
    sequence: &'a W,
    offset: usize,
    _marker: std::marker::PhantomData<S>,
}

impl<'a, S, W: LinearWord<S>> Offset<'a, S, W> {
    /// Creates a new suffix, which skips the first `offset` symbols of the given sequence.
    pub fn new(sequence: &'a W, offset: usize) -> Self {
        Self {
            sequence,
            offset,
            _marker: std::marker::PhantomData,
        }
    }
}

/// A suffix of a [`Word`] which skips the first `offset` symbols.
#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub struct Infix<'a, S, W: LinearWord<S>> {
    sequence: &'a W,
    offset: usize,
    length: usize,
    _marker: std::marker::PhantomData<S>,
}

impl<'a, S, W: LinearWord<S>> Infix<'a, S, W> {
    /// Creates a new suffix, which skips the first `offset` symbols of the given sequence.
    pub fn new(sequence: &'a W, offset: usize, length: usize) -> Self {
        Self {
            sequence,
            offset,
            length,
            _marker: std::marker::PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{upw, word::OmegaWord, FiniteLength};

    #[test]
    fn subwords() {
        todo!();
        // let word = OmegaWord::new(vec!['a', 'b', 'a', 'b'], FiniteLength::new(4));
        // let pref = word.prefix(2);
        // assert_eq!(pref.raw_to_vec(), vec!['a', 'b']);

        // let word = upw!("ab", "ac");
        // assert_eq!(
        //     word.offset(3).prefix(4).finite_to_vec(),
        //     vec!['c', 'a', 'c', 'a']
        // );
        // assert_eq!(
        //     word.offset(1)
        //         .offset(1)
        //         .offset(1)
        //         .offset(1)
        //         .offset(4)
        //         .prefix(2)
        //         .finite_to_vec(),
        //     vec!['a', 'c']
        // );
        // assert_eq!(
        //     upw!("abba").offset(1).offset(20).normalized().raw_to_vec(),
        //     vec!['b', 'b', 'a', 'a']
        // );
    }
}
