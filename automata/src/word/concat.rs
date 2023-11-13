use std::hash::Hash;

use crate::{length::HasLength, FiniteLength, Length};

/// Represents the concatenation of `W` and `V`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Concat<W, V>(W, V);

#[cfg(test)]
mod tests {

    #[test]
    fn concatenations() {
        let prefix = "abc";
        let suffix = "def";
        todo!()
        // let combined = prefix.append(suffix);
        // assert_eq!(combined.finite_to_vec(), vec!['a', 'b', 'c', 'd', 'e', 'f']);
    }
}
