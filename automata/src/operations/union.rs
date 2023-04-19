use crate::{Map, Mapping, Priority};

use super::{product::DirectProduct, Product};

pub trait Union<Rhs = Self> {
    type Output;

    fn union(self, rhs: Rhs) -> Self::Output;
}

impl Union for bool {
    type Output = bool;

    fn union(self, rhs: Self) -> Self::Output {
        self || rhs
    }
}

impl Union for Priority {
    type Output = Priority;

    fn union(self, rhs: Self) -> Self::Output {
        self.max(rhs)
    }
}
