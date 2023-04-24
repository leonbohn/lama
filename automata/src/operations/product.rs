use std::fmt::Display;

use itertools::Itertools;

use crate::{Mapping, Set, Symbol, TransitionSystem};

use super::union::Union;

pub trait DirectProduct<Rhs> {
    type Output;

    fn direct_product(self, rhs: Rhs) -> Self::Output;
}

impl<TS1: TransitionSystem, TS2: TransitionSystem> DirectProduct<TS2> for TS1 {
    type Output = Product<TS1, TS2>;

    fn direct_product(self, rhs: TS2) -> Self::Output {
        Product {
            left: self,
            right: rhs,
        }
    }
}

/// A product of two transition systems.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[allow(missing_docs)]
pub struct Product<L, R> {
    pub left: L,
    pub right: R,
}

impl<L, R> Product<L, R> {
    /// Creates a new product of two things.
    pub fn new(left: L, right: R) -> Self {
        Self { left, right }
    }

    /// Creates a new product from a pair of elements.
    pub fn from_pair(pair: (L, R)) -> Self {
        Self::new(pair.0, pair.1)
    }
}

impl<L, R> Product<L, R>
where
    L: Union<R>,
{
    /// Computes the union of two transition systems.
    pub fn union(self) -> <L as Union<R>>::Output {
        self.left.union(self.right)
    }
}

impl<L, R> Symbol for Product<L, R>
where
    L: Symbol,
    R: Symbol,
{
}

pub struct ProductIter<L, R> {
    left: L,
    right: R,
}

impl<L, R> Mapping for Product<L, R>
where
    L: Mapping,
    R: Mapping,
    L::Domain: Clone + std::hash::Hash + Eq,
    R::Domain: Clone + std::hash::Hash + Eq,
    L::Range: Clone + std::hash::Hash + Eq,
    R::Range: Clone + std::hash::Hash + Eq,
{
    type Domain = Product<L::Domain, R::Domain>;

    type Range = Product<L::Range, R::Range>;

    fn get_value(&self, of: &Self::Domain) -> Self::Range {
        Product::new(
            self.left.get_value(&of.left),
            self.right.get_value(&of.right),
        )
    }

    fn universe(&self) -> crate::Set<Self::Range> {
        let mut out = Set::new();
        for l in self.left.universe() {
            for r in self.right.universe() {
                out.insert(Product::new(l.clone(), r.clone()));
            }
        }
        out
    }

    fn domain(&self) -> Set<Self::Domain> {
        let mut out = Set::new();

        for l in self.left.domain() {
            for r in self.right.domain() {
                out.insert(Product::new(l.clone(), r.clone()));
            }
        }

        out
    }
}

impl<L, R> TransitionSystem for Product<L, R>
where
    L: TransitionSystem,
    R: TransitionSystem<Input = L::Input>,
{
    type State = Product<L::State, R::State>;

    type Input = L::Input;

    fn succ(&self, from: &Self::State, on: &Self::Input) -> Option<Self::State> {
        let Product { left, right } = from;
        self.left.succ(left, on).and_then(|left| {
            self.right
                .succ(right, on)
                .map(|right| Product { left, right })
        })
    }

    fn vec_alphabet(&self) -> Vec<Self::Input> {
        self.left
            .vec_alphabet()
            .into_iter()
            .chain(self.right.vec_alphabet())
            .unique()
            .collect()
    }

    fn vec_states(&self) -> Vec<Self::State> {
        self.left
            .vec_states()
            .into_iter()
            .cartesian_product(self.right.vec_states())
            .map(Product::from_pair)
            .unique()
            .collect()
    }

    fn set_alphabet(&self) -> ahash::HashSet<Self::Input> {
        self.left
            .vec_alphabet()
            .into_iter()
            .chain(self.right.vec_alphabet())
            .collect()
    }

    fn set_states(&self) -> ahash::HashSet<Self::State> {
        self.left
            .vec_states()
            .into_iter()
            .cartesian_product(self.right.vec_states())
            .map(Product::from_pair)
            .collect()
    }
}

impl<L, R> Display for Product<L, R>
where
    L: Display,
    R: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} * {})", self.left, self.right)
    }
}
