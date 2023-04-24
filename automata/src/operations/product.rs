use std::fmt::Display;

use itertools::Itertools;

use crate::{
    ts::{HasStates, IntoTransitions, LengthLexicographicEdges, Visitor, VisitorIter},
    Mapping, Pointed, Set, Symbol, TransitionSystem,
};

use super::union::Union;

pub trait DirectProduct<Rhs> {
    type Output;

    fn direct_product(self, rhs: Rhs) -> Self::Output;
}

impl<TS1: TransitionSystem, TS2: TransitionSystem<Input = TS1::Input>> DirectProduct<TS2> for TS1 {
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

pub struct ProductIter<'a, L: TransitionSystem, R: TransitionSystem<Input = L::Input>> {
    states: Vec<(L::Q, R::Q)>,
    state_pos: usize,
    alphabet: Vec<L::Input>,
    alphabet_pos: usize,
    left: &'a L,
    right: &'a R,
}

impl<'a, L: TransitionSystem, R: TransitionSystem<Input = L::Input>> Iterator
    for ProductIter<'a, L, R>
{
    type Item = ((L::Q, R::Q), L::Input, (L::Q, R::Q));

    fn next(&mut self) -> Option<Self::Item> {
        if self.alphabet_pos >= self.alphabet.len() {
            self.state_pos += 1;
            self.alphabet_pos = 0;
        }
        if self.state_pos >= self.states.len() {
            return None;
        }
        if self.alphabet_pos >= self.alphabet.len() {
            return None;
        }

        let state = &self.states[self.state_pos];
        let sym = &self.alphabet[self.alphabet_pos];
        match (
            self.left.succ(&state.0, sym),
            self.right.succ(&state.1, sym),
        ) {
            (Some(left), Some(right)) => {
                self.alphabet_pos += 1;
                Some((state.clone(), sym.clone(), (left.clone(), right.clone())))
            }
            _ => {
                self.alphabet_pos += 1;
                self.next()
            }
        }
    }
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
    type Input = L::Input;

    fn succ(&self, from: &Self::Q, on: &Self::Input) -> Option<Self::Q> {
        let (left, right) = from;
        self.left
            .succ(left, on)
            .and_then(|left| self.right.succ(right, on).map(|right| (left, right)))
    }

    fn vec_alphabet(&self) -> Vec<Self::Input> {
        self.left
            .vec_alphabet()
            .into_iter()
            .chain(self.right.vec_alphabet())
            .unique()
            .sorted()
            .collect()
    }

    fn vec_states(&self) -> Vec<Self::Q> {
        self.left
            .vec_states()
            .into_iter()
            .cartesian_product(self.right.vec_states())
            .unique()
            .sorted()
            .collect()
    }

    fn set_alphabet(&self) -> ahash::HashSet<Self::Input> {
        self.left
            .vec_alphabet()
            .into_iter()
            .chain(self.right.vec_alphabet())
            .collect()
    }

    fn set_states(&self) -> ahash::HashSet<Self::Q> {
        self.left
            .vec_states()
            .into_iter()
            .cartesian_product(self.right.vec_states())
            .collect()
    }
}

impl<L: TransitionSystem, R: TransitionSystem<Input = L::Input>> HasStates for Product<L, R> {
    type Q = (L::Q, R::Q);

    type States<'me> = itertools::Product<L::States<'me>, R::States<'me>> where Self: 'me;

    fn states(&self) -> Set<Self::Q> {
        todo!()
    }
}

impl<'a, L, R> IntoTransitions for &'a Product<L, R>
where
    L: TransitionSystem,
    R: TransitionSystem<Input = L::Input>,
{
    type Transition = ((L::Q, R::Q), L::Input, (L::Q, R::Q));

    type TransitionIter = ProductIter<'a, L, R>;

    fn into_transitions(self) -> Self::TransitionIter {
        ProductIter {
            states: self.vec_states(),
            state_pos: 0,
            alphabet: self.vec_alphabet(),
            alphabet_pos: 0,
            left: &self.left,
            right: &self.right,
        }
    }
}

impl<L, R> Pointed for Product<L, R>
where
    L: Pointed,
    R: Pointed<Input = L::Input>,
{
    fn initial(&self) -> Self::Q {
        (self.left.initial(), self.right.initial())
    }
}

impl<L, R> Display for Product<L, R>
where
    L: Display,
    R: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Product of (\n{}\n\tAND\n{})", self.left, self.right)
    }
}

#[cfg(test)]
mod tests {
    use crate::{ts::IntoTransitions, Deterministic};

    #[test]
    fn simple_direct_product() {
        use crate::operations::DirectProduct;

        let left = Deterministic::from_iter([(0, 'a', 0), (0, 'b', 1), (1, 'a', 0), (1, 'b', 1)]);
        let right = Deterministic::from_iter([(0, 'a', 1), (0, 'b', 0), (1, 'a', 0), (1, 'b', 0)]);

        let prod = left.direct_product(right);
        println!("{}", prod);

        let edges = prod.into_transitions();
        for edge in edges {
            println!("{:?}", edge);
        }
    }
}
