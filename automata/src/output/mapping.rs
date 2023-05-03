use std::{borrow::Borrow, hash::Hash};

use impl_tools::autoimpl;
use itertools::Itertools;

use crate::{Map, StateIndex, Symbol, Value};

use super::with_output::HasOutput;

pub mod types {
    use super::{Assignment, IntoAssignments};

    /// Helper type to access the left and right type of an assignment.
    pub type AssignmentRefLeft<A> = <<A as IntoAssignments>::AssignmentRef as Assignment>::Left;
    pub type AssignmentRefRight<A> = <<A as IntoAssignments>::AssignmentRef as Assignment>::Right;
}

/// Captures the mapping from a type `X` to a type `Y`.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Mapping<X: Hash + Eq, Y> {
    map: Map<X, Y>,
}

impl<X: Value, Y: Value> Default for Mapping<X, Y> {
    fn default() -> Self {
        Self {
            map: Map::default(),
        }
    }
}

impl<X, Y> Mapping<X, Y>
where
    X: Value,
    Y: Value,
{
    /// Returns the number of unique values taken by the mapping.
    pub fn codomain_size(&self) -> usize {
        self.map.values().unique().count()
    }

    /// Returns the number of unique values that this mapping can be applied to.
    pub fn domain_size(&self) -> usize {
        self.map.keys().unique().count()
    }

    /// Returns an Iterator over the assignments in this mapping.
    pub fn assignments(&self) -> impl Iterator<Item = (&X, &Y)> {
        self.map.iter()
    }

    /// Obtains an iterator over mutable references to the assignments. Here, only the
    /// right side (i.e. assigned values) are mutable.
    pub fn assignments_mut(&mut self) -> impl Iterator<Item = (&X, &mut Y)> {
        self.map.iter_mut()
    }
}

impl<A: Assignment> Extend<A> for Mapping<A::Left, A::Right> {
    fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T) {
        self.map
            .extend(iter.into_iter().map(|a| (a.left(), a.right())));
    }
}

/// A mapping from a type `X` to a [`Priority`].
#[autoimpl(for<T: trait> &T)]
pub trait Transformer {
    /// The domain is the set of values that the mapping can be applied to.
    type Domain: Value;

    /// The range is the set of values that the mapping can take.
    type Range: Value;

    /// Applies the mapping to the given input.
    fn apply<R: Borrow<Self::Domain>>(&self, input: R) -> Self::Range;
}

/// Type alias to help extract the input of something that implements [`Transformer`].
pub type TransformerInput<T> = <T as Transformer>::Domain;
/// Type alias to help extract the output of something that implements [`Transformer`].
pub type TransformerOutput<T> = <T as Transformer>::Range;

/// A mutable mapping from a type `X` to a [`Priority`].
pub trait MutableTransformer: Transformer {
    /// Sets the priority of the given element to the given priority.
    fn set_map<R: Borrow<Self::Domain>>(&mut self, of: R, to: Self::Range) -> Option<Self::Range>;
}

/// A reference to an [`Assignment`].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AssignmentReference<'a, X, Y> {
    left: &'a X,
    right: &'a Y,
}

impl<'a, X: Clone, Y: Clone> AssignmentReference<'a, X, Y> {
    /// Creates a new assignment reference.
    pub fn new((left, right): (&'a X, &'a Y)) -> Self {
        Self { left, right }
    }

    /// Gets the left part of the underlying assignment.
    pub fn left(&self) -> X {
        self.left.clone()
    }

    /// Gets the right part of the underlying assignment.
    pub fn right(&self) -> Y {
        self.right.clone()
    }

    /// Maps the right part of the assignment.
    pub fn map_right<Z: Value, F: Fn(&Y) -> Z>(self, f: F) -> (X, Z) {
        (self.left.clone(), f(self.right))
    }
}

/// A reference to an assignment, encoded in an arbitrary way.
pub trait Assignment {
    /// The left type, i.e. an element from the domain.
    type Left: Value;
    /// The right type is the element from the codomain.
    type Right: Value;

    /// Gets the domain element.
    fn left(&self) -> Self::Left;
    /// Gets the codomain element.
    fn right(&self) -> Self::Right;
}

impl<Q: StateIndex, S: Symbol, O: Value> Assignment for (Q, S, Q, O) {
    type Left = (Q, S);

    type Right = O;

    fn left(&self) -> Self::Left {
        (self.0.clone(), self.1.clone())
    }

    fn right(&self) -> Self::Right {
        self.3.clone()
    }
}

impl<L: Value, R: Value> Assignment for (L, R) {
    type Left = L;

    type Right = R;

    fn left(&self) -> Self::Left {
        self.0.clone()
    }

    fn right(&self) -> Self::Right {
        self.1.clone()
    }
}

impl<'a, L: Value, R: Value> Assignment for AssignmentReference<'a, L, R> {
    type Left = L;

    type Right = R;

    fn left(&self) -> Self::Left {
        self.left.clone()
    }

    fn right(&self) -> Self::Right {
        self.right.clone()
    }
}

/// Converter trait that allows conversion of a reference to a [`Mapping`] into an
/// [`Iterator`] over [`Assignment`]s.
pub trait IntoAssignments: Transformer + Copy {
    /// The type of the assignment reference, something like [`AssignmentReference`].
    type AssignmentRef: Assignment<Left = Self::Domain, Right = Self::Range>;

    /// The type of the iterator over the assignments.
    type Assignments: Iterator<Item = Self::AssignmentRef>;

    /// Converts the reference into an iterator over the individual assignments.
    fn into_assignments(self) -> Self::Assignments;

    fn collect_mapping(self) -> Mapping<Self::Domain, Self::Range> {
        self.into_assignments().collect()
    }
}

impl<'a, X, Y> IntoAssignments for &'a Mapping<X, Y>
where
    X: Value,
    Y: Value,
{
    type AssignmentRef = AssignmentReference<'a, X, Y>;
    type Assignments = std::iter::Map<
        std::collections::hash_map::Iter<'a, X, Y>,
        fn((&'a X, &'a Y)) -> Self::AssignmentRef,
    >;

    fn into_assignments(self) -> Self::Assignments {
        self.map.iter().map(AssignmentReference::new)
    }
}

impl<'a, W: IntoAssignments> IntoAssignments for &'a W {
    type AssignmentRef = W::AssignmentRef;

    type Assignments = W::Assignments;

    fn into_assignments(self) -> Self::Assignments {
        (*self).into_assignments()
    }
}

impl<I, O: Symbol> HasOutput for Map<I, O> {
    type Gamma = O;

    type Output<'me> = std::collections::hash_map::Values<'me, I, O> where Self:'me;

    fn output_alphabet_iter(&self) -> Self::Output<'_> {
        self.values()
    }
}

impl<X, Y> Transformer for Map<X, Y>
where
    X: Value,
    Y: Value,
{
    type Range = Y;
    type Domain = X;

    fn apply<R: Borrow<X>>(&self, input: R) -> Self::Range {
        self.get(input.borrow())
            .cloned()
            .expect("Mapping must be total")
    }
}

impl<X, Y> MutableTransformer for Map<X, Y>
where
    X: Value,
    Y: Value,
{
    fn set_map<R: Borrow<X>>(&mut self, of: R, to: Self::Range) -> Option<Self::Range> {
        self.insert(of.borrow().clone(), to)
    }
}

impl<X, Y> Transformer for Mapping<X, Y>
where
    X: Value,
    Y: Value,
{
    type Range = Y;

    type Domain = X;

    fn apply<R: Borrow<X>>(&self, input: R) -> Self::Range {
        self.map.apply(input)
    }
}

impl<X, Y> MutableTransformer for Mapping<X, Y>
where
    X: Value,
    Y: Value,
{
    fn set_map<R: Borrow<X>>(&mut self, of: R, to: Self::Range) -> Option<Self::Range> {
        self.map.set_map(of, to)
    }
}

impl<A: Assignment> FromIterator<A> for Mapping<A::Left, A::Right> {
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        Self {
            map: iter.into_iter().map(|a| (a.left(), a.right())).collect(),
        }
    }
}
