use itertools::Itertools;

use crate::{
    alphabet::{ExpressionOf, HasAlphabet, SymbolOf},
    ts::{
        predecessors::{IsPreTransition, PredecessorIterable},
        transition_system::IsTransition,
        IndexType,
    },
    Alphabet, Color, Pointed, TransitionSystem,
};

/// Trait that abstracts the product operation for transition systems.
pub trait Product: TransitionSystem + Sized {
    /// The type of output that is produced by the product operation.
    type Output<Ts: TransitionSystem<Alphabet = Self::Alphabet>>: TransitionSystem<
        Alphabet = Self::Alphabet,
        StateColor = (Self::StateColor, Ts::StateColor),
    >;
    /// Perform a product operation on two transition systems.
    fn ts_product<Ts: TransitionSystem<Alphabet = Self::Alphabet>>(
        self,
        other: Ts,
    ) -> Self::Output<Ts>;
}

impl<Lts: TransitionSystem + Sized> Product for Lts {
    type Output<Ts: TransitionSystem<Alphabet = Self::Alphabet>> = MatchingProduct<Self, Ts>;

    fn ts_product<Ts: TransitionSystem<Alphabet = Self::Alphabet>>(
        self,
        other: Ts,
    ) -> Self::Output<Ts> {
        MatchingProduct(self, other)
    }
}

/// Index type for product transition systems.
#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProductIndex<L, R>(pub L, pub R);

impl<L, R> From<ProductIndex<L, R>> for (L, R) {
    fn from(value: ProductIndex<L, R>) -> Self {
        (value.0, value.1)
    }
}

impl<L: std::fmt::Display, R: std::fmt::Display> std::fmt::Display for ProductIndex<L, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.0, self.1)
    }
}

/// A product of two transition systems, which only permits transitions on matching symbols, while
/// ignoring all others.
#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MatchingProduct<L, R>(pub L, pub R);

impl<L, R> Pointed for MatchingProduct<L, R>
where
    L: Pointed,
    R: Pointed,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
{
    fn initial(&self) -> Self::StateIndex {
        ProductIndex(self.0.initial(), self.1.initial())
    }
}

impl<L: HasAlphabet, R> HasAlphabet for MatchingProduct<L, R> {
    type Alphabet = L::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.0.alphabet()
    }
}

/// Iterator over the state indices of a product transition system.
pub struct ProductStatesIter<'a, L: TransitionSystem, R: TransitionSystem> {
    left: L::StateIndices<'a>,
    right: R::StateIndices<'a>,
    left_state: Option<L::StateIndex>,
    lts: &'a L,
    rts: &'a R,
}

impl<'a, L: TransitionSystem, R: TransitionSystem> Iterator for ProductStatesIter<'a, L, R> {
    type Item = ProductIndex<L::StateIndex, R::StateIndex>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.left_state {
            None => None,
            Some(p) => {
                if let Some(q) = self.right.next() {
                    Some(ProductIndex(p, q))
                } else {
                    self.left_state = self.left.next();
                    self.right = self.rts.state_indices();
                    self.next()
                }
            }
        }
    }
}

impl<'a, L: TransitionSystem, R: TransitionSystem> ProductStatesIter<'a, L, R> {
    /// Create a new iterator over the state indices of a product transition system.
    pub fn new(lts: &'a L, rts: &'a R) -> Self {
        let mut lit = lts.state_indices();
        Self {
            left_state: lit.next(),
            left: lit,
            right: rts.state_indices(),
            rts,
            lts,
        }
    }
}

/// Type that encapsulates a transition in a product transition system.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProductTransition<LI, RI, E, LC, RC> {
    expression: E,
    target: ProductIndex<LI, RI>,
    color: (LC, RC),
}

#[allow(missing_docs)]
impl<LI, RI, E, LC, RC> ProductTransition<LI, RI, E, LC, RC> {
    pub fn new(expression: E, target: ProductIndex<LI, RI>, color: (LC, RC)) -> Self {
        Self {
            expression,
            target,
            color,
        }
    }
}

impl<Idx, Jdx, E, C, D> IsTransition<E, ProductIndex<Idx, Jdx>, (C, D)>
    for ProductTransition<Idx, Jdx, E, C, D>
where
    Idx: IndexType,
    Jdx: IndexType,
    C: Color,
    D: Color,
{
    fn target(&self) -> ProductIndex<Idx, Jdx> {
        self.target
    }

    fn color(&self) -> (C, D) {
        self.color.clone()
    }

    fn expression(&self) -> &E {
        &self.expression
    }
}

/// Iterator over the edges of a product transition system.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProductEdgesFrom<'a, L: TransitionSystem, R: TransitionSystem> {
    left: &'a L,
    right: &'a R,
    cur: Option<L::TransitionRef<'a>>,
    it: L::EdgesFromIter<'a>,
    right_edges: Vec<R::TransitionRef<'a>>,
    position: usize,
}

#[allow(missing_docs)]
impl<'a, L: TransitionSystem, R: TransitionSystem> ProductEdgesFrom<'a, L, R> {
    pub fn new(
        left: &'a L,
        right: &'a R,
        index: ProductIndex<L::StateIndex, R::StateIndex>,
    ) -> Option<Self> {
        let mut it = left.edges_from(index.0)?;
        Some(Self {
            left,
            right_edges: right.edges_from(index.1)?.collect(),
            right,
            position: 0,
            cur: it.next(),
            it,
        })
    }
}

impl<'a, L, R> Iterator for ProductEdgesFrom<'a, L, R>
where
    L: TransitionSystem,
    R: TransitionSystem,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
{
    type Item = ProductTransition<
        L::StateIndex,
        R::StateIndex,
        ExpressionOf<L>,
        L::EdgeColor,
        R::EdgeColor,
    >;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position >= self.right_edges.len() {
            self.position = 0;
            self.cur = self.it.next();
        }
        match &self.cur {
            None => return None,
            Some(l) => {
                while let Some(r) = self.right_edges.get(self.position) {
                    self.position += 1;
                    if l.expression() == r.expression() {
                        return Some(ProductTransition::new(
                            l.expression().clone(),
                            ProductIndex(l.target(), r.target()),
                            (l.color(), r.color()),
                        ));
                    }
                }
            }
        }
        self.next()
    }
}

/// Analogous to [`ProductTransition`]; encapsulates a pre-transition in a product transition system.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProductPreTransition<LI, RI, E, LC, RC> {
    source: ProductIndex<LI, RI>,
    expression: E,
    color: (LC, RC),
}

#[allow(missing_docs)]
impl<LI, RI, E, LC, RC> ProductPreTransition<LI, RI, E, LC, RC> {
    pub fn new(source: ProductIndex<LI, RI>, expression: E, color: (LC, RC)) -> Self {
        Self {
            source,
            expression,
            color,
        }
    }
}

impl<Idx, Jdx, E, C, D> IsPreTransition<ProductIndex<Idx, Jdx>, E, (C, D)>
    for ProductPreTransition<Idx, Jdx, E, C, D>
where
    Idx: IndexType,
    Jdx: IndexType,
    C: Color,
    D: Color,
{
    fn source(&self) -> ProductIndex<Idx, Jdx> {
        self.source
    }

    fn color(&self) -> (C, D) {
        self.color.clone()
    }

    fn expression(&self) -> &E {
        &self.expression
    }
}

/// Iterator over the predecessors of a product transition system.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProductEdgesTo<'a, L: PredecessorIterable, R: PredecessorIterable> {
    left: &'a L,
    right: &'a R,
    cur: Option<L::PreTransitionRef<'a>>,
    it: L::EdgesToIter<'a>,
    right_edges: Vec<R::PreTransitionRef<'a>>,
    position: usize,
}

impl<'a, L, R> Iterator for ProductEdgesTo<'a, L, R>
where
    L: PredecessorIterable,
    R: PredecessorIterable,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
{
    type Item = ProductPreTransition<
        L::StateIndex,
        R::StateIndex,
        ExpressionOf<L>,
        L::EdgeColor,
        R::EdgeColor,
    >;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position >= self.right_edges.len() {
            self.position = 0;
            self.cur = self.it.next();
        }
        match &self.cur {
            None => return None,
            Some(left) => {
                while let Some(right) = self.right_edges.get(self.position) {
                    self.position += 1;
                    if left.expression() == right.expression() {
                        return Some(ProductPreTransition::new(
                            ProductIndex(left.source(), right.source()),
                            left.expression().clone(),
                            (left.color(), right.color()),
                        ));
                    }
                }
            }
        }
        self.next()
    }
}

#[allow(missing_docs)]
impl<'a, L: PredecessorIterable, R: PredecessorIterable> ProductEdgesTo<'a, L, R> {
    pub fn new(
        left: &'a L,
        right: &'a R,
        index: ProductIndex<L::StateIndex, R::StateIndex>,
    ) -> Option<Self> {
        let mut it = left.predecessors(index.0)?;
        Some(Self {
            left,
            right,
            cur: it.next(),
            it,
            right_edges: right.predecessors(index.1)?.collect(),
            position: 0,
        })
    }
}
