use itertools::Itertools;

use crate::{
    alphabet::{ExpressionOf, HasAlphabet, SymbolOf},
    ts::{
        predecessors::{IsPreTransition, PredecessorIterable},
        transition_system::IsTransition,
        FiniteState, FiniteStatesIterType, HasFiniteStates, IndexType,
    },
    Alphabet, Color, Pointed, TransitionSystem,
};

pub trait Product: TransitionSystem + Sized {
    type Output<Ts: TransitionSystem<Alphabet = Self::Alphabet>>: TransitionSystem<
        Alphabet = Self::Alphabet,
        StateColor = (Self::StateColor, Ts::StateColor),
    >;
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

impl<'a, L, R> HasFiniteStates<'a> for MatchingProduct<L, R>
where
    L: HasFiniteStates<'a>,
    R: HasFiniteStates<'a>,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
    L::StateColor: Clone,
    R::StateColor: Clone,
    R::StateIndicesIter: Clone,
{
    type StateIndicesIter = std::iter::Map<
        itertools::Product<L::StateIndicesIter, R::StateIndicesIter>,
        fn((L::StateIndex, R::StateIndex)) -> ProductIndex<L::StateIndex, R::StateIndex>,
    >;
}

impl<L, R> FiniteState for MatchingProduct<L, R>
where
    L: FiniteState,
    R: FiniteState,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
    L::StateColor: Clone,
    R::StateColor: Clone,
{
    fn state_indices(&self) -> FiniteStatesIterType<'_, Self> {
        self.0
            .state_indices()
            .cartesian_product(self.1.state_indices())
            .map(|(l, r)| ProductIndex(l, r))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProductTransition<LI, RI, E, LC, RC> {
    expression: E,
    target: ProductIndex<LI, RI>,
    color: (LC, RC),
}

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProductEdgesFrom<'a, L: TransitionSystem, R: TransitionSystem> {
    left: &'a L,
    right: &'a R,
    cur: Option<L::TransitionRef<'a>>,
    it: L::EdgesFromIter<'a>,
    right_edges: Vec<R::TransitionRef<'a>>,
    position: usize,
}

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

pub struct ProductPreTransition<LI, RI, E, LC, RC> {
    source: ProductIndex<LI, RI>,
    expression: E,
    color: (LC, RC),
}

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
