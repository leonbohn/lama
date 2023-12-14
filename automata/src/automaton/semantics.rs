use crate::{
    ts::{path::Lasso, Deterministic},
    Color, Set,
};

pub trait FiniteSemantics<Q> {
    fn finite_classify(&self, color: &Q) -> bool;
}

pub trait OmegaSemantics<Q, C> {
    fn omega_classify<P: ProvidesInfinitySets<Q, C>>(&self, infset: P) -> bool;
}

pub trait ProvidesInfinitySets<Q, C> {
    fn recurrent_edges(&self) -> impl Iterator<Item = C>;
    fn recurrent_states(&self) -> impl Iterator<Item = Q>;
}

impl<D: Deterministic> ProvidesInfinitySets<D::StateColor, D::EdgeColor>
    for (D, Lasso<D::Alphabet, D::StateIndex>)
{
    fn recurrent_edges(&self) -> impl Iterator<Item = D::EdgeColor> {
        self.1.recurrent_edge_colors(&self.0)
    }

    fn recurrent_states(&self) -> impl Iterator<Item = D::StateColor> {
        self.1.recurrent_state_colors(&self.0)
    }
}

pub struct ReachSingleColor<Q>(Q);
impl<Q: Color> FiniteSemantics<Q> for ReachSingleColor<Q> {
    fn finite_classify(&self, color: &Q) -> bool {
        self.0.eq(color)
    }
}

pub struct ReachColors<Q>(Set<Q>);
impl<Q: Color> FiniteSemantics<Q> for ReachColors<Q> {
    fn finite_classify(&self, color: &Q) -> bool {
        self.0.contains(color)
    }
}
