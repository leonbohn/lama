use crate::{Alphabet, Set};

use super::{
    deterministic::{FiniteRunResult, OmegaRunResult},
    path::Lasso,
    Congruence, EdgeColor, IndexType, Path, StateColor,
};

/// This module deals with partial runs.
pub mod partial;
/// Module for dealing with completed and successful runs.
pub mod successful;
// /// Module that defines a walker, which can perform stepwise computations on a transition system.
// pub mod walker;

pub trait FiniteRun {
    type StateColor;
    type EdgeColor;
    type StateIndex: IndexType;
    fn state_colors(self) -> Option<impl Iterator<Item = Self::StateColor>>;
    fn edge_colors(self) -> Option<impl Iterator<Item = Self::EdgeColor>>;
    fn indices(self) -> Option<impl Iterator<Item = Self::StateIndex>>;
    fn successful(&self) -> bool;
}

impl<A: Alphabet, Q: Clone, C: Clone, Idx: IndexType> FiniteRun for FiniteRunResult<A, Idx, Q, C> {
    type StateColor = Q;
    type EdgeColor = C;
    type StateIndex = Idx;

    fn state_colors(self) -> Option<impl Iterator<Item = Self::StateColor>> {
        self.ok().map(|run| run.into_state_colors())
    }

    fn edge_colors(self) -> Option<impl Iterator<Item = Self::EdgeColor>> {
        self.ok().map(|run| run.into_edge_colors())
    }

    fn indices(self) -> Option<impl Iterator<Item = Self::StateIndex>> {
        self.ok().map(|run| run.into_state_sequence())
    }

    fn successful(&self) -> bool {
        match self {
            Ok(_) => true,
            _ => false,
        }
    }
}

pub trait OmegaRun {
    type StateColor;
    type EdgeColor;
    type StateIndex: IndexType;
    fn infinity_state_colors(self) -> Option<impl Iterator<Item = Self::StateColor>>;
    fn infinity_edge_colors(self) -> Option<impl Iterator<Item = Self::EdgeColor>>;
    fn infinity_indices(self) -> Option<impl Iterator<Item = Self::StateIndex>>;
}

impl<A: Alphabet, Q: Clone, C: Clone, Idx: IndexType> OmegaRun for OmegaRunResult<A, Idx, Q, C> {
    type StateColor = Q;

    type EdgeColor = C;

    type StateIndex = Idx;

    fn infinity_state_colors(self) -> Option<impl Iterator<Item = Self::StateColor>> {
        self.ok().map(|path| path.into_recurrent_state_colors())
    }

    fn infinity_edge_colors(self) -> Option<impl Iterator<Item = Self::EdgeColor>> {
        self.ok().map(|path| path.into_recurrent_edge_colors())
    }

    fn infinity_indices(self) -> Option<impl Iterator<Item = Self::StateIndex>> {
        self.ok().map(|path| path.into_recurrent_state_indices())
    }
}
