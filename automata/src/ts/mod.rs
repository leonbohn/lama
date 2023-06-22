mod index;
pub use index::{Idx, Index, Indexes};

mod state;
pub use state::{State, StateIndex};

mod has_states;
pub use has_states::HasStates;

mod successor;
pub use successor::Successor;

mod edge;
pub use edge::{Edge, EdgeIndex};

mod transition;
pub use transition::Transition;

use crate::alphabet::HasAlphabet;

use self::edge::{EdgeIndicesFrom, EdgesFrom};

mod index_ts;
pub use index_ts::IndexTS;

mod path;
pub use path::Path;

/// Implementors of this trait have a distinguished (initial) state.
pub trait Pointed {
    /// Returns the index of the initial state.
    fn initial(&self) -> StateIndex;
}

pub trait TransitionSystem: HasStates + Successor + HasAlphabet {}
impl<Ts: HasStates + Successor + HasAlphabet> TransitionSystem for Ts {}

pub trait Congruence: TransitionSystem + Pointed {}
impl<Sim: TransitionSystem + Pointed> Congruence for Sim {}
