use super::{SymbolFor, TransitionSystem};

/// An implementor has labels of type `C` associated with its states. could be used for example for state-based acceptance in parity automata or for some other types of automata.
pub trait StateLabeled: TransitionSystem {
    type Label;
    /// Returns the state label.
    fn state_label(&self, of: &Self::Q) -> Option<&Self::Label>;
}

/// Implementors have colors or labels associated with some [`Transition`]s.
pub trait TransitionLabeled: TransitionSystem {
    type Label;
    fn label(&self, q: &Self::Q, a: &SymbolFor<Self>) -> Option<&Self::Label>;
}
