/// Represents a deterministic transition system consisting of a finite set of states, which are linked to each other with transitions.
/// The states of a TS are indexed with the provided type `Id`.
/// Transitions are on input symbols of the type `Sym`.
pub trait Deterministic {
    type Id;
    type Sym;
    /// Returns the successor from `state` on the input `symbol`, if one exists. Otherwise `None` is returned.
    fn successor(&self, state: &Self::Id, symbol: &Self::Sym) -> Option<Self::Id>;

    /// Gives a vector containing all state indices.
    fn states(&self) -> Vec<Self::Id>;

    /// Returns true if and only if a state with that ID exists.
    fn has_state(&self, state: &Self::Id) -> bool;
}

/// A deterministic transition system which can be modified. See also [`Deterministic`].
pub trait MutableDeterministic: Deterministic {
    /// Insert a new state and return its ID.
    fn add_state(&mut self) -> Self::Id;

    /// Delete the state with given `id`.
    /// Returns `id` if the deletion was successful and `None` if no state with the `id` exists.
    fn remove_state(&mut self, id: &Self::Id) -> Option<&Self::Id>;

    fn set_successor(
        &mut self,
        state: &Self::Id,
        symbol: &Self::Sym,
        target: &Self::Id,
    ) -> Option<&Self::Id>;

    fn remove_successor(&mut self, state: &Self::Id, symbol: &Self::Sym) -> Option<&Self::Id>;
}
