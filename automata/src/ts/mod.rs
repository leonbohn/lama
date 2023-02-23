use crate::Alphabet;

pub mod labels;
pub mod transition;

/// An implementation of a deterministic `TransitionSystem` in form of an edge list.
#[cfg(feature = "det")]
pub mod deterministic;

/// Implemented by objects which have a designated initial state.
pub trait Pointed<Q> {
    /// Get the initial state of the automaton.
    fn initial(&self) -> Q;
}

pub trait StateIndex: Clone + Eq + std::hash::Hash + std::fmt::Debug {}

impl<X: Clone + Eq + std::hash::Hash + std::fmt::Debug> StateIndex for X {}

pub trait TransitionTarget {
    type Output;
    fn is_deterministic() -> bool;
}
pub trait Transition {
    type Sym;
    type Id;
    fn from(&self) -> &Self::Id;
    fn to(&self) -> &Self::Id;
    fn symbol(&self) -> &Self::Sym;
}

impl<T: Transition> Transition for &T {
    type Sym = T::Sym;
    type Id = T::Id;
    fn from(&self) -> &Self::Id {
        (*self).from()
    }
    fn to(&self) -> &Self::Id {
        (*self).to()
    }
    fn symbol(&self) -> &Self::Sym {
        (*self).symbol()
    }
}

pub type SymbolFor<X> = <<X as TransitionSystem>::S as Alphabet>::C;
pub type OutputOf<X> = <X as TransitionSystem>::Q;

pub trait TransitionSystem {
    type Q: StateIndex;
    type S: Alphabet;
    type Transition: Transition<Sym = SymbolFor<Self>, Id = Self::Q>;

    fn succ(&self, from: &Self::Q, on: &SymbolFor<Self>) -> Option<OutputOf<Self>>;
}

pub trait FiniteState: TransitionSystem {
    fn states(&self) -> Vec<Self::Q>;
    fn size(&self) -> usize;
}

pub trait IntoStateReferences<'a>: TransitionSystem + 'a {
    type Output: Iterator<Item = &'a Self::Q>;
    fn into_state_references(self) -> Self::Output;
}

pub trait Growable: TransitionSystem {
    /// Add a new state to the transition system..
    fn add_state(&mut self) -> Self::Q;

    /// Add a new transition to the transition system. If the transition did not exist before, `None` is returned. Otherwise, the old target state is returned.
    fn add_transition<X: AsRef<SymbolFor<Self>>>(
        &mut self,
        from: Self::Q,
        on: SymbolFor<Self>,
        to: Self::Q,
    ) -> Option<Self::Q>;
}

pub trait Shrinkable: TransitionSystem {
    fn remove_state(&mut self, state: Self::Q) -> Option<Self::Q>;
    fn remove_transition<X: AsRef<SymbolFor<Self>>>(
        &mut self,
        from: Self::Q,
        on: SymbolFor<Self>,
    ) -> Option<Self::Q>;
}
