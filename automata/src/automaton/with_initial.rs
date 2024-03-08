use owo_colors::OwoColorize;

use crate::prelude::*;
use std::fmt::Debug;

/// Wrapper around a [`TransitionSystem`] with a designated initial state.
#[derive(Clone, PartialEq)]
pub struct Initialized<Ts: TransitionSystem>(Ts, Ts::StateIndex);

impl<Ts: TransitionSystem> Initialized<Ts> {
    /// Gives a reference to the underlying transition system.
    pub fn ts(&self) -> &Ts {
        &self.0
    }

    /// Consumes and decomposes self into a tuple of the underlying transition system and the initial state.
    pub fn into_parts(self) -> (Ts, Ts::StateIndex) {
        (self.0, self.1)
    }

    /// Returns a mutable reference to the underlying transition system.
    pub fn ts_mut(&mut self) -> &mut Ts {
        &mut self.0
    }
}

impl<Ts: TransitionSystem> Pointed for Initialized<Ts> {
    fn initial(&self) -> Self::StateIndex {
        assert!(!self.0.is_empty());
        self.1
    }
}

impl<Ts> std::fmt::Debug for Initialized<Ts>
where
    Ts: Deterministic + Debug,
    Ts::StateColor: Debug,
    Ts::EdgeColor: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Initialized to state {} with table\n{:?}",
            self.initial(),
            self.ts()
        )
    }
}

impl<Ts: TransitionSystem> From<(Ts, Ts::StateIndex)> for Initialized<Ts> {
    fn from(value: (Ts, Ts::StateIndex)) -> Self {
        Self(value.0, value.1)
    }
}

impl<A, C, Q> Initialized<DTS<A, Q, C>>
where
    A: Alphabet,
    C: Clone,
    Q: Clone,
{
    /// Takes an alphabet and a color and constructs an [`Initialized`] instance with the given alphabet, no
    /// transitions and a single initial state with the given color.
    pub fn with_initial_color(alphabet: A, color: Q) -> Self {
        let mut ts = DTS::new_for_alphabet(alphabet);
        let initial = ts.add_state(color);
        Self(ts, initial)
    }

    /// Creates a new instance for the given `alphabet`.
    pub fn new(alphabet: A) -> Self
    where
        StateColor<Self>: Default,
    {
        Self::with_initial_color(alphabet, Q::default())
    }

    /// Creats a new instance for the given `alphabet` and ensures capacity for at least `size` states.
    pub fn with_capacity(alphabet: A, size: usize) -> Self
    where
        StateColor<Self>: Default,
    {
        let mut ts = DTS::with_capacity(alphabet, size);
        let initial = ts.add_state(<StateColor<Self> as Default>::default());
        Self(ts, initial)
    }
}

impl<Ts: TransitionSystem + Sproutable> Sproutable for Initialized<Ts> {
    fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
        let mut ts = Ts::new_for_alphabet(alphabet);
        Self(ts, <Ts::StateIndex as IndexType>::first())
    }
    fn add_edge<X, Y, CI>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: CI,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Indexes<Self>,
        Y: Indexes<Self>,
        CI: Into<EdgeColor<Self>>,
    {
        let from = from.to_index(self)?;
        let to = to.to_index(self)?;
        self.ts_mut().add_edge(from, on, to, color.into())
    }
    fn remove_edges<X>(&mut self, from: X, on: <Self::Alphabet as Alphabet>::Expression) -> bool
    where
        X: Indexes<Self>,
    {
        from.to_index(self)
            .map(|idx| self.ts_mut().remove_edges(idx, on))
            .unwrap_or(false)
    }

    fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
        self.ts_mut().add_state(color)
    }

    fn set_state_color<Idx: Indexes<Self>, X: Into<StateColor<Self>>>(
        &mut self,
        index: Idx,
        color: X,
    ) {
        let Some(index) = index.to_index(self) else {
            tracing::error!("cannot set color of state that does not exist");
            return;
        };
        self.ts_mut().set_state_color(index, color)
    }

    type ExtendStateIndexIter = Ts::ExtendStateIndexIter;
    fn extend_states<I: IntoIterator<Item = StateColor<Self>>>(
        &mut self,
        iter: I,
    ) -> Self::ExtendStateIndexIter {
        self.ts_mut().extend_states(iter)
    }
}
