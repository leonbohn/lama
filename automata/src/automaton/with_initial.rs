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
    Ts::StateColor: Show,
    Ts::EdgeColor: Show,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Initial state {} with table\n{}",
            self.initial(),
            self.build_transition_table(|index, color| {
                if index == self.initial() {
                    format!("{} : {}", index.to_string().bold(), color.show())
                } else {
                    format!("{} : {}", index, color.show())
                }
            })
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

    fn add_edge<X, Y>(
        &mut self,
        from: X,
        on: <Self::Alphabet as Alphabet>::Expression,
        to: Y,
        color: EdgeColor<Self>,
    ) -> Option<(Self::StateIndex, Self::EdgeColor)>
    where
        X: Into<Self::StateIndex>,
        Y: Into<Self::StateIndex>,
    {
        self.ts_mut().add_edge(from, on, to, color)
    }

    fn remove_edges(
        &mut self,
        from: Self::StateIndex,
        on: <Self::Alphabet as Alphabet>::Expression,
    ) -> bool {
        self.ts_mut().remove_edges(from, on)
    }

    fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
        self.ts_mut().add_state(color)
    }

    fn set_state_color<X: Into<StateColor<Self>>>(&mut self, index: Self::StateIndex, color: X) {
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
impl<Ts: TransitionSystem + HasStates> HasStates for Initialized<Ts> {
    type State<'this> = Ts::State<'this>
        where
            Self: 'this;

    type StatesIter<'this> = Ts::StatesIter<'this>
        where
            Self: 'this;

    fn state(&self, index: Self::StateIndex) -> Option<Self::State<'_>> {
        self.ts().state(index)
    }

    fn states_iter(&self) -> Self::StatesIter<'_> {
        self.ts().states_iter()
    }
}
impl<Ts: TransitionSystem + HasMutableStates> HasMutableStates for Initialized<Ts> {
    type StateMut<'this>  = Ts::StateMut<'this> where Self:'this;

    fn state_mut(&mut self, index: Self::StateIndex) -> Option<Self::StateMut<'_>> {
        self.ts_mut().state_mut(index)
    }
}
