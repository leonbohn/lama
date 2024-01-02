use std::{
    collections::BTreeSet,
    fmt::{Debug, Display},
    marker::PhantomData,
};

use impl_tools::autoimpl;
use itertools::Itertools;
use owo_colors::OwoColorize;

use crate::{
    alphabet::{Alphabet, Symbol},
    prelude::Simple,
    prelude::*,
    ts::{
        finite::{InfinityColors, ReachedColor},
        operations::{MapStateColor, MatchingProduct, Product, ProductIndex, ProductTransition},
        transition_system::IsEdge,
        EdgeColor, HasMutableStates, HasStates, IndexType, Pointed, Quotient, Sproutable,
        StateColor, SymbolOf, TransitionSystem, BTS,
    },
    word::{OmegaWord, Reduced},
    Color, FiniteLength, InfiniteLength, Length,
};

mod semantics;

#[macro_use]
mod moore;
pub use moore::{AsMooreMachine, IntoMooreMachine, MooreLike, MooreMachine};

#[macro_use]
mod mealy;
pub use mealy::{AsMealyMachine, IntoMealyMachine, MealyLike, MealyMachine};

mod dfa;
pub use dfa::{DFALike, IntoDFA, DFA};

mod dpa;
pub use dpa::{DPALike, IntoDPA, DPA};

mod dba;
pub use dba::{DBALike, IntoDBA, DBA};

mod omega;
pub use omega::{
    AcceptanceMask, DeterministicOmegaAutomaton, OmegaAcceptanceCondition, OmegaAutomaton,
};

mod acceptor;
pub use acceptor::{
    FiniteWordAcceptor, FiniteWordTransformer, OmegaWordAcceptor, OmegaWordTransformer,
};

mod with_initial;
pub use with_initial::WithInitial;

/// Type alias for the unit type. Purpose is mainly to be used in Macros, as `()` is more difficult to
/// handle than a simple alphabetic identifier.
pub type NoColor = ();

#[allow(missing_docs)]
macro_rules! impl_automaton_type {
    ($name:ident, onstates $color:ident) => {
        impl_automaton_type!($name, $color, NoColor);
    };
    ($name:ident, onedges $color:ident) => {
        impl_automaton_type!($name, NoColor, $color);
    };
    ($name:ident, $color:ident, $edgecolor:ident) => {
        paste::paste! {
            /// Type alias to allow conversion of a transition system into an automaton of the type.
            pub type [< Into $name >]<Ts> = $name<<Ts as TransitionSystem>::Alphabet, <Ts as TransitionSystem>::StateColor, <Ts as TransitionSystem>::EdgeColor, Ts>;
        }

        #[allow(missing_docs)]
        #[derive(Clone)]
        pub struct $name<
            A = Simple,
            Q = $color,
            C = $edgecolor,
            Ts = WithInitial<DTS<A, Q, C>>,
        > {
            ts: Ts,
            _alphabet: PhantomData<(A, Q, C)>,
        }
        impl<A: Alphabet>
            $name<A, $color, $edgecolor, WithInitial<DTS<A, $color, $edgecolor>>>
        {
            /// Creates a new automaton from a given alphabet.
            pub fn new(alphabet: A) -> $name<A, $color, $edgecolor, WithInitial<DTS<A, $color, $edgecolor>>> {
                $name {
                    ts: WithInitial::new(alphabet),
                    _alphabet: PhantomData,
                }
            }
        }
        impl<Ts: TransitionSystem> $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts> {
            /// Returns a reference to the underlying transition system.
            pub fn ts(&self) -> &Ts {
                &self.ts
            }
            /// Returns a mutable reference to the underlying transition system.
            pub fn ts_mut(&mut self) -> &mut Ts {
                &mut self.ts
            }
        }
        impl<Ts: TransitionSystem<StateColor = $color>> From<Ts>
            for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
        {
            fn from(ts: Ts) -> Self {
                Self {
                    ts,
                    _alphabet: PhantomData,
                }
            }
        }
        impl<Ts: PredecessorIterable> PredecessorIterable
            for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
        {
            type PreTransitionRef<'this> = Ts::PreTransitionRef<'this> where Self: 'this;
            type EdgesToIter<'this> = Ts::EdgesToIter<'this> where Self: 'this;
            fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
                self.ts().predecessors(state)
            }
        }
        impl<Ts: Pointed> std::fmt::Debug for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use itertools::Itertools;
                use crate::prelude::IsEdge;
                writeln!(
                    f,
                    "Initial state {} with sdtates {} and transitions\n{}",
                    self.ts().initial(),
                    self.ts().state_indices().map(|i| format!("{i}")).join(", "),
                    self.ts().state_indices().map(|i| self.edges_from(i).unwrap().map(|e| format!("{} --{:?}--> {}", i, e.expression(), e.target())).join("\n")).join("\n")
                )
            }
        }
        impl<Ts: Sproutable> Sproutable
            for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
        {
            type ExtendStateIndexIter = Ts::ExtendStateIndexIter;
            fn extend_states<I: IntoIterator<Item = StateColor<Self>>>(
                &mut self,
                iter: I,
            ) -> Self::ExtendStateIndexIter {
                self.ts_mut().extend_states(iter)
            }
            fn set_state_color<X: Into<StateColor<Self>>>(
                &mut self,
                index: Self::StateIndex,
                color: X,
            ) {
                self.ts_mut().set_state_color(index, color)
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
            fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
                Self {
                    ts: Ts::new_for_alphabet(alphabet),
                    _alphabet: PhantomData,
                }
            }
            fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
                self.ts_mut().add_state(color)
            }
            fn remove_edges(
                &mut self,
                from: Self::StateIndex,
                on: <Self::Alphabet as Alphabet>::Expression,
            ) -> bool {
                self.ts_mut().remove_edges(from, on)
            }
        }
        impl<Ts: TransitionSystem> TransitionSystem
            for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts>
        {
            type Alphabet = Ts::Alphabet;
            fn alphabet(&self) -> &Self::Alphabet {
                self.ts().alphabet()
            }
            type StateIndex = Ts::StateIndex;
            type EdgeColor = Ts::EdgeColor;
            type StateColor = Ts::StateColor;
            type TransitionRef<'this> = Ts::TransitionRef<'this> where Self: 'this;
            type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
            type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;
            fn state_indices(&self) -> Self::StateIndices<'_> {
                self.ts().state_indices()
            }

            fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
                self.ts().state_color(state)
            }

            fn edges_from<Idx: $crate::prelude::Indexes<Self>>(
                &self,
                state: Idx,
            ) -> Option<Self::EdgesFromIter<'_>> {
                self.ts().edges_from(state.to_index(self)?)
            }

            fn maybe_initial_state(&self) -> Option<Self::StateIndex> {
                self.ts().maybe_initial_state()
            }
        }

        impl<Ts: Deterministic> Deterministic for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts> {
            fn transition<Idx: $crate::prelude::Indexes<Self>>(
                &self,
                state: Idx,
                symbol: SymbolOf<Self>,
            ) -> Option<Self::TransitionRef<'_>> {
                self.ts().transition(state.to_index(self)?, symbol)
            }

            fn edge_color(
                &self,
                state: Self::StateIndex,
                expression: &ExpressionOf<Self>,
            ) -> Option<EdgeColor<Self>> {
                self.ts().edge_color(state, expression)
            }
        }
        impl<Ts: Pointed> Pointed for $name<Ts::Alphabet, Ts::StateColor, Ts::EdgeColor, Ts> {
            fn initial(&self) -> Self::StateIndex {
                self.ts().initial()
            }
        }
    };
}

#[allow(missing_docs)]
// impl_automaton_type!(DFA, onstates bool);
// impl_automaton_type!(MooreMachine, onstates usize);
impl_automaton_type!(StateBasedDBA, onstates bool);
impl_automaton_type!(StateBasedDPA, onstates usize);

/// Iterator over the accepting states of a [`TransitionSystem`] that have a certain coloring.
pub struct StatesWithColor<'a, Ts: TransitionSystem> {
    ts: &'a Ts,
    iter: Ts::StateIndices<'a>,
    color: Ts::StateColor,
}

impl<'a, Ts: TransitionSystem> StatesWithColor<'a, Ts> {
    /// Creates a new instance for the given transition system and color.
    pub fn new(ts: &'a Ts, color: Ts::StateColor) -> Self {
        Self {
            iter: ts.state_indices(),
            ts,
            color,
        }
    }
}

impl<'a, Ts: TransitionSystem> Clone for StatesWithColor<'a, Ts> {
    fn clone(&self) -> Self {
        Self {
            ts: self.ts,
            iter: self.ts.state_indices(),
            color: self.color.clone(),
        }
    }
}

impl<'a, Ts: TransitionSystem<StateColor = bool>> Iterator for StatesWithColor<'a, Ts> {
    type Item = Ts::StateIndex;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .find(|&index| self.ts.state_color(index).unwrap() == self.color)
    }
}

#[cfg(test)]
mod tests {
    use super::{DFALike, MooreLike, WithInitial};
    use crate::{
        automaton::{
            acceptor::{FiniteWordAcceptor, OmegaWordAcceptor},
            NoColor,
        },
        prelude::*,
        ts::BTS,
    };
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn mealy_color_or_below() {
        let mut mm = MooreMachine::new(alphabet!(simple 'a', 'b'), 2);
        let a = mm.initial();
        let b = mm.add_state(1usize);
        let c = mm.add_state(1usize);
        let d = mm.add_state(0usize);
        mm.add_edge(a, 'a', b, ());
        mm.add_edge(a, 'b', c, ());
        mm.add_edge(b, 'a', c, ());
        mm.add_edge(b, 'b', c, ());
        mm.add_edge(c, 'a', d, ());
        mm.add_edge(c, 'b', c, ());
        mm.add_edge(d, 'a', d, ());
        mm.add_edge(d, 'b', d, ());

        let dfas = mm.decompose_dfa();
        let dfa2 = &dfas[2];
        let dfa0 = &dfas[0];

        println!("{:?}", dfa0);
        assert!(dfa2.accepts_finite(""));
        assert!(dfa2.accepts_finite("b"));
        assert!(!dfa0.accepts_finite("b"));
        assert!(dfa0.accepts_finite("ba"));
    }

    #[test]
    fn dbas() {
        let mut dba = super::DBA::new(Simple::from_iter(['a', 'b']), ());
        let q1 = dba.add_state(());
        let q0 = dba.initial();

        let _e0 = dba.add_edge(q0, 'a', q1, true);
        let _e1 = dba.add_edge(q0, 'b', q0, false);
        let _e2 = dba.add_edge(q1, 'a', q1, true);
        let _e3 = dba.add_edge(q1, 'b', q0, false);
        assert!(dba.accepts_omega(Reduced::periodic("abb")));
        assert!(!dba.accepts_omega(Reduced::periodic("b")));
        assert!(dba.accepts_omega(upw!("a")));
        assert!(!dba.accepts_omega(upw!("b")));

        assert!(!dba.dba_is_empty());
        println!("{:?}", dba.dba_give_word());

        println!("{:?}", &dba);
    }

    #[test]
    fn dfas_and_boolean_operations() {
        let mut dfa = super::DFA::new(Simple::new(['a', 'b']));
        let s0 = dfa.initial();
        dfa.set_initial_color(true);
        let s1 = dfa.add_state(false);
        let _e0 = dfa.add_edge(s0, 'a', s1, ());
        let _e1 = dfa.add_edge(s0, 'b', s0, ());
        let _e2 = dfa.add_edge(s1, 'a', s1, ());
        let _e3 = dfa.add_edge(s1, 'b', s0, ());

        assert!(!dfa.dfa_is_empty());
        assert_eq!(dfa.dfa_give_word(), Some(vec![]));

        let _dfb = dfa.clone();

        assert!(dfa.accepts_finite("ababab"));
        assert!(!dfa.accepts_finite("a"));

        let notdfa = (&dfa).negation().into_dfa();
        assert!(!notdfa.accepts_finite("ababab"));
        assert!(notdfa.accepts_finite("a"));

        let intersection = (&dfa).intersection(&notdfa).into_dfa();
        assert!(!intersection.accepts_finite("ababab"));
        assert!(!intersection.accepts_finite("a"));

        let union = (&dfa).union(&notdfa).into_dfa();
        assert!(union.accepts_finite("ababab"));
        assert!(union.accepts_finite("a"));
    }
}
