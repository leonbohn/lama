#![allow(missing_docs)]
use itertools::Itertools;
use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use crate::{prelude::*, Void};

use super::{Automaton, MooreLike};

/// Represents the semantics of a Mealy machine. Concretely, this type returns for
/// a finite run, the last transition color that is taken. It panics if the run has
/// no transitions at all.
#[derive(Debug, Clone, Default)]
pub struct MealySemantics<C>(PhantomData<C>);

/// A Mealy machine is a transition system where each transition has an output. Thus, the output
/// of running a Mealy machine on a word produces a sequence of outputs, one for each transition
/// that is taken. Note that since the empty word does not take any transitions, it does not
/// produce any output. For a word of length `n`, there are `n` outputs.
///
/// Usually, we are interested in the output of the last state that is reached during a run
/// on a word. In case of a deterministic Mealy machine, this is the only output that is
/// produced.
pub type MealyMachine<A = CharAlphabet, C = usize> =
    Automaton<Initialized<DTS<A, Void, C>>, MealySemantics<C>, false>;

/// Helper type that takes a pointed transition system and returns the corresponding
/// [`MealyMachine`].
pub type IntoMealyMachine<D> = Automaton<D, MealySemantics<EdgeColor<D>>, false>;

impl<Ts: Congruence> IntoMealyMachine<Ts>
where
    EdgeColor<Ts>: Color,
{
    pub fn minimize(&self) -> MealyMachine<Ts::Alphabet, EdgeColor<Ts>> {
        crate::algorithms::mealy_partition_refinement(self)
    }

    pub fn restricted_inequivalence<
        O: MealyLike<Alphabet = Ts::Alphabet, EdgeColor = Ts::EdgeColor>,
    >(
        &self,
        other: &IntoMealyMachine<O>,
    ) -> Option<Vec<SymbolOf<Ts>>> {
        let prod = self.ts_product(other);
        for (mut rep, ProductIndex(l, r)) in prod.minimal_representatives() {
            'edges: for edge in self.edges_from(l).unwrap() {
                let Some(sym) = edge.expression().symbols().next() else {
                    continue 'edges;
                };
                let mut it = other.edges_from(r).unwrap();

                match other.transition(r, sym) {
                    Some(e) => {
                        if edge.color() != e.color() {
                            rep.push(sym);
                            return Some(rep);
                        }
                    }
                    None => {
                        rep.push(sym);
                        return Some(rep);
                    }
                }
            }
        }
        None
    }

    pub fn witness_inequivalence<
        O: MealyLike<Alphabet = Ts::Alphabet, EdgeColor = Ts::EdgeColor>,
    >(
        &self,
        other: &IntoMealyMachine<O>,
    ) -> Option<Vec<SymbolOf<Ts>>> {
        self.restricted_inequivalence(other)
            .or(other.restricted_inequivalence(self))
    }
}

#[allow(missing_docs)]
macro_rules! impl_mealy_automaton {
    ($name:ident, $color:ident) => {
        paste::paste! {
            /// Type alias for considering `Ts` as an automaton.
            pub type [< Into $name >]<Ts> = $name<<Ts as TransitionSystem>::Alphabet, <Ts as TransitionSystem>::StateColor, Ts>;
        }

        #[allow(missing_docs)]
        #[derive(Clone)]
        pub struct $name<
            A = CharAlphabet,
            Q = crate::Void,
            Ts = Initialized<DTS<A, Q, $color>>,
        > {
            ts: Ts,
            _alphabet: std::marker::PhantomData<(A, Q, $color)>,
        }
        impl<A: Alphabet, Q: Color + Default>
            $name<A, Q, Initialized<DTS<A, Q, $color>>>
        {
            #[allow(missing_docs)]
            pub fn new(alphabet: A, initial_state_color: Q) -> $name<A, Q, Initialized<DTS<A, Q, $color>>> {
                let mut ts = Initialized::new(alphabet);
                ts.set_initial_color(initial_state_color);
                $name {
                    ts,
                    _alphabet: std::marker::PhantomData,
                }
            }
        }
        #[allow(missing_docs)]
        impl<Ts: TransitionSystem> $name<Ts::Alphabet, Ts::StateColor, Ts> {
            pub fn ts(&self) -> &Ts {
                &self.ts
            }
            pub fn ts_mut(&mut self) -> &mut Ts {
                &mut self.ts
            }
        }
        impl<Ts: TransitionSystem<EdgeColor = $color>> From<Ts>
            for $name<Ts::Alphabet,  Ts::StateColor, Ts>
        {
            fn from(ts: Ts) -> Self {
                Self {
                    ts,
                    _alphabet: std::marker::PhantomData,
                }
            }
        }
        impl<Ts: PredecessorIterable> PredecessorIterable
            for $name<Ts::Alphabet, Ts::StateColor, Ts>
        {
            type PreEdgeRef<'this> = Ts::PreEdgeRef<'this> where Self: 'this;
            type EdgesToIter<'this> = Ts::EdgesToIter<'this> where Self: 'this;
            fn predecessors(&self, state: Self::StateIndex) -> Option<Self::EdgesToIter<'_>> {
                self.ts().predecessors(state)
            }
        }
        impl<Ts: Pointed> std::fmt::Debug for $name<Ts::Alphabet,  Ts::StateColor, Ts> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use itertools::Itertools;
                use crate::prelude::IsEdge;
                writeln!(
                    f,
                    "Initial state {} with states {} and transitions\n{}",
                    self.ts().initial(),
                    self.ts().state_indices().map(|i| format!("{i}")).join(", "),
                    self.ts().state_indices().map(|i| self.edges_from(i).unwrap().map(|e| format!("{} --{:?}--> {}", i, e.expression(), e.target())).join("\n")).join("\n")
                )
            }
        }
        impl<Ts: Sproutable> Sproutable
            for $name<Ts::Alphabet, Ts::StateColor, Ts>
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
                X: Indexes<Self>,
                Y: Indexes<Self>,
            {
                let from = from.to_index(self)?;
                let to = to.to_index(self)?;
                self.ts_mut().add_edge(from, on, to, color)
            }
            fn remove_edges<X>(
                &mut self,
                from: X,
                on: <Self::Alphabet as Alphabet>::Expression,
            ) -> bool
            where
                X: Indexes<Self>
            {
                from.to_index(self)
                    .map(|idx| self.ts_mut().remove_edges(idx, on))
                    .unwrap_or(false)
            }
            fn new_for_alphabet(alphabet: Self::Alphabet) -> Self            {
                Self {
                    ts: Ts::new_for_alphabet(alphabet),
                    _alphabet: std::marker::PhantomData,
                }
            }
            fn add_state<X: Into<StateColor<Self>>>(&mut self, color: X) -> Self::StateIndex {
                self.ts_mut().add_state(color)
            }
        }
        impl<Ts: TransitionSystem> TransitionSystem
            for $name<Ts::Alphabet, Ts::StateColor, Ts>
        {
            type StateIndex = Ts::StateIndex;
            type EdgeColor = Ts::EdgeColor;
            type StateColor = Ts::StateColor;
            type EdgeRef<'this> = Ts::EdgeRef<'this> where Self: 'this;
            type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
            type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;
            type Alphabet = Ts::Alphabet;
            fn alphabet(&self) -> &Self::Alphabet {
                self.ts.alphabet()
            }
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
        impl<Ts: Pointed> Pointed for $name<Ts::Alphabet, Ts::StateColor, Ts> {
            fn initial(&self) -> Self::StateIndex {
                self.ts().initial()
            }
        }
        impl<Ts: Deterministic> Deterministic for $name<Ts::Alphabet, Ts::StateColor, Ts> {
            fn transition<Idx: $crate::prelude::Indexes<Self>>(
                &self,
                state: Idx,
                symbol: SymbolOf<Self>,
            ) -> Option<Self::EdgeRef<'_>> {
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
    };
}

/// Implemented by objects which can be viewed as a MealyMachine, i.e. a finite transition system
/// which has outputs of type usize on its edges.
pub trait MealyLike: Congruence {
    fn mealy_bisimilar<M>(&self, other: M) -> bool
    where
        M: Congruence,
        EdgeColor<M>: Color,
    {
        todo!()
    }

    /// Uses a reference to `self` for obtaining a [`MealyMachine`].
    fn as_mealy(&self) -> IntoMealyMachine<&Self>
    where
        EdgeColor<Self>: Color,
    {
        Automaton::from_parts(self, MealySemantics(PhantomData))
    }

    /// Self::EdgeColoronsumes `self`, returning a [`MealyMachine`] that uses the underlying transition system.
    fn into_mealy(self) -> IntoMealyMachine<Self>
    where
        EdgeColor<Self>: Color,
    {
        Automaton::from_parts(self, MealySemantics(PhantomData))
    }

    fn collect_mealy(self) -> MealyMachine<Self::Alphabet, Self::EdgeColor>
    where
        EdgeColor<Self>: Color,
    {
        self.erase_state_colors().collect_pointed().0.into_mealy()
    }

    /// Attempts to run the given finite word in `self`, returning the color of the last transition that
    /// is taken wrapped in `Some`. If no successful run on `input` is possible, the function returns `None`.
    fn try_mealy_map<W: FiniteWord<SymbolOf<Self>>>(&self, input: W) -> Option<Self::EdgeColor>
    where
        Self: Deterministic,
    {
        self.finite_run(input)
            .ok()
            .and_then(|r| r.last_transition_color().cloned())
    }

    /// Returns a vector over all colors that can be emitted.
    fn color_range(&self) -> impl Iterator<Item = Self::EdgeColor>
    where
        EdgeColor<Self>: Clone + Hash + Eq,
    {
        self.reachable_state_indices()
            .flat_map(|o| self.edges_from(o).unwrap().map(|e| IsEdge::color(&e)))
            .unique()
    }
}
impl<Ts: Congruence> MealyLike for Ts where EdgeColor<Ts>: Color {}

#[cfg(test)]
mod tests {
    use crate::{ts::NTS, TransitionSystem};

    use super::MealyLike;

    #[test]
    fn mealy_equivalence() {
        let mm1 = NTS::builder()
            .default_color(())
            .with_transitions([
                (0, 'a', 1, 0),
                (0, 'b', 0, 1),
                (1, 'a', 1, 0),
                (1, 'b', 0, 2),
                (2, 'a', 1, 0),
                (2, 'b', 0, 0),
            ])
            .deterministic()
            .with_initial(0)
            .into_mealy();
        let mm2 = NTS::builder()
            .default_color(())
            .with_transitions([
                (0, 'a', 1, 0),
                (0, 'b', 0, 1),
                (1, 'a', 1, 0),
                (1, 'b', 0, 2),
                (2, 'a', 1, 0),
                (2, 'b', 1, 0),
            ])
            .deterministic()
            .with_initial(0)
            .into_mealy();
        let mm3 = NTS::builder()
            .default_color(())
            .with_transitions([
                (0, 'a', 1, 0),
                (0, 'b', 0, 1),
                (1, 'a', 1, 0),
                (1, 'b', 0, 2),
                (2, 'a', 1, 0),
                (2, 'b', 0, 2),
            ])
            .deterministic()
            .with_initial(0)
            .into_mealy();

        assert_eq!(mm1.witness_inequivalence(&mm2), Some(vec!['b', 'b', 'b']))
    }
}
