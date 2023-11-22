use crate::{
    alphabet::{ExpressionOf, HasAlphabet, SymbolOf},
    automata::WithInitial,
    congruence::ColoredClass,
    prelude::{Expression, Simple, Symbol},
    word::{FiniteWord, OmegaWord},
    Alphabet, Class, Color, FiniteLength, Map, Partition, Pointed, RightCongruence, Set, Show,
};

use super::{
    connected_components::{tarjan_scc, SccDecomposition, TarjanDAG},
    index_ts::BTState,
    operations::{
        MapEdgeColor, MapStateColor, MappedEdgesFromIter, MappedTransition, MatchingProduct,
        ProductEdgesFrom, ProductIndex, ProductStatesIter, ProductTransition, RestrictByStateIndex,
        RestrictedEdgesFromIter, StateIndexFilter, SubsetConstruction,
    },
    predecessors::PredecessorIterable,
    reachable::{MinimalRepresentatives, ReachableStateIndices, ReachableStates},
    run::successful::Successful,
    Deterministic, Quotient,
};

use super::{
    finite::{ReachedColor, ReachedState},
    path::Lasso,
    CanInduce, EdgeColor, IndexType, Induced, Path, StateColor, BTS,
};

use impl_tools::autoimpl;
use itertools::Itertools;

/// Trait that helps with accessing states in more elaborate [`TransitionSystem`]s. For
/// example in a [`crate::RightCongruence`], we have more information than the [`Color`]
/// on a state, we have its [`Class`] as well. Since we would like to be able to
/// access a state of a congruence not only by its index, but also by its classname
/// or any other [`Word`] of finite length, this trait is necessary.
///
/// Implementors should be able to _uniquely_ identify a single state in a transition
/// system of type `Ts`.
pub trait Indexes<Ts: TransitionSystem> {
    /// _Uniquely_ identifies a state in `ts` and return its index. If the state does
    /// not exist, `None` is returned.
    fn to_index(&self, ts: &Ts) -> Option<Ts::StateIndex>;
}

impl<Ts: TransitionSystem> Indexes<Ts> for Ts::StateIndex {
    #[inline(always)]
    fn to_index(&self, ts: &Ts) -> Option<<Ts as TransitionSystem>::StateIndex> {
        Some(*self)
    }
}
/// This trait is implemented for references to transitions, so that they can be used in
/// generic contexts. It is automatically implemented for (mutable) references.
#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait IsTransition<E, Idx, C> {
    /// Returns the target state of the transition.
    fn target(&self) -> Idx;
    /// Returns the color of the transition.
    fn color(&self) -> C;
    /// Gives a reference to the expression that labels the transition.
    fn expression(&self) -> &E;
    /// Destructures the transition into its components.
    fn into_tuple(self) -> (E, Idx, C)
    where
        E: Clone,
        Self: Sized,
    {
        (self.expression().clone(), self.target(), self.color())
    }
    /// Destructures `self` but into a slightly different form.
    fn into_nested_tuple(self) -> (E, (Idx, C))
    where
        E: Clone,
        Self: Sized,
    {
        (self.expression().clone(), (self.target(), self.color()))
    }
}

impl<'a, Idx: IndexType, E, C: Color> IsTransition<E, Idx, C> for (&'a E, &'a (Idx, C)) {
    fn target(&self) -> Idx {
        self.1 .0
    }

    fn color(&self) -> C {
        self.1 .1.clone()
    }

    fn expression(&self) -> &E {
        self.0
    }
}
impl<'a, Idx: IndexType, E, C: Color> IsTransition<E, Idx, C> for (&'a E, Idx, &'a C) {
    fn target(&self) -> Idx {
        self.1
    }

    fn color(&self) -> C {
        self.2.clone()
    }

    fn expression(&self) -> &E {
        self.0
    }
}

impl<'a, Idx: IndexType, E, C: Color> IsTransition<E, Idx, C> for (E, Idx, C) {
    fn target(&self) -> Idx {
        self.1
    }

    fn color(&self) -> C {
        self.2.clone()
    }

    fn expression(&self) -> &E {
        &self.0
    }
}

/// Type alias to extract the state color of a [`TransitionSystem`].
pub type StateColorOf<Ts> = <Ts as TransitionSystem>::StateColor;
/// Type alias to extract the edge color of a [`TransitionSystem`].
pub type EdgeColorOf<Ts> = <Ts as TransitionSystem>::EdgeColor;

/// Encapsulates the transition function δ of a (finite) transition system. This is the main trait that
/// is used to query a transition system. Transitions are labeled with a [`Alphabet::Expression`], which
/// determines on which [`Alphabet::Symbol`]s the transition can be taken. Additionally, every transition
/// is labeled with a [`Color`], which can be used to store additional information about it, like an
/// associated priority.
///
/// # The difference between transitions and edges
/// Internally, a transition system is represented as a graph, where the states are the nodes and the
/// transitions are the edges. However, the transitions are not the same as the edges.
/// Both store the source and target vertex as well as the color, however an edge is labelled
/// with an expression, while a transition is labelled with an actual symbol (that [`Alphabet::matches`]
/// the expression). So a transition is a concrete edge that is taken (usually by the run on a word), while
/// an edge may represent any different number of transitions.
pub trait TransitionSystem: HasAlphabet + Sized {
    /// The type of the indices of the states of the transition system.
    type StateIndex: IndexType;
    /// The type of the colors of the states of the transition system.
    type StateColor: Color;
    /// The type of the colors of the edges of the transition system.
    type EdgeColor: Color;
    /// The type of the references to the transitions of the transition system.
    type TransitionRef<'this>: IsTransition<ExpressionOf<Self>, Self::StateIndex, EdgeColor<Self>>
    where
        Self: 'this;
    /// The type of the iterator over the transitions that start in a given state.
    type EdgesFromIter<'this>: Iterator<Item = Self::TransitionRef<'this>>
    where
        Self: 'this;
    /// Type of the iterator over the state indices.
    type StateIndices<'this>: Iterator<Item = Self::StateIndex>
    where
        Self: 'this;

    /// Returns an iterator over the state indices of `self`.
    fn state_indices(&self) -> Self::StateIndices<'_>;

    /// Returns an iterator over the transitions that start in the given `state`. If the state does
    /// not exist, `None` is returned.
    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>>;

    fn has_transition(
        &self,
        source: Self::StateIndex,
        sym: SymbolOf<Self>,
        target: Self::StateIndex,
    ) -> bool {
        self.transitions_from(source)
            .find(|(p, a, _, q)| p == &source && q == &target && a == &sym)
            .is_some()
    }

    fn transitions_from<Idx: Indexes<Self>>(&self, state: Idx) -> TransitionsFrom<'_, Self> {
        TransitionsFrom::new(
            self,
            state
                .to_index(self)
                .expect("Should only be called for states that exist!"),
        )
    }

    fn subset_construction_from<I: IntoIterator<Item = Self::StateIndex>>(
        self,
        states: I,
    ) -> SubsetConstruction<Self> {
        SubsetConstruction::new(self, states)
    }

    fn subset_construction(self) -> SubsetConstruction<Self>
    where
        Self: Pointed,
    {
        let initial = self.initial();
        SubsetConstruction::new_from(self, initial)
    }

    /// Returns the color of the given `state`, if it exists. Otherwise, `None` is returned.
    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor>;

    /// Attempts to find a word which leads from the state `from` to state `to`. If no such
    /// word exists, `None` is returned.
    fn word_from_to(
        &self,
        from: Self::StateIndex,
        to: Self::StateIndex,
    ) -> Option<Vec<SymbolOf<Self>>>
    where
        Self: Sized,
    {
        self.minimal_representatives_from(from)
            .find_map(|(word, state)| if state == to { Some(word) } else { None })
    }

    /// Gives the size of `self`, which is obtained simply by counting the number of elements yielded by [`Self::states()`].
    fn size(&self) -> usize {
        self.state_indices().count()
    }

    /// Returns true if and only if the given state `index` exists.
    fn contains_state_index(&self, index: Self::StateIndex) -> bool {
        self.state_indices().contains(&index)
    }

    /// Tries to find the index of a state with the given `color`. Note that this uses `find` and thus
    /// returns the first such state that is found. There is no guarantee on the order in which the states
    /// are visited such that if more than one state with the given `color` exists, subsequent calls to
    /// this method may return different indices.
    fn find_by_color(&self, color: &StateColor<Self>) -> Option<Self::StateIndex> {
        self.state_indices()
            .find(|index| self.state_color(*index).as_ref() == Some(color))
    }

    /// Returns true if and only if a state with the given `color` exists.
    fn contains_state_color(&self, color: &StateColor<Self>) -> bool {
        self.find_by_color(color).is_some()
    }

    /// Obtains the [`Self::StateIndex`] of a state if it can be found. See [`Indexes`]
    /// for more.
    fn get<I: Indexes<Self>>(&self, elem: I) -> Option<Self::StateIndex>
    where
        Self: Sized,
    {
        elem.to_index(self)
    }

    /// Returns a [`WithInitial`] wrapper around `self`, which designates the given `initial` state.
    /// Note that this function does not (yet) ensure that the index actually exists!
    // FIXME: Ensure that the index actually exists.
    fn with_initial(self, initial: Self::StateIndex) -> WithInitial<Self>
    where
        Self: Sized,
    {
        (self, initial).into()
    }

    /// Builds the [`Quotient`] of `self` with regard to some given [`Partition`].
    fn quotient(self, partition: Partition<Self::StateIndex>) -> Quotient<Self>
    where
        Self: Sized,
    {
        Quotient::new(self, partition)
    }

    /// Restricts the state indices with the given function. This means that only the states for
    /// which the function returns `true` are kept, while all others are removed.
    fn restrict_state_indices<F>(self, filter: F) -> RestrictByStateIndex<Self, F>
    where
        Self: Sized,
        F: StateIndexFilter<Self::StateIndex>,
    {
        RestrictByStateIndex::new(self, filter)
    }

    /// Recolors the edges of `self` with the given function `f`. This works akin to
    /// [`Self::map_edge_colors()`] but allows for a more fine-grained control over the
    /// recoloring process, by giving access not only to the color itself, but also to
    /// the origin, target and expression of the respective edge.
    fn map_edge_colors_full<D, F>(self, f: F) -> super::operations::MapEdges<Self, F>
    where
        F: Fn(Self::StateIndex, &ExpressionOf<Self>, Self::EdgeColor, Self::StateIndex) -> D,
        D: Color,
        Self: Sized,
    {
        super::operations::MapEdges::new(self, f)
    }

    /// Completely removes the edge coloring.
    fn erase_edge_colors(self) -> MapEdgeColor<Self, fn(Self::EdgeColor) -> ()>
    where
        Self: Sized,
    {
        self.map_edge_colors(|_| ())
    }

    /// Completely removes the state coloring.
    fn erase_state_colors(self) -> MapStateColor<Self, fn(Self::StateColor) -> ()>
    where
        Self: Sized,
    {
        self.map_state_colors(|_| ())
    }

    /// Map the edge colors of `self` with the given function `f`.
    fn map_edge_colors<D: Color, F: Fn(Self::EdgeColor) -> D>(self, f: F) -> MapEdgeColor<Self, F>
    where
        Self: Sized,
    {
        MapEdgeColor::new(self, f)
    }

    /// Map the state colors of `self` with the given function.
    fn map_state_colors<D: Color, F: Fn(Self::StateColor) -> D>(
        self,
        f: F,
    ) -> MapStateColor<Self, F>
    where
        Self: Sized,
    {
        MapStateColor::new(self, f)
    }

    /// Turns `self` into a DFA that accepts all words, i.e. all states are accepting.
    fn all_accepting_dfa(self) -> MapStateColor<Self, fn(Self::StateColor) -> bool>
    where
        Self: Sized,
    {
        self.map_state_colors(|_| true)
    }

    /// Obtains the [`SccDecomposition`] of self, which is a partition of the states into strongly
    /// connected components. Uses Tarjan's algorithm.
    fn sccs(&self) -> SccDecomposition<'_, Self>
    where
        Self: Sized,
    {
        tarjan_scc(self)
    }

    /// Obtains the [`TarjanDAG`] of self, which is a directed acyclic graph that represents the
    /// strongly connected components of the transition system and the edges between them.
    fn tarjan_dag(&self) -> TarjanDAG<'_, Self>
    where
        Self: Sized + Clone,
    {
        TarjanDAG::from(tarjan_scc(self))
    }

    /// Returns `true` iff the given state is reachable from the initial state.
    fn is_reachable(&self, state: Self::StateIndex) -> bool
    where
        Self: Sized + Pointed,
    {
        self.is_reachable_from(self.initial(), state)
    }

    /// Returns `true` iff the given `state` is reachable from the given `origin` state.
    fn is_reachable_from(&self, origin: Self::StateIndex, state: Self::StateIndex) -> bool
    where
        Self: Sized + Pointed,
    {
        self.reachable_state_indices_from(origin)
            .any(|s| s == state)
    }

    /// Returns an iterator over the minimal representative (i.e. length-lexicographically minimal
    /// word reaching the state) of each state that is reachable from the initial state.
    fn minimal_representatives(&self) -> MinimalRepresentatives<&Self>
    where
        Self: Sized + Pointed,
    {
        MinimalRepresentatives::new(self, self.initial())
    }

    /// Returns an iterator over the indices of the states that are reachable from the initial state.
    fn reachable_states(&self) -> ReachableStates<&Self>
    where
        Self: Sized + Pointed,
    {
        ReachableStates::new(self, self.initial())
    }

    /// Returns an iterator over the minimal representatives (i.e. length-lexicographically minimal
    /// word reaching the state) of each state that is reachable from the given `state`.
    fn minimal_representatives_from<I: Into<Self::StateIndex>>(
        &self,
        state: I,
    ) -> MinimalRepresentatives<&Self>
    where
        Self: Sized,
    {
        MinimalRepresentatives::new(self, state.into())
    }

    /// Returns an iterator over the indices of the states that are reachable from the initial state.
    fn reachable_state_indices(&self) -> ReachableStateIndices<&Self>
    where
        Self: Sized + Pointed,
    {
        self.reachable_state_indices_from(self.initial())
    }

    /// Returns an iterator over the indices of the states that are reachable from the given `state`.
    fn reachable_state_indices_from<I: Into<Self::StateIndex>>(
        &self,
        state: I,
    ) -> ReachableStateIndices<&Self>
    where
        Self: Sized,
    {
        ReachableStateIndices::new(self, state.into())
    }

    /// Returns an iterator over the states that are reachable from the given `state`.
    fn reachable_states_from<I: Indexes<Self>>(&self, state: I) -> ReachableStates<&Self>
    where
        Self: Sized,
    {
        ReachableStates::new(self, state.to_index(self).unwrap())
    }

    /// Returns an option containing the index of the initial state, if it exists.
    /// This is a somewhat hacky way of dealing with the fact that we cannot express
    /// negative trait bounds. In particular, we cannot express that a transition system
    /// is not pointed, which prevents us from correctly implementing e.g. the `ToDot`
    /// trait for non-pointed transition systems. This function is a workaround for this
    /// problem, as it allows us to check whether a transition system is pointed or not,
    /// since the provided default implementation assumes that the no initial state exists.
    fn maybe_initial_state(&self) -> Option<Self::StateIndex> {
        None
    }
}

pub struct TransitionsFrom<'a, D: TransitionSystem + 'a> {
    edges: Option<D::EdgesFromIter<'a>>,
    target: Option<D::StateIndex>,
    color: Option<D::EdgeColor>,
    source: Option<D::StateIndex>,
    expression:
        Option<<<D::Alphabet as Alphabet>::Expression as Expression<SymbolOf<D>>>::SymbolsIter>,
}

impl<'a, D: TransitionSystem + 'a> Iterator for TransitionsFrom<'a, D> {
    type Item = (D::StateIndex, SymbolOf<D>, D::EdgeColor, D::StateIndex);
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(sym) = self.expression.as_mut().and_then(|mut it| it.next()) {
            Some((
                self.source.unwrap(),
                sym,
                self.color.clone().unwrap(),
                self.target.unwrap(),
            ))
        } else {
            if let Some(edge) = self.edges.as_mut().and_then(|mut it| it.next()) {
                self.expression = Some(edge.expression().symbols());
                self.target = Some(edge.target());
                self.color = Some(edge.color());
                self.next()
            } else {
                None
            }
        }
    }
}

impl<'a, D: TransitionSystem + 'a> TransitionsFrom<'a, D> {
    pub fn new(det: &'a D, state: D::StateIndex) -> Self {
        let Some(mut edges) = det.edges_from(state) else {
            return Self {
                edges: None,
                expression: None,
                color: None,
                target: None,
                source: Some(state),
            };
        };

        let Some(edge) = edges.next() else {
            return Self {
                edges: Some(edges),
                expression: None,
                color: None,
                source: Some(state),
                target: None,
            };
        };

        let target = Some(edge.target());
        let color = Some(edge.color());
        let expression = Some(edge.expression().symbols());
        Self {
            edges: Some(edges),
            target,
            color,
            expression,
            source: Some(state),
        }
    }
}

/// This macro implements the [`TransitionSystem`] trait for a given wrapper type. This is
/// especially useful for the automata definitions, since a DFA/DPA is simply a wrapper
/// around a transition system. We want to use it just like it was the underlying ts, but we
/// also want to encapsulate it in a new type to avoid ambiguity.
///
/// The macro assumes that the type for which the trait is implemented makes the functions
/// `ts()` and `ts_mut()` available, which return a reference/mutable reference to the underlying
/// transition system.
#[macro_export]
macro_rules! impl_ts_by_passthrough_on_wrapper {
    ($basetype: ident <Ts, $($typevar:ident : $type:ident),* > ) => {
        use $crate::prelude::*;
        impl<Ts: TransitionSystem, $($typevar : $type),*> HasAlphabet for $basetype<Ts::Alphabet, $($typevar),*, Ts> {
            type Alphabet = Ts::Alphabet;

            fn alphabet(&self) -> &Self::Alphabet {
                self.ts().alphabet()
            }
        }
        impl<Ts: TransitionSystem, $($typevar : $type),*> TransitionSystem for $basetype<Ts::Alphabet, $($typevar),*, Ts> {
            type StateIndex = Ts::StateIndex;
            type EdgeColor = Ts::EdgeColor;
            type StateColor = Ts::StateColor;
            type TransitionRef<'this> = Ts::TransitionRef<'this> where Self: 'this;
            type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
            type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

            fn state_indices(&self) -> Self::StateIndices<'_> {
                self.ts().state_indices()
            }

            fn transition<Idx: $crate::prelude::Indexes<Self>>(
                &self,
                state: Idx,
                symbol: SymbolOf<Self>,
            ) -> Option<Self::TransitionRef<'_>> {
                self.ts().transition(state.to_index(self)?, symbol)
            }

            fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
                self.ts().state_color(state)
            }

            fn edge_color(
                &self,
                state: Self::StateIndex,
                expression: &ExpressionOf<Self>,
            ) -> Option<EdgeColor<Self>> {
                self.ts().edge_color(state, expression)
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
    };
    ($basetype: ident < Ts >) => {
        impl<Ts: TransitionSystem> HasAlphabet for $basetype<Ts> {
            type Alphabet = Ts::Alphabet;
            fn alphabet(&self) -> &Self::Alphabet {
                self.ts().alphabet()
            }
        }
        impl<Ts: TransitionSystem> TransitionSystem for $basetype<Ts> {
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

            fn edges_from<Idx: Indexes<Self>>(
                &self,
                state: Idx,
            ) -> Option<Self::EdgesFromIter<'_>> {
                self.ts().edges_from(state.to_index(self)?)
            }

            fn maybe_initial_state(&self) -> Option<Self::StateIndex> {
                self.ts().maybe_initial_state()
            }
        }
        impl<D: Deterministic> Deterministic for $basetype<D> {
            fn transition<Idx: Indexes<Self>>(
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
    };
}

impl_ts_by_passthrough_on_wrapper!(WithInitial<Ts>);

impl<Ts: TransitionSystem> TransitionSystem for &Ts {
    type StateIndex = Ts::StateIndex;
    type EdgeColor = Ts::EdgeColor;
    type StateColor = Ts::StateColor;
    type TransitionRef<'this> = Ts::TransitionRef<'this> where Self: 'this;
    type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
    type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        Ts::state_indices(self)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        Ts::state_color(self, state)
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        Ts::edges_from(self, state.to_index(self)?)
    }

    fn maybe_initial_state(&self) -> Option<Self::StateIndex> {
        Ts::maybe_initial_state(self)
    }
}

impl<Ts: TransitionSystem> TransitionSystem for &mut Ts {
    type StateIndex = Ts::StateIndex;
    type EdgeColor = Ts::EdgeColor;
    type StateColor = Ts::StateColor;
    type TransitionRef<'this> = Ts::TransitionRef<'this> where Self : 'this;
    type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
    type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        Ts::state_indices(self)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        Ts::state_color(self, state)
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        Ts::edges_from(self, state.to_index(self)?)
    }

    fn maybe_initial_state(&self) -> Option<Self::StateIndex> {
        Ts::maybe_initial_state(self)
    }
}

impl<A: Alphabet, Q: Color, C: Color> TransitionSystem for RightCongruence<A, Q, C> {
    type StateIndex = usize;
    type EdgeColor = C;
    type StateColor = ColoredClass<A::Symbol, Q>;
    type TransitionRef<'this> = (&'this A::Expression, &'this (usize, C)) where Self: 'this;
    type EdgesFromIter<'this> = std::collections::hash_map::Iter<'this, A::Expression, (usize, C)>
    where
        Self: 'this;
    type StateIndices<'this> = std::iter::Cloned<std::collections::hash_map::Keys<'this, usize, BTState<A, ColoredClass<A::Symbol, Q>, C, usize>>> where Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts().state_indices()
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        self.ts().state_color(state)
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        self.ts().edges_from(state.to_index(self)?)
    }

    fn maybe_initial_state(&self) -> Option<Self::StateIndex> {
        Some(self.initial())
    }
}
impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> TransitionSystem for BTS<A, Q, C, Idx> {
    type StateColor = Q;
    type EdgeColor = C;
    type StateIndex = Idx;
    type TransitionRef<'this> = (&'this A::Expression, &'this (Idx, C)) where Self: 'this;
    type EdgesFromIter<'this> = std::collections::hash_map::Iter<'this, A::Expression, (Idx, C)>
    where
        Self: 'this;
    type StateIndices<'this> = std::iter::Cloned<std::collections::hash_map::Keys<'this, Idx, super::index_ts::BTState<A, Q, C, Idx>>> where Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.states.keys().cloned()
    }

    fn state_color(&self, index: Idx) -> Option<StateColor<Self>> {
        self.raw_state_map().get(&index).map(|s| s.color().clone())
    }

    fn edges_from<X: Indexes<Self>>(&self, state: X) -> Option<Self::EdgesFromIter<'_>> {
        self.raw_state_map()
            .get(&state.to_index(self)?)
            .map(|o| o.edge_map().iter())
    }
}

impl<L, R> TransitionSystem for MatchingProduct<L, R>
where
    L: TransitionSystem,
    R: TransitionSystem,
    R::Alphabet: Alphabet<Symbol = SymbolOf<L>, Expression = ExpressionOf<L>>,
    L::StateColor: Clone,
    R::StateColor: Clone,
{
    type StateIndex = ProductIndex<L::StateIndex, R::StateIndex>;
    type EdgeColor = (L::EdgeColor, R::EdgeColor);
    type StateColor = (L::StateColor, R::StateColor);
    type TransitionRef<'this> = ProductTransition<L::StateIndex, R::StateIndex, ExpressionOf<L>, L::EdgeColor, R::EdgeColor> where Self: 'this;
    type EdgesFromIter<'this> = ProductEdgesFrom<'this, L, R> where Self: 'this;
    type StateIndices<'this> = ProductStatesIter<'this, L, R> where Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        ProductStatesIter::new(&self.0, &self.1)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        let ProductIndex(l, r) = state;
        let left = self.0.state_color(l)?;
        let right = self.1.state_color(r)?;
        Some((left, right))
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        ProductEdgesFrom::new(&self.0, &self.1, state.to_index(self)?)
    }

    fn maybe_initial_state(&self) -> Option<Self::StateIndex> {
        Some(ProductIndex(
            self.0.maybe_initial_state()?,
            self.1.maybe_initial_state()?,
        ))
    }
}

impl<D, Ts, F> TransitionSystem for MapStateColor<Ts, F>
where
    D: Color,
    Ts: TransitionSystem,
    F: Fn(Ts::StateColor) -> D,
{
    type StateIndex = Ts::StateIndex;
    type EdgeColor = Ts::EdgeColor;
    type StateColor = D;
    type TransitionRef<'this> = Ts::TransitionRef<'this> where Self: 'this;
    type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
    type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts().state_indices()
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        let color = self.ts().state_color(state)?;
        Some((self.f())(color))
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        self.ts().edges_from(state.to_index(self)?)
    }

    fn maybe_initial_state(&self) -> Option<Self::StateIndex> {
        self.ts().maybe_initial_state()
    }
}

impl<D, Ts, F> TransitionSystem for MapEdgeColor<Ts, F>
where
    D: Color,
    Ts: TransitionSystem,
    F: Fn(Ts::EdgeColor) -> D,
{
    type StateIndex = Ts::StateIndex;
    type EdgeColor = D;
    type StateColor = Ts::StateColor;
    type TransitionRef<'this> = MappedTransition<Ts::TransitionRef<'this>, &'this F, Ts::EdgeColor> where Self: 'this;
    type EdgesFromIter<'this> =
        MappedEdgesFromIter<'this, Ts::EdgesFromIter<'this>, F, Ts::EdgeColor> where Self: 'this;

    type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts().state_indices()
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        self.ts().state_color(state)
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        Some(MappedEdgesFromIter::new(
            self.ts().edges_from(state.to_index(self)?)?,
            self.f(),
        ))
    }

    fn maybe_initial_state(&self) -> Option<Self::StateIndex> {
        self.ts().maybe_initial_state()
    }
}

impl<Ts: TransitionSystem, F> TransitionSystem for RestrictByStateIndex<Ts, F>
where
    F: StateIndexFilter<Ts::StateIndex>,
{
    type StateIndex = Ts::StateIndex;
    type EdgeColor = Ts::EdgeColor;
    type StateColor = Ts::StateColor;
    type TransitionRef<'this> = Ts::TransitionRef<'this> where Self: 'this;
    type EdgesFromIter<'this> = RestrictedEdgesFromIter<'this, Ts, F> where Self: 'this;
    type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.ts().state_indices()
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        assert!((self.filter()).is_unmasked(state));
        self.ts().state_color(state)
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        if !(self.filter()).is_unmasked(state.to_index(self)?) {
            return None;
        }
        self.ts()
            .edges_from(state.to_index(self)?)
            .map(|iter| RestrictedEdgesFromIter::new(iter, self.filter()))
    }

    fn maybe_initial_state(&self) -> Option<Self::StateIndex> {
        let initial = self.ts().maybe_initial_state()?;
        if self.filter().is_unmasked(initial) {
            Some(initial)
        } else {
            None
        }
    }
}

pub struct DeterministicEdgesFrom<'a, Ts: TransitionSystem> {
    ts: &'a Ts,
    state: Ts::StateIndex,
    symbols: <Ts::Alphabet as Alphabet>::Universe<'a>,
}

impl<'a, Ts: Deterministic> Iterator for DeterministicEdgesFrom<'a, Ts> {
    type Item = Ts::TransitionRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.symbols
            .next()
            .and_then(|sym| self.ts.transition(self.state, *sym))
    }
}

impl<'a, Ts: TransitionSystem> DeterministicEdgesFrom<'a, Ts> {
    pub fn new(ts: &'a Ts, state: Ts::StateIndex) -> Self {
        Self {
            ts,
            state,
            symbols: ts.alphabet().universe(),
        }
    }
}
#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use super::TransitionSystem;
    use crate::{
        alphabet,
        tests::wiki_dfa,
        ts::{
            finite::{ReachedColor, ReachedState},
            index_ts::MealyTS,
            Deterministic, Sproutable,
        },
        FiniteLength,
    };

    #[test]
    fn transitions_from() {
        let dfa = wiki_dfa();
        assert_eq!(dfa.transitions_from(0).count(), 2);
    }

    #[test]
    #[traced_test]
    fn run() {
        let mut ts = MealyTS::new(alphabet::Simple::from_iter(['a', 'b']));
        let s0 = ts.add_state(());
        let s1 = ts.add_state(());
        let _e0 = ts.add_edge(s0, 'a', s1, 0);
        let _e1 = ts.add_edge(s0, 'b', s0, 1);
        let _e2 = ts.add_edge(s1, 'a', s1, 0);
        let _e3 = ts.add_edge(s1, 'b', s0, 1);

        let ts = ts.with_initial(s0);
        assert!(ts.finite_run("abba").is_ok());
        assert_eq!(ts.reached_state_index("ab"), Some(s0));
        assert_eq!(ts.reached_state_index("bbb"), Some(s0));
    }
}
