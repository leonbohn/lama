use std::any::Any;

use crate::{
    automaton::Initialized,
    congruence::ColoredClass,
    prelude::{Expression, Simple, Symbol},
    word::{FiniteWord, OmegaWord},
    Alphabet, Class, Color, FiniteLength, Map, Partition, Pointed, RightCongruence, Set, Show,
};

use super::{
    connected_components::{
        tarjan_scc_iterative, tarjan_scc_recursive, SccDecomposition, TarjanDAG,
    },
    index_ts::BTState,
    nts::{NTEdge, NTSEdgesFromIter},
    operations::{
        ColorRestricted, MapEdgeColor, MapStateColor, MappedEdgesFromIter, MappedTransition,
        MatchingProduct, ProductEdgesFrom, ProductIndex, ProductStatesIter, ProductTransition,
        RestrictByStateIndex, RestrictedEdgesFromIter, StateIndexFilter, SubsetConstruction,
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

/// Type alias to extract the state color of a [`TransitionSystem`].
pub type StateColorOf<Ts> = <Ts as TransitionSystem>::StateColor;
/// Type alias to extract the edge color of a [`TransitionSystem`].
pub type EdgeColorOf<Ts> = <Ts as TransitionSystem>::EdgeColor;

/// Helper trait for extracting the [`Symbol`] type from an a transition system.
pub type SymbolOf<A> = <<A as TransitionSystem>::Alphabet as Alphabet>::Symbol;
/// Helper trait for extracting the [`Expression`] type from an a transition system.
pub type ExpressionOf<A> = <<A as TransitionSystem>::Alphabet as Alphabet>::Expression;

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
pub trait TransitionSystem: Sized {
    /// The type of the underlying [`Alphabet`].
    type Alphabet: Alphabet;
    /// The type of the indices of the states of the transition system.
    type StateIndex: IndexType;
    /// The type of the colors of the states of the transition system.
    type StateColor: Color;
    /// The type of the colors of the edges of the transition system.
    type EdgeColor: Color;
    /// The type of the references to the transitions of the transition system.
    type EdgeRef<'this>: IsEdge<'this, ExpressionOf<Self>, Self::StateIndex, EdgeColor<Self>>
    where
        Self: 'this;
    /// The type of the iterator over the transitions that start in a given state.
    type EdgesFromIter<'this>: Iterator<Item = Self::EdgeRef<'this>>
    where
        Self: 'this;
    /// Type of the iterator over the state indices.
    type StateIndices<'this>: Iterator<Item = Self::StateIndex>
    where
        Self: 'this;

    /// Returns a reference to the alphabet of `self`.
    fn alphabet(&self) -> &Self::Alphabet;

    /// Calls the [`Alphabet::universe`] method on the alphabet of `self`, returning
    /// an iterator of all symbols.
    fn symbols(&self) -> <Self::Alphabet as Alphabet>::Universe<'_> {
        self.alphabet().universe()
    }

    /// Returns a vector of all state indices of `self`. By default, this is simply a helper
    /// calling to [`Self::state_indices`], but it can be overridden to provide a more
    /// efficient implementation.
    fn state_indices_vec(&self) -> Vec<Self::StateIndex> {
        self.state_indices().collect()
    }

    /// Returns an iterator over the state indices of `self`.
    fn state_indices(&self) -> Self::StateIndices<'_>;

    /// Returns true if the transition system has no states.
    fn is_stateless(&self) -> bool {
        self.state_indices().next().is_none()
    }

    /// Returns an iterator over pairs consisting of a state index and the corresponding state color.
    fn state_indices_with_color(
        &self,
    ) -> impl Iterator<Item = (Self::StateIndex, Self::StateColor)> {
        self.state_indices()
            .map(|q| (q, self.state_color(q).expect("Every state must be colored")))
    }

    /// Helper function which creates an expression from the given symbol.
    /// This is a convenience function that simply calls [`Alphabet::expression`].
    fn make_expression(&self, sym: SymbolOf<Self>) -> ExpressionOf<Self> {
        <Self::Alphabet as Alphabet>::expression(sym)
    }

    /// Gives an iterator over all transitions of `self`.
    fn transitions(&self) -> impl Iterator<Item = Self::EdgeRef<'_>> {
        self.state_indices().flat_map(move |q| {
            self.edges_from(q)
                .expect("should return iterator for state that exists")
        })
    }

    /// Returns true if the transition system has no edges. This is rather costly, as it
    /// simply iterates over all states and checks whether they have any outgoing edges.
    /// Should be overridden if a more efficient implementation is available.
    fn is_edgeless(&self) -> bool {
        self.state_indices().all(|q| {
            self.edges_from(q)
                .map(|mut it| it.next().is_none())
                .unwrap_or(true)
        })
    }

    /// Returns an iterator giving all colors that are used by the edges of `self`.
    /// Note that this **may output the same color multiple times**, if it is used by multiple
    /// edges. If that is not desired, use [`Self::edge_colors_unique()`] instead.
    ///
    /// A call is rather costly, as it simply iterates over all states and collects the
    /// colors of the outgoing edges. Should be overridden if a more efficient implementation
    /// is available.
    fn edge_colors(&self) -> impl Iterator<Item = Self::EdgeColor> {
        self.state_indices()
            .flat_map(|q| {
                self.edges_from(q)
                    .expect("should return iterator for state that exists")
            })
            .map(|t| t.color().clone())
    }

    /// Returns an iterator giving all **unique** colors that are used by the edges of `self`.
    /// By default, a call is rather costly as it simply iterates over all states and collects
    /// the colors of the outgoing edges. Should be overridden if a more efficient implementation
    /// is available.
    fn edge_colors_unique(&self) -> impl Iterator<Item = Self::EdgeColor> {
        self.edge_colors().unique()
    }

    /// Returns an iterator over the transitions that start in the given `state`. If the state does
    /// not exist, `None` is returned.
    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>>;

    /// Returns an iterator over all transitions that start in the given `state` and whose expression
    /// matches the given `sym`. If the state does not exist, `None` is returned.
    fn edges_matching<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        sym: SymbolOf<Self>,
    ) -> Option<
        impl Iterator<
            Item = (
                Self::StateIndex,
                &ExpressionOf<Self>,
                Self::EdgeColor,
                Self::StateIndex,
            ),
        >,
    > {
        let idx = state.to_index(self)?;
        Some(self.edges_from(state)?.filter_map(move |t| {
            if t.expression().matches(sym) {
                Some((idx, t.expression(), t.color().clone(), t.target()))
            } else {
                None
            }
        }))
    }

    /// Returns true if and only if there exists a transition from the given `source` state to the
    /// given `target` state, whose expression is matched by the given `sym`. If either the source
    /// or the target state does not exist, `false` is returned.
    fn has_transition(
        &self,
        source: Self::StateIndex,
        sym: SymbolOf<Self>,
        target: Self::StateIndex,
    ) -> bool {
        if let Some(mut it) = self.edges_from(source) {
            it.any(|t| t.target() == target && t.expression().matches(sym))
        } else {
            false
        }
    }

    /// Returns an iterator over the transitions that start in the given `state`. Panics if the
    /// state does not exist.
    fn transitions_from<Idx: Indexes<Self>>(&self, state: Idx) -> TransitionsFrom<'_, Self> {
        TransitionsFrom::new(
            self,
            state
                .to_index(self)
                .expect("Should only be called for states that exist!"),
        )
    }

    /// Commence a new subset construction starting from the collection of states given by `states`.
    /// This is a convenience function that simply calls [`SubsetConstruction::new`]. It produces a
    /// deterministic transition system operating on sets of states.
    fn subset_construction_from<I: IntoIterator<Item = Self::StateIndex>>(
        self,
        states: I,
    ) -> SubsetConstruction<Self> {
        SubsetConstruction::new(self, states)
    }

    /// Performs a subset construction using [`Self::subset_construction_from`] starting with a singleton
    /// set containing the only initial state of `self`.
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

    /// Gives the size of `self`, which is obtained simply by counting the number of elements yielded by [`Self::state_indices()`].
    fn size(&self) -> usize {
        self.state_indices().count()
    }

    /// Returns `true` if and only if there exists at least one state.
    fn is_empty(&self) -> bool {
        self.size() == 0
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

    /// Returns a [`Initialized`] wrapper around `self`, which designates the given `initial` state.
    /// Note that this function does not (yet) ensure that the index actually exists!
    fn with_initial(self, initial: Self::StateIndex) -> Initialized<Self>
    where
        Self: Sized,
    {
        assert!(
            self.contains_state_index(initial),
            "Cannot set initial state that does not exist"
        );
        (self, initial).into()
    }

    /// Builds the [`Quotient`] of `self` with regard to some given [`Partition`].
    fn quotient(self, partition: Partition<Self::StateIndex>) -> Quotient<Self>
    where
        Self: Sized,
    {
        Quotient::new(self, partition)
    }

    /// Restricts the edges of `self` such that only transitions p --a|c--> q exist where
    /// `min` <= `c` <= `max`, i.e. all transitions where either `c` < `min` or `max` < `c`
    /// are discarded.
    fn edge_color_restricted(
        self,
        min: Self::EdgeColor,
        max: Self::EdgeColor,
    ) -> ColorRestricted<Self> {
        ColorRestricted::new(self, min, max)
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
        tarjan_scc_iterative(self)
    }

    /// Obtains the [`SccDecomposition`] of self, which is a partition of the states into strongly
    /// connected components. Uses Tarjan's algorithm.
    fn sccs_recursive(&self) -> SccDecomposition<'_, Self>
    where
        Self: Sized,
    {
        tarjan_scc_recursive(self)
    }

    /// Obtains the [`TarjanDAG`] of self, which is a directed acyclic graph that represents the
    /// strongly connected components of the transition system and the edges between them.
    fn tarjan_dag(&self) -> TarjanDAG<'_, Self>
    where
        Self: Sized,
    {
        TarjanDAG::from(tarjan_scc_iterative(self))
    }

    /// Obtains the [`TarjanDAG`] of self, which is a directed acyclic graph that represents the
    /// strongly connected components of the transition system and the edges between them.
    fn tarjan_dag_recursive(&self) -> TarjanDAG<'_, Self>
    where
        Self: Sized,
    {
        TarjanDAG::from(tarjan_scc_recursive(self))
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

    /// Returns an iterator over the indices of the states that are reachable from the initial state. The iterator yields tuples
    /// (State Index, State Color)
    fn reachable_states(&self) -> ReachableStates<&Self>
    where
        Self: Sized + Pointed,
    {
        ReachableStates::new(self, self.initial())
    }

    /// Returns an iterator over all state colors that are reachable from the initial state. May yield the same color multiple times.
    fn reachable_state_colors(&self) -> impl Iterator<Item = Self::StateColor>
    where
        Self: Sized + Pointed,
    {
        self.reachable_states().map(|(_, c)| c)
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
    fn reachable_state_indices_from<I: Indexes<Self>>(
        &self,
        state: I,
    ) -> ReachableStateIndices<&Self>
    where
        Self: Sized,
    {
        let origin = state
            .to_index(self)
            .expect("Can only run this from a state that exists");
        ReachableStateIndices::new(self, origin)
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

/// Trait that helps with accessing states in more elaborate [`TransitionSystem`]s. For
/// example in a [`crate::RightCongruence`], we have more information than the [`Color`]
/// on a state, we have its [`Class`] as well. Since we would like to be able to
/// access a state of a congruence not only by its index, but also by its classname
/// or any other [word](`crate::prelude::LinearWord`) of finite length, this trait is necessary.
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

/// Implementors of this trait store the necessary information to fully specify a transition
/// in a transition system. Specifically, an implementor gives access to
/// - the index of the source state,
/// - the index of the target state,
/// - the color of the transition, and
/// - the symbol on which the transition is taken.
pub trait FullTransition<Idx, S, C> {
    /// Returns the index of the source state of the transition.
    fn source(&self) -> &Idx;
    /// Returns the index of the target state of the transition.
    fn target(&self) -> &Idx;
    /// Returns the color of the transition.
    fn color(&self) -> &C;
    /// Gives a reference to the expression that labels the transition.
    fn symbol(&self) -> &S;

    /// Produces a tuple (source, symbol, color, target) from the transition, by cloning
    /// all components.
    fn clone_tuple(&self) -> (Idx, S, C, Idx)
    where
        Idx: Clone,
        S: Clone,
        C: Clone,
    {
        (
            self.source().clone(),
            self.symbol().clone(),
            self.color().clone(),
            self.target().clone(),
        )
    }
}

impl<Idx, S, C> FullTransition<Idx, S, C> for (Idx, S, C, Idx) {
    fn clone_tuple(&self) -> (Idx, S, C, Idx)
    where
        Idx: Clone,
        S: Clone,
        C: Clone,
    {
        self.clone()
    }

    fn source(&self) -> &Idx {
        &self.0
    }

    fn target(&self) -> &Idx {
        &self.3
    }

    fn color(&self) -> &C {
        &self.2
    }

    fn symbol(&self) -> &S {
        &self.1
    }
}

impl<'a, Idx, S, C> FullTransition<Idx, S, C> for &'a (Idx, S, C, Idx) {
    fn source(&self) -> &Idx {
        &self.0
    }

    fn target(&self) -> &Idx {
        &self.3
    }

    fn color(&self) -> &C {
        &self.2
    }

    fn symbol(&self) -> &S {
        &self.1
    }
}

impl<'a, Idx, S, C> FullTransition<Idx, S, C> for (&'a Idx, &'a S, &'a C, &'a Idx) {
    fn source(&self) -> &Idx {
        self.0
    }

    fn target(&self) -> &Idx {
        self.3
    }

    fn color(&self) -> &C {
        self.2
    }

    fn symbol(&self) -> &S {
        self.1
    }
}

/// This trait is implemented for references to transitions, so that they can be used in
/// generic contexts. It is automatically implemented for (mutable) references.
pub trait IsEdge<'ts, E, Idx, C> {
    /// Returns the index of the source state of the transition.
    fn source(&self) -> Idx;
    /// Returns the target state of the transition.
    fn target(&self) -> Idx;
    /// Returns the color of the transition.
    fn color(&self) -> C;
    /// Gives a reference to the expression that labels the transition.
    fn expression(&self) -> &'ts E;
    /// Destructures the transition into its components.
    fn into_tuple(self) -> (Idx, &'ts E, Idx, C)
    where
        Self: Sized,
    {
        (
            self.source(),
            self.expression(),
            self.target(),
            self.color(),
        )
    }
    /// Destructures `self` but into a slightly different form.
    fn into_nested_tuple(self) -> (Idx, &'ts E, (Idx, C))
    where
        Self: Sized,
    {
        (
            self.source(),
            self.expression(),
            (self.target(), self.color()),
        )
    }
}

/// Represents a reference to an edge in a transition system. This stores a lifetime
/// to the transition system and references to the color and expression.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct EdgeReference<'ts, E, Idx, C> {
    source: Idx,
    target: Idx,
    color: &'ts C,
    expression: &'ts E,
}

impl<'ts, E, Idx, C> EdgeReference<'ts, E, Idx, C> {
    /// Creates a new edge reference from the given components.
    pub fn new(source: Idx, expression: &'ts E, color: &'ts C, target: Idx) -> Self {
        Self {
            source,
            target,
            color,
            expression,
        }
    }
}

impl<'ts, E, Idx: IndexType, C: Color> IsEdge<'ts, E, Idx, C> for EdgeReference<'ts, E, Idx, C> {
    fn source(&self) -> Idx {
        self.source
    }

    fn target(&self) -> Idx {
        self.target
    }

    fn color(&self) -> C {
        self.color.clone()
    }

    fn expression(&self) -> &'ts E {
        self.expression
    }
}

/// Represents an edge in a transition system similar to [`EdgeReference`], but it owns the
/// associated color, while the expression is still a reference.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TransitionOwnedColor<'ts, E, Idx, C> {
    source: Idx,
    target: Idx,
    color: C,
    expression: &'ts E,
}

impl<'ts, E, Idx, C> TransitionOwnedColor<'ts, E, Idx, C> {
    /// Creates a new instance from the given components.
    pub fn new(source: Idx, expression: &'ts E, color: C, target: Idx) -> Self {
        Self {
            source,
            target,
            color,
            expression,
        }
    }
}

impl<'ts, E, Idx: IndexType, C: Color> IsEdge<'ts, E, Idx, C>
    for TransitionOwnedColor<'ts, E, Idx, C>
{
    fn source(&self) -> Idx {
        self.source
    }

    fn target(&self) -> Idx {
        self.target
    }

    fn color(&self) -> C {
        self.color.clone()
    }

    fn expression(&self) -> &'ts E {
        self.expression
    }
}

/// Through this struct, we enable iterating over all transitions that leave a given state of
/// a transition system.
pub struct TransitionsFrom<'a, D: TransitionSystem + 'a> {
    edges: D::EdgesFromIter<'a>,
    symbols: Option<<ExpressionOf<D> as Expression<SymbolOf<D>>>::SymbolsIter<'a>>,
    target: Option<D::StateIndex>,
    color: Option<D::EdgeColor>,
    source: D::StateIndex,
}

impl<'a, D: TransitionSystem + 'a> Iterator for TransitionsFrom<'a, D> {
    type Item = (D::StateIndex, SymbolOf<D>, D::EdgeColor, D::StateIndex);
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(sym) = self.symbols.as_mut().and_then(|mut it| it.next()) {
            return Some((
                self.source,
                sym,
                self.color.clone().unwrap(),
                self.target.unwrap(),
            ));
        } else {
            for t in self.edges.by_ref() {
                let mut it = t.expression().symbols();
                if let Some(sym) = it.next() {
                    let target = t.target();
                    let color = t.color();
                    self.symbols = Some(it);
                    self.color = Some(color.clone());
                    self.target = Some(target);
                    return Some((self.source, sym, color.clone(), target));
                }
            }
        }
        None
    }
}

impl<'a, D: TransitionSystem + 'a> TransitionsFrom<'a, D> {
    /// Creates a new instance from a reference to a transition system and the index of the state
    /// from which the transitions should be taken.
    pub fn new(det: &'a D, state: D::StateIndex) -> Self {
        let Some(mut edges) = det.edges_from(state) else {
            panic!(
                "We should at least get an iterator. Probably the state {state} does not exist."
            );
        };

        Self {
            edges,
            symbols: None,

            target: None,
            color: None,
            source: state,
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
    ($basetype: ident < $bound: ident >) => {
        impl<Ts: TransitionSystem + $bound> TransitionSystem for $basetype<Ts> {
            type StateIndex = Ts::StateIndex;
            type EdgeColor = Ts::EdgeColor;
            type StateColor = Ts::StateColor;
            type EdgeRef<'this> = Ts::EdgeRef<'this> where Self: 'this;
            type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
            type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

            type Alphabet = Ts::Alphabet;
            fn alphabet(&self) -> &Self::Alphabet {
                self.ts().alphabet()
            }
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
        impl<D: Deterministic + $bound> Deterministic for $basetype<D> {
            fn transition<Idx: Indexes<Self>>(
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

impl_ts_by_passthrough_on_wrapper!(Initialized<TransitionSystem>);

impl<Ts: TransitionSystem> TransitionSystem for &Ts {
    type StateIndex = Ts::StateIndex;
    type EdgeColor = Ts::EdgeColor;
    type StateColor = Ts::StateColor;
    type EdgeRef<'this> = Ts::EdgeRef<'this> where Self: 'this;
    type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
    type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;
    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        Ts::alphabet(self)
    }

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
    type EdgeRef<'this> = Ts::EdgeRef<'this> where Self : 'this;
    type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
    type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        Ts::alphabet(self)
    }
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
    type EdgeRef<'this> = &'this NTEdge<A::Expression, C> where Self: 'this;
    type EdgesFromIter<'this> = NTSEdgesFromIter<'this, A::Expression, C>
    where
        Self: 'this;
    type StateIndices<'this> = std::ops::Range<usize> where Self: 'this;

    type Alphabet = A;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts().alphabet()
    }
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
    type EdgeRef<'this> = EdgeReference<'this, A::Expression, Idx, C> where Self: 'this;
    type EdgesFromIter<'this> = BTSEdgesFrom<'this, A::Expression, Idx, C> where Self: 'this;
    type StateIndices<'this> = std::iter::Cloned<std::collections::hash_map::Keys<'this, Idx, super::index_ts::BTState<A, Q, C, Idx>>> where Self: 'this;

    type Alphabet = A;

    fn alphabet(&self) -> &Self::Alphabet {
        &self.alphabet
    }
    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.states.keys().cloned()
    }

    fn state_color(&self, index: Idx) -> Option<StateColor<Self>> {
        self.raw_state_map().get(&index).map(|s| s.color().clone())
    }

    fn edges_from<X: Indexes<Self>>(&self, state: X) -> Option<Self::EdgesFromIter<'_>> {
        let source = state.to_index(self)?;
        Some(BTSEdgesFrom::new(
            source,
            self.raw_state_map()
                .get(&source)
                .map(|o| o.edge_map().iter())?,
        ))
    }
}

/// Specialized iterator over the edges that leave a given state in a BTS.
pub struct BTSEdgesFrom<'ts, E, Idx, C> {
    edges: std::collections::hash_map::Iter<'ts, E, (Idx, C)>,
    source: Idx,
}

impl<'ts, E, Idx, C> BTSEdgesFrom<'ts, E, Idx, C> {
    /// Creates a new instance from the given components.
    pub fn new(source: Idx, edges: std::collections::hash_map::Iter<'ts, E, (Idx, C)>) -> Self {
        Self { edges, source }
    }
}

impl<'ts, E, Idx: IndexType, C> Iterator for BTSEdgesFrom<'ts, E, Idx, C> {
    type Item = EdgeReference<'ts, E, Idx, C>;

    fn next(&mut self) -> Option<Self::Item> {
        self.edges.next().map(|(e, (t, c))| EdgeReference {
            source: self.source,
            target: *t,
            color: c,
            expression: e,
        })
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
    type EdgeRef<'this> = ProductTransition<'this, L::StateIndex, R::StateIndex, ExpressionOf<L>, L::EdgeColor, R::EdgeColor> where Self: 'this;
    type EdgesFromIter<'this> = ProductEdgesFrom<'this, L, R> where Self: 'this;
    type StateIndices<'this> = ProductStatesIter<'this, L, R> where Self: 'this;
    type Alphabet = L::Alphabet;
    fn alphabet(&self) -> &Self::Alphabet {
        self.0.alphabet()
    }

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
    type EdgeRef<'this> = Ts::EdgeRef<'this> where Self: 'this;
    type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;
    type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts().alphabet()
    }
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
    type EdgeRef<'this> = MappedTransition<Ts::EdgeRef<'this>, &'this F, Ts::EdgeColor> where Self: 'this;
    type EdgesFromIter<'this> =
        MappedEdgesFromIter<'this, Ts::EdgesFromIter<'this>, F, Ts::EdgeColor> where Self: 'this;

    type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts().alphabet()
    }
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
    type EdgeRef<'this> = Ts::EdgeRef<'this> where Self: 'this;
    type EdgesFromIter<'this> = RestrictedEdgesFromIter<'this, Ts, F> where Self: 'this;
    type StateIndices<'this> = Ts::StateIndices<'this> where Self: 'this;

    type Alphabet = Ts::Alphabet;

    fn alphabet(&self) -> &Self::Alphabet {
        self.ts().alphabet()
    }
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

/// Specialized version of an iterator over the edges leaving a specific state of a **deterministic**
/// transition system.
pub struct DeterministicEdgesFrom<'a, Ts: TransitionSystem> {
    ts: &'a Ts,
    state: Ts::StateIndex,
    symbols: <Ts::Alphabet as Alphabet>::Universe<'a>,
}

impl<'a, Ts: Deterministic> Iterator for DeterministicEdgesFrom<'a, Ts> {
    type Item = Ts::EdgeRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.symbols
            .next()
            .and_then(|sym| self.ts.transition(self.state, sym))
    }
}

impl<'a, Ts: TransitionSystem> DeterministicEdgesFrom<'a, Ts> {
    /// Creates a new instance from the given components.
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
