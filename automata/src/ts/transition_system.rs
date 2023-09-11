use std::{collections::BTreeSet, iter::Product};

use crate::{
    alphabet::{Expression, ExpressionOf, HasAlphabet, Symbol, SymbolOf},
    automaton::WithInitial,
    word::OmegaWord,
    Alphabet, Class, Color, Map, Pointed, RightCongruence, Set, Word,
};

use super::{
    operations::{
        MapEdgeColor, MapStateColor, MappedEdgesFromIter, MappedEdgesToIter, MappedPreTransition,
        MappedTransition, MatchingProduct, ProductEdgesFrom, ProductEdgesTo, ProductIndex,
        ProductPreTransition, ProductTransition, RestrictByStateIndex, RestrictedEdgesFromIter,
        RestrictedEdgesToIter,
    },
    reachable::{MinimalRepresentatives, ReachableStateIndices, ReachableStates},
    run::{
        successful::Successful,
        walker::{RunResult, Walker},
    },
    sccs::{tarjan_scc, Scc, SccDecomposition, TarjanDAG},
};

use super::{
    finite::{ReachedColor, ReachedState},
    path::Lasso,
    CanInduce, Edge, EdgeColor, FiniteState, IndexType, Induced, Path, StateColor, StateIndex,
    Transition, BTS,
};

use impl_tools::autoimpl;

#[autoimpl(for<T: trait + ?Sized> &T, &mut T)]
pub trait IsTransition<E, Idx, C> {
    fn target(&self) -> Idx;
    fn color(&self) -> C;
    fn expression(&self) -> &E;
    fn into_tuple(self) -> (E, Idx, C)
    where
        E: Clone,
        Self: Sized,
    {
        (self.expression().clone(), self.target(), self.color())
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

/// Encapsulates the transition function δ of a (finite) transition system. This is the main trait that
/// is used to query a transition system. Transitions are labeled with a [`Alphabet::Expression`], which
/// determines on which [`Alphabet::Symbol`]s the transition can be taken. Additionally, every transition
/// is labeled with a [`Color`], which can be used to store additional information about it, like an
/// associated priority.
///
/// # The difference between [`Transition`]s and [`crate::ts::Edge`]s
/// Internally, a transition system is represented as a graph, where the states are the nodes and the
/// transitions are the edges. However, the [`Transition`]s are not the same as the [`crate::ts::Edge`]s.
/// Both store the source and target vertex as well as the color, however an [`crate::ts::Edge`] is labelled
/// with an expression, while a [`Transition`] is labelled with an actual symbol (that [`Alphabet::matches`]
/// the expression). So a transition is a concrete edge that is taken (usually by the run on a word), while
/// an edge may represent any different number of transitions.
pub trait TransitionSystem: HasAlphabet {
    type StateIndex: IndexType;
    type StateColor: Color;
    type EdgeColor: Color;
    type TransitionRef<'this>: IsTransition<ExpressionOf<Self>, Self::StateIndex, EdgeColor<Self>>
    where
        Self: 'this;
    type EdgesFromIter<'this>: Iterator<Item = Self::TransitionRef<'this>>
    where
        Self: 'this;

    /// For a given `state` and `symbol`, returns the transition that is taken, if it exists.
    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>>;

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>>;

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>>;

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor>;

    fn with_initial(self, initial: Self::StateIndex) -> WithInitial<Self>
    where
        Self: Sized,
    {
        (self, initial).into()
    }

    fn restrict_state_indices<F: Fn(Self::StateIndex) -> bool>(
        self,
        filter: F,
    ) -> RestrictByStateIndex<Self, F>
    where
        Self: Sized,
    {
        RestrictByStateIndex::new(self, filter)
    }

    fn map_colors<D: Color, F: Fn(Self::StateColor) -> D>(self, f: F) -> MapStateColor<Self, F>
    where
        Self: Sized,
    {
        MapStateColor::new(self, f)
    }

    fn all_accepting_dfa(self) -> MapStateColor<Self, fn(Self::StateColor) -> bool>
    where
        Self: Sized,
    {
        self.map_colors(|_| true)
    }

    fn sccs(&self) -> SccDecomposition<'_, Self>
    where
        Self: Sized + FiniteState,
    {
        tarjan_scc(self)
    }

    fn tarjan_tree(&self) -> TarjanDAG<'_, Self>
    where
        Self: Sized + FiniteState + Clone,
    {
        TarjanDAG::from(tarjan_scc(self))
    }

    /// Returns just the [Self::Index] of the successor that is reached on the given `symbol`
    /// from `state`. If no suitable transition exists, `None` is returned.
    fn successor_index(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::StateIndex> {
        self.transition(state, symbol).map(|t| t.target())
    }

    /// Starts a new [`Walker`] that can be used to successively take transitions from `state` on
    /// the letters of `word`.
    fn walk<'a, 'b, R: Word<Symbol = SymbolOf<Self>>>(
        &'a self,
        word: &'b R,
        state: Self::StateIndex,
    ) -> Walker<'a, 'b, Self, R>
    where
        Self: Sized,
    {
        Walker::new(word, self, state)
    }

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

    fn is_reachable(&self, state: Self::StateIndex) -> bool
    where
        Self: Sized + Pointed,
    {
        self.is_reachable_from(self.initial(), state)
    }

    fn is_reachable_from(&self, origin: Self::StateIndex, state: Self::StateIndex) -> bool
    where
        Self: Sized + Pointed,
    {
        self.reachable_state_indices_from(origin)
            .any(|s| s == state)
    }

    /// Runs the given `word` on the transition system, starting from `state`. The result is
    /// - [`Ok`] if the run is successful (i.e. for all symbols of `word` a suitable transition
    ///  can be taken),
    /// - [`Err`] if the run is unsuccessful, meaning a symbol is encountered for which no
    /// transition exists.
    #[allow(clippy::type_complexity)]
    fn finite_run(
        &self,
        origin: Self::StateIndex,
        word: &[SymbolOf<Self>],
    ) -> Result<Path<Self::Alphabet, Self::StateIndex>, Path<Self::Alphabet, Self::StateIndex>>
    where
        Self: Sized,
    {
        let mut current = origin;
        let mut path = Path::empty(current);
        for symbol in word {
            if let Some(o) = path.extend_in(&self, *symbol) {
                current = o.target();
            } else {
                return Err(path);
            }
        }
        Ok(path)
    }

    /// Runs the given `word` on the transition system, starting from `state`.
    #[allow(clippy::type_complexity)]
    fn omega_run(
        &self,
        origin: Self::StateIndex,
        base: &[SymbolOf<Self>],
        recur: &[SymbolOf<Self>],
    ) -> Result<Lasso<Self::Alphabet, Self::StateIndex>, Path<Self::Alphabet, Self::StateIndex>>
    where
        Self: Pointed + Sized,
    {
        let mut path = self.finite_run(origin, base)?;
        let mut position = path.len();
        let mut seen = Map::default();

        loop {
            match seen.insert(path.reached(), position) {
                Some(p) => {
                    return Ok(path.loop_back_to(p));
                }
                None => match self.finite_run(path.reached(), recur) {
                    Ok(p) => {
                        position += p.len();
                        path.extend_with(p);
                    }
                    Err(p) => {
                        path.extend_with(p);
                        return Err(path);
                    }
                },
            }
        }

        unreachable!()
    }

    /// Runs the given `word` on the transition system, starting from `state`, which means starting
    /// a new [`Walker`] and immediately taking all transitions on the letters of `word`. If the
    /// run is successful (i.e. for all symbols of `word` a suitable transition can be taken), this
    /// returns a [`Successful`] run, which can then be used to obtain the colors of the transitions
    /// or the sequence of states that are visited. If the run is unsuccessful, meaning a symbol is
    /// encountered for which no transition exists, this returns a [`Partial`] run, which can be used
    /// to obtain the colors of the transitions that were taken before, as well as the state that
    /// the transition system was left from and the remaining suffix.
    fn run<'a, 'b, R: Word<Symbol = SymbolOf<Self>>>(
        &'a self,
        word: &'b R,
        state: Self::StateIndex,
    ) -> RunResult<'a, 'b, Self, R>
    where
        Self: Sized,
    {
        self.walk(word, state).result()
    }

    /// Runs the given `word` on the transition system, starting from `state` by calling [`Self::run`].
    /// If the run is successful (i.e. for all symbols of `word` a suitable transition can be taken),
    /// this returns whatever is *induced* by the run. For a [`Word`] of finite length, this is
    /// simply
    fn induced<'a, 'b, R, I>(&'a self, word: &'b R, state: Self::StateIndex) -> Option<I>
    where
        I: Induced,
        Successful<'a, 'b, R, Self>: CanInduce<I>,
        Self: Sized,
        R: Word<Symbol = SymbolOf<Self>>,
    {
        self.run(word, state).ok().map(|r| r.induce())
    }

    fn reached_color<'a, 'b, R>(&'a self, word: &'b R) -> Option<ReachedColor<StateColor<Self>>>
    where
        Successful<'a, 'b, R, Self>: CanInduce<ReachedColor<StateColor<Self>>>,
        Self: Sized + Pointed,
        R: Word<Symbol = SymbolOf<Self>>,
    {
        self.induced(word, self.initial())
    }

    fn can_separate<'a, 'b, 'c, R, RR>(&'a self, left: &'b R, right: &'c RR) -> bool
    where
        Successful<'a, 'b, R, Self>: CanInduce<ReachedState<Self::StateIndex>>,
        Successful<'a, 'c, RR, Self>: CanInduce<ReachedState<Self::StateIndex>>,
        Self: Sized + Pointed,
        R: Word<Symbol = SymbolOf<Self>>,
        RR: Word<Symbol = SymbolOf<Self>>,
    {
        let left = self.induced(left, self.initial());
        let right = self.induced(right, self.initial());
        match (left, right) {
            (Some(ReachedState(l)), Some(ReachedState(r))) => l != r,
            _ => true,
        }
    }

    fn reached_state_index<'a, 'b, R>(
        &'a self,
        word: &'b R,
    ) -> Option<ReachedState<Self::StateIndex>>
    where
        Successful<'a, 'b, R, Self>: CanInduce<ReachedState<Self::StateIndex>>,
        Self: Sized + Pointed,
        R: Word<Symbol = SymbolOf<Self>>,
    {
        self.induced(word, self.initial())
    }

    fn minimal_representatives(&self) -> MinimalRepresentatives<&Self>
    where
        Self: Sized + Pointed,
    {
        MinimalRepresentatives::new(self, self.initial())
    }

    fn reachable_state_indices(&self) -> ReachableStateIndices<&Self>
    where
        Self: Sized + Pointed,
    {
        ReachableStateIndices::new(self, self.initial())
    }

    fn reachable_states(&self) -> ReachableStates<&Self>
    where
        Self: Sized + Pointed,
    {
        ReachableStates::new(self, self.initial())
    }

    fn minimal_representatives_from<I: Into<Self::StateIndex>>(
        &self,
        state: I,
    ) -> MinimalRepresentatives<&Self>
    where
        Self: Sized,
    {
        MinimalRepresentatives::new(self, state.into())
    }

    fn reachable_state_indices_from<I: Into<Self::StateIndex>>(
        &self,
        state: I,
    ) -> ReachableStateIndices<&Self>
    where
        Self: Sized,
    {
        ReachableStateIndices::new(self, state.into())
    }

    fn reachable_states_from<I: Into<Self::StateIndex>>(&self, state: I) -> ReachableStates<&Self>
    where
        Self: Sized + Pointed,
    {
        ReachableStates::new(self, self.initial())
    }

    fn build_transition_table<SD>(&self, state_decorator: SD) -> String
    where
        SD: Fn(Self::StateIndex, StateColor<Self>) -> String,
        Self: FiniteState,
        Self::EdgeColor: std::fmt::Debug,
    {
        let mut builder = tabled::builder::Builder::default();
        builder.set_header(
            std::iter::once("State".to_string())
                .chain(self.alphabet().universe().map(|s| format!("{:?}", s))),
        );
        for id in self.state_indices() {
            let mut row = vec![format!(
                "{}",
                state_decorator(
                    id,
                    self.state_color(id)
                        .expect("Every state should be colored!")
                )
            )];
            for &sym in self.alphabet().universe() {
                if let Some(edge) = self.transition(id, sym) {
                    row.push(format!("{} : {:?}", edge.target(), edge.color()));
                } else {
                    row.push("-".to_string());
                }
            }
            builder.push_record(row);
        }

        builder
            .build()
            .with(tabled::settings::Style::rounded())
            .to_string()
    }

    fn collect_ts(&self) -> BTS<Self::Alphabet, Self::StateColor, Self::EdgeColor>
    where
        Self: FiniteState,
    {
        use crate::ts::Sproutable;
        let mut ts = BTS::new_for_alphabet(self.alphabet().clone());
        let mut map = std::collections::HashMap::new();
        for index in self.state_indices() {
            map.insert(
                index,
                ts.add_state(
                    self.state_color(index)
                        .expect("We assume each state to be colored!"),
                ),
            );
        }
        for index in self.state_indices() {
            for sym in self.alphabet().universe() {
                if let Some(edge) = self.transition(index, *sym) {
                    ts.add_edge(
                        *map.get(&index).unwrap(),
                        <Self::Alphabet as Alphabet>::expression(*sym),
                        *map.get(&edge.target()).unwrap(),
                        edge.color().clone(),
                    );
                }
            }
        }
        ts
    }

    fn collect_into_ts<
        Ts: TransitionSystem<
                StateColor = Self::StateColor,
                EdgeColor = Self::EdgeColor,
                Alphabet = Self::Alphabet,
            > + super::Sproutable,
    >(
        &self,
    ) -> Ts
    where
        Self: FiniteState,
    {
        let mut ts = Ts::new_for_alphabet(self.alphabet().clone());
        let mut map = std::collections::HashMap::new();
        for index in self.state_indices() {
            map.insert(
                index,
                ts.add_state(
                    self.state_color(index)
                        .expect("Every state should be colored!"),
                ),
            );
        }
        for index in self.state_indices() {
            for sym in self.alphabet().universe() {
                if let Some(edge) = self.transition(index, *sym) {
                    ts.add_edge(
                        *map.get(&index).unwrap(),
                        <Self::Alphabet as Alphabet>::expression(*sym),
                        *map.get(&edge.target()).unwrap(),
                        edge.color().clone(),
                    );
                }
            }
        }
        ts
    }
}

impl<Ts: TransitionSystem> TransitionSystem for &Ts {
    type StateIndex = Ts::StateIndex;
    type EdgeColor = Ts::EdgeColor;
    type StateColor = Ts::StateColor;
    type TransitionRef<'this> = Ts::TransitionRef<'this> where Self: 'this;
    type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        Ts::transition(self, state, symbol)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        Ts::state_color(self, state)
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        Ts::edge_color(self, state, expression)
    }

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
        Ts::edges_from(self, state)
    }
}
impl<Ts: TransitionSystem> TransitionSystem for &mut Ts {
    type StateIndex = Ts::StateIndex;
    type EdgeColor = Ts::EdgeColor;
    type StateColor = Ts::StateColor;
    type TransitionRef<'this> = Ts::TransitionRef<'this> where Self : 'this;
    type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        Ts::transition(self, state, symbol)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        Ts::state_color(self, state)
    }

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
        Ts::edges_from(self, state)
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        Ts::edge_color(self, state, expression)
    }
}

impl<A: Alphabet> TransitionSystem for RightCongruence<A> {
    type StateIndex = usize;
    type EdgeColor = ();
    type StateColor = Class<A::Symbol>;
    type TransitionRef<'this> = (&'this A::Expression, &'this (usize, ())) where Self: 'this;
    type EdgesFromIter<'this> = std::collections::hash_map::Iter<'this, A::Expression, (usize, ())>
    where
        Self: 'this;

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: crate::alphabet::SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        self.ts().transition(state, symbol)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        self.ts().state_color(state)
    }

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
        self.ts().edges_from(state)
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &crate::alphabet::ExpressionOf<Self>,
    ) -> Option<crate::ts::EdgeColor<Self>> {
        self.ts().edge_color(state, expression)
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

    fn transition(&self, state: Idx, symbol: A::Symbol) -> Option<Self::TransitionRef<'_>> {
        self.states()
            .get(&state)
            .and_then(|o| A::search_edge(o.edge_map(), symbol))
    }

    fn state_color(&self, index: Idx) -> Option<StateColor<Self>> {
        self.states().get(&index).map(|s| s.color().clone())
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &crate::alphabet::ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.states()
            .get(&state)
            .and_then(|o| o.edge_map().get(expression).map(|(_, c)| c.clone()))
    }

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
        self.states().get(&state).map(|o| o.edge_map().iter())
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

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        let ProductIndex(l, r) = state;

        let ll = self.0.transition(l, symbol)?;
        let rr = self.1.transition(r, symbol)?;
        Some(ProductTransition::new(
            ll.expression().clone(),
            ProductIndex(ll.target(), rr.target()),
            (ll.color(), rr.color()),
        ))
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        let ProductIndex(l, r) = state;
        let left = self.0.state_color(l)?;
        let right = self.1.state_color(r)?;
        Some((left, right))
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        let ProductIndex(l, r) = state;
        let left = self.0.edge_color(l, expression)?;
        let right = self.1.edge_color(r, expression)?;
        Some((left, right))
    }

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
        ProductEdgesFrom::new(&self.0, &self.1, state)
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

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        self.ts().transition(state, symbol)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        let color = self.ts().state_color(state)?;
        Some((self.f())(color))
    }

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
        self.ts().edges_from(state)
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.ts().edge_color(state, expression)
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

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        Some(MappedTransition::new(
            self.ts().transition(state, symbol)?,
            self.f(),
        ))
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        self.ts().state_color(state)
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.ts()
            .edge_color(state, expression)
            .map(|c| (self.f())(c))
    }

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
        Some(MappedEdgesFromIter::new(
            self.ts().edges_from(state)?,
            self.f(),
        ))
    }
}

impl<Ts: TransitionSystem, F> TransitionSystem for RestrictByStateIndex<Ts, F>
where
    F: Fn(Ts::StateIndex) -> bool,
{
    type StateIndex = Ts::StateIndex;
    type EdgeColor = Ts::EdgeColor;
    type StateColor = Ts::StateColor;
    type TransitionRef<'this> = Ts::TransitionRef<'this> where Self: 'this;
    type EdgesFromIter<'this> = RestrictedEdgesFromIter<'this, Ts, F> where Self: 'this;

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: crate::alphabet::SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        self.ts()
            .transition(state, symbol)
            .filter(|successor| (self.filter())(state) && (self.filter())(successor.target()))
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        assert!((self.filter())(state));
        self.ts().state_color(state)
    }

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
        if !(self.filter())(state) {
            return None;
        }
        self.ts()
            .edges_from(state)
            .map(|iter| RestrictedEdgesFromIter::new(iter, self.filter()))
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &crate::alphabet::ExpressionOf<Self>,
    ) -> Option<crate::ts::EdgeColor<Self>> {
        self.ts()
            .edge_color(state, expression)
            .filter(|_| (self.filter())(state))
    }
}

impl<Ts: TransitionSystem> TransitionSystem for WithInitial<Ts> {
    type StateIndex = Ts::StateIndex;
    type StateColor = Ts::StateColor;
    type EdgeColor = Ts::EdgeColor;
    type TransitionRef<'this> = Ts::TransitionRef<'this> where Self: 'this;
    type EdgesFromIter<'this> = Ts::EdgesFromIter<'this> where Self: 'this;

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        self.ts().transition(state, symbol)
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<StateColor<Self>> {
        self.ts().state_color(state)
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &crate::alphabet::ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.ts().edge_color(state, expression)
    }

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
        self.ts().edges_from(state)
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use super::TransitionSystem;
    use crate::{
        alphabet,
        ts::{
            finite::{self, ReachedColor, ReachedState},
            index_ts::MealyTS,
            Sproutable, BTS,
        },
        word::OmegaWord,
        FiniteLength, Word,
    };

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

        let input = OmegaWord::new(vec!['a', 'b', 'b', 'a'], FiniteLength::new(4));
        let res = ts.run(&input, s0);
        assert!(res.is_ok());

        let ReachedState(q) = ts.induced(&"ab", s0).unwrap();
        assert_eq!(q, s0);
        let ReachedColor(c) = ts.induced(&input, s0).unwrap();
    }
}
