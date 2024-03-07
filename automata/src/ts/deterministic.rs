use std::{hash::Hash, marker::PhantomData};
use tracing::trace;

use itertools::Itertools;

use crate::Bijection;
use crate::Map;
use crate::Set;
use crate::TransitionSystem;

use crate::prelude::*;

use super::operations::MapEdgeColor;
use super::operations::MapEdges;
use super::operations::MapStateColor;
use super::operations::MappedEdge;
use super::operations::MappedTransition;
use super::operations::MatchingProduct;
use super::operations::ProductTransition;
use super::operations::RestrictByStateIndex;
use super::operations::StateIndexFilter;
use super::path::Lasso;
use super::path::LassoIn;
use super::path::PathIn;
use super::reachable::MinimalRepresentative;
use super::sproutable::{IndexedAlphabet, Sproutable};
use super::IntoHashTs;
use super::Path;

pub type FiniteRunResult<A, Idx, Q, C> = Result<Path<A, Idx, Q, C>, Path<A, Idx, Q, C>>;
pub type OmegaRunResult<A, Idx, Q, C> = Result<Lasso<A, Idx, Q, C>, Path<A, Idx, Q, C>>;

/// A marker tait indicating that a [`TransitionSystem`] is deterministic, meaning for every state and
/// each possible input symbol from the alphabet, there is at most one transition. Under the hood, this
/// trait simply calls [`TransitionSystem::edges_from`] and checks whether there is at most one edge
/// for each symbol. If there is more than one edge, the methods of this trait panic.
///
/// # Implementaiton
/// This trait contains mostly convenience functions and provides default implementations. To ensure
/// performance, the [`Self::collect_dts`] function and any other collectors for different types of
/// transition system implementations should be overridden. By default, they simply insert states
/// and edges one by one and are therefore horribly inefficient.
pub trait Deterministic: TransitionSystem {
    /// For a given `state`, returns the unique edge that matches the given `symbol`. Panics if multiple
    /// edges match the symbol. If the state does not exist or no edge matches the symbol, `None` is returned.
    ///
    /// # Example
    /// ```
    /// use automata::prelude::*;
    /// let ts = TSBuilder::without_state_colors()
    ///     .with_transitions([(0, 'a', Void, 1), (1, 'a', Void, 2), (2, 'a', Void, 0)])
    ///     .into_right_congruence_bare(0);
    /// assert_eq!(ts.transition(0, 'a').unwrap().target(), 1);
    /// assert_eq!(ts.transition(1, 'a').unwrap().target(), 2);
    /// assert_eq!(ts.transition(2, 'a').unwrap().target(), 0);
    /// assert_eq!(ts.transition(0, 'b'), None);
    /// assert_eq!(ts.transition(3, 'a'), None);
    /// ```
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        let state = state.to_index(self)?;
        let mut it = self
            .edges_from(state)
            .expect("We know this state exists")
            .filter(|e| e.expression().matches(symbol));
        let first = it.next()?;
        debug_assert!(
            it.next().is_none(),
            "There should be only one edge with the given symbol"
        );
        Some(first)
    }

    /// Attempts to find the first edge that matches the given `expression` from the given `state`. If no
    /// suitable transition exists, `None` is returned. If more than one edge matches the expression, the
    /// method panics.
    ///
    /// # Example
    /// ```
    /// use automata::prelude::*;
    /// let ts = TSBuilder::without_state_colors()
    ///     .with_transitions([(0, 'a', Void, 1), (1, 'a', Void, 2), (2, 'a', Void, 0)])
    ///     .into_right_congruence_bare(0);
    /// assert_eq!(ts.edge(0, &'a').unwrap().target(), 1);
    /// assert_eq!(ts.edge(1, &'a').unwrap().target(), 2);
    /// assert_eq!(ts.edge(2, &'a').unwrap().target(), 0);
    /// assert_eq!(ts.edge(0, &'b'), None);
    /// assert_eq!(ts.edge(3, &'a'), None);
    /// ```
    fn edge<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        let state = state.to_index(self)?;
        let mut it = self
            .edges_from(state)
            .expect("We know this state exists")
            .filter(|e| e.expression() == expression);

        let first = it.next()?;
        debug_assert!(
            it.next().is_none(),
            "There should be only one edge with the given expression"
        );
        Some(first)
    }

    /// Returns just the [`TransitionSystem::StateIndex`] of the successor that is reached on the given `symbol`
    /// from `state`. If no suitable transition exists, `None` is returned.
    ///
    /// # Example
    /// ```
    /// use automata::prelude::*;
    ///
    /// let ts = TSBuilder::without_state_colors()
    ///     .with_transitions([(0, 'a', Void, 0), (0, 'b', Void, 1), (1, 'a', Void, 1), (1, 'b', Void, 1)])
    ///     .into_right_congruence_bare(0);
    /// assert_eq!(ts.successor_index(0, 'a'), Some(0));
    /// assert_eq!(ts.successor_index(0, 'b'), Some(1));
    /// assert_eq!(ts.successor_index(0, 'c'), None);
    /// ```
    fn successor_index<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::StateIndex> {
        self.transition(state.to_index(self)?, symbol)
            .map(|t| t.target())
    }

    /// Returns the color of an edge starting in the given `state` and labeled with the given
    /// `expression`, if it exists. Otherwise, `None` is returned.
    fn edge_color<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        let mut symbols = expression.symbols();
        let sym = symbols.next().unwrap();
        assert_eq!(
            symbols.next(),
            None,
            "There are multiple symbols for this expression"
        );
        Some(self.transition(state.to_index(self)?, sym)?.color().clone())
    }

    /// Attempts to find the minimal representative of the indexed `state`, which the the length-lexicographically
    /// minimal word that can be used to reach `state`. If `state` is not reachable, `None` is returned.
    fn minimal_representative<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Vec<SymbolOf<Self>>>
    where
        Self: Pointed,
    {
        let q = state.to_index(self)?;
        self.minimal_representatives()
            .find_map(|(rep, p)| if p == q { Some(rep) } else { None })
    }

    /// Gives an iterator over the minimal transition representatives, which are the length-lexicographically
    /// minimal words that can be used to use a transition. The iterator returns only unique elements.
    fn minimal_transition_representatives(&self) -> impl Iterator<Item = Vec<SymbolOf<Self>>>
    where
        Self: Pointed,
    {
        self.minimal_representatives()
            .flat_map(|(rep, _)| {
                self.symbols()
                    .map(move |a| crate::word::Concat(&rep, [a]).to_vec())
            })
            .unique()
    }

    /// Runs the given `word` on the transition system, starting from the initial state. The result is
    /// - [`Ok`] if the run is successful (i.e. for all symbols of `word` a suitable transition
    ///  can be taken),
    /// - [`Err`] if the run is unsuccessful, meaning a symbol is encountered for which no
    /// transition exists.
    ///
    /// It returns a [`PathIn`] in either case, which is a path in the transition system. So it is possible
    /// to inspect the path, e.g. to find out which state was reached or which transitions were taken.
    /// For more information, see [`crate::prelude::Path`].
    #[allow(clippy::type_complexity)]
    fn finite_run<W: FiniteWord<SymbolOf<Self>>>(
        &self,
        word: W,
    ) -> FiniteRunResult<Self::Alphabet, Self::StateIndex, Self::StateColor, Self::EdgeColor>
    where
        Self: Pointed,
    {
        self.finite_run_from(word, self.initial())
    }

    /// Runs the given `word` on the transition system, starting from `state`. The result is
    /// - [`Ok`] if the run is successful (i.e. for all symbols of `word` a suitable transition
    ///  can be taken),
    /// - [`Err`] if the run is unsuccessful, meaning a symbol is encountered for which no
    /// transition exists.
    #[allow(clippy::type_complexity)]
    fn finite_run_from<W, Idx>(
        &self,
        word: W,
        origin: Idx,
    ) -> FiniteRunResult<Self::Alphabet, Self::StateIndex, Self::StateColor, Self::EdgeColor>
    where
        Self: Sized,
        W: FiniteWord<SymbolOf<Self>>,
        Idx: Indexes<Self>,
    {
        let mut current = origin
            .to_index(self)
            .expect("run must start in state that exists");
        let mut path = Path::empty_in_with_capacity(self, current, word.len());
        for symbol in word.symbols() {
            if let Some(o) = path.extend_in(&self, symbol) {
                current = o.target();
                continue;
            }
            return Err(path);
        }
        Ok(path)
    }

    /// Runs the given `word` from the `origin` state. If the run is successful, the function returns the indices
    /// of all states which appear infinitely often. For unsuccessful runs, `None` is returned.
    fn recurrent_state_indices_from<W: OmegaWord<SymbolOf<Self>>, Idx: Indexes<Self>>(
        &self,
        word: W,
        origin: Idx,
    ) -> Option<impl Iterator<Item = Self::StateIndex>> {
        Some(
            self.omega_run_from(word, origin)
                .ok()?
                .into_recurrent_state_indices(),
        )
    }

    /// Returns an iterator over the state indices that are visited infinitely often when running the given `word`
    /// on the transition system, starting from the initial state. If the run is unsuccessful, `None` is returned.
    fn recurrent_state_indices<W: OmegaWord<SymbolOf<Self>>>(
        &self,
        word: W,
    ) -> Option<impl Iterator<Item = Self::StateIndex>>
    where
        Self: Pointed,
    {
        self.recurrent_state_indices_from(word, self.initial())
    }

    /// Returns an iterator yielding the colors of states which are visited infinitely often when running the given `word`
    /// on the transition system, starting from the initial state. If the run is unsuccessful, `None` is returned.  
    fn recurrent_state_colors_from<W: OmegaWord<SymbolOf<Self>>, Idx: Indexes<Self>>(
        &self,
        word: W,
        origin: Idx,
    ) -> Option<impl Iterator<Item = Self::StateColor>> {
        Some(
            self.omega_run_from(word, origin)
                .ok()?
                .into_recurrent_state_colors(),
        )
    }

    /// Returns an iterator yielding the colors of states which are visited infinitely often when running the given `word`
    /// on the transition system, starting from the initial state. If the run is unsuccessful, `None` is returned.
    fn recurrent_state_colors<W: OmegaWord<SymbolOf<Self>>>(
        &self,
        word: W,
    ) -> Option<impl Iterator<Item = Self::StateColor>>
    where
        Self: Pointed,
    {
        self.recurrent_state_colors_from(word, self.initial())
    }

    /// Gives an iterator that emits the colors of edges which are taken infinitely often when running the given `word`
    /// on the transition system, starting from the initial state. If the run is unsuccessful, `None` is returned.
    fn recurrent_edge_colors_from<W, Idx>(
        &self,
        word: W,
        origin: Idx,
    ) -> Option<impl Iterator<Item = Self::EdgeColor>>
    where
        W: OmegaWord<SymbolOf<Self>>,
        Idx: Indexes<Self>,
    {
        self.omega_run_from(word, origin)
            .ok()
            .map(|p| p.into_recurrent_edge_colors())
    }

    /// Gives an iterator that emits the colors of edges which are taken infinitely often when running the given `word`
    /// on the transition system, starting from the initial state. If the run is unsuccessful, `None` is returned.
    fn recurrent_edge_colors<W>(&self, word: W) -> Option<impl Iterator<Item = Self::EdgeColor>>
    where
        W: OmegaWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.recurrent_edge_colors_from(word, self.initial())
    }

    /// Returns a [`Vec`] containing the state indices that are visited when running the given `word`
    /// on the transition system, starting from the initial state. This may include states that are
    /// visited only finitely often. If the run is unsuccessful, `None` is returned.
    // Todo: once RTTIT is stabilized (1.72), we should return an iterator.
    fn visited_state_sequence_from<W, Idx>(
        &self,
        word: W,
        origin: Idx,
    ) -> Option<Vec<Self::StateIndex>>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Idx: Indexes<Self>,
    {
        self.finite_run_from(word, origin)
            .ok()
            .map(|p| p.state_sequence().collect())
    }

    /// Returns a [`Vec`] containing the state indices that are visited when running the given `word`
    /// on the transition system, starting from the initial state. This may include states that are
    /// visited only finitely often. If the run is unsuccessful, `None` is returned.
    fn visited_state_sequence<W>(&self, word: W) -> Option<Vec<Self::StateIndex>>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.visited_state_sequence_from(word, self.initial())
    }

    /// Returns a [`Vec`] containing the state colors that are visited when running the given `word`
    /// on the transition system, starting from the initial state. This may include states that are
    /// visited only finitely often. If the run is unsuccessful, `None` is returned.
    fn visited_state_colors_from<W, Idx>(
        &self,
        word: W,
        origin: Idx,
    ) -> Option<Vec<Self::StateColor>>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Idx: Indexes<Self>,
    {
        self.finite_run_from(word, origin)
            .ok()
            .map(|p| p.state_colors().cloned().collect())
    }

    /// Returns a [`Vec`] containing the state colors that are visited when running the given `word`
    /// on the transition system, starting from the initial state. This may include states that are
    /// visited only finitely often. If the run is unsuccessful, `None` is returned.
    fn visited_state_colors<W>(&self, word: W) -> Option<Vec<Self::StateColor>>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.visited_state_colors_from(word, self.initial())
    }

    /// Returns a [`Vec`] containing the edge colors that are visited when running the given `word`
    /// on the transition system, starting from the initial state. This may include edges that are
    /// visited only finitely often. If the run is unsuccessful, `None` is returned.
    fn visited_edge_colors_from<W, Idx>(&self, word: W, origin: Idx) -> Option<Vec<Self::EdgeColor>>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Idx: Indexes<Self>,
    {
        self.finite_run_from(word, origin)
            .ok()
            .map(|p| p.edge_colors().cloned().collect())
    }

    /// Returns a [`Vec`] containing the edge colors that are visited when running the given `word`
    /// on the transition system, starting from the initial state. This may include edges that are
    /// visited only finitely often. If the run is unsuccessful, `None` is returned.
    fn visited_edge_colors<W>(&self, word: W) -> Option<Vec<Self::EdgeColor>>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.visited_edge_colors_from(word, self.initial())
    }

    /// Returns the color of the last edge that is taken when running the given `word` on the transition system,
    /// starting from the state indexed by `origin`. If the run is unsuccessful, `None` is returned.
    fn last_edge_color_from<W, Idx>(&self, word: W, origin: Idx) -> Option<Self::EdgeColor>
    where
        Idx: Indexes<Self>,
        W: FiniteWord<SymbolOf<Self>>,
    {
        self.finite_run_from(word, origin.to_index(self)?)
            .ok()
            .and_then(|p| p.last_transition_color().cloned())
    }

    /// Returns the color of the last edge that is taken when running the given `word` on the transition system,
    /// starting from the initial state. If the run is unsuccessful, `None` is returned.
    fn last_edge_color<W>(&self, word: W) -> Option<Self::EdgeColor>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.last_edge_color_from(word, self.initial())
    }

    /// Checks whether `self` is complete, meaning every state has a transition for every symbol
    /// of the alphabet.
    fn is_complete(&self) -> bool {
        for q in self.state_indices() {
            if !self
                .alphabet()
                .universe()
                .all(|sym| self.transition(q, sym).is_some())
            {
                return false;
            }
        }
        true
    }
    /// Runs the given `word` on the transition system, starting in the initial state.
    #[allow(clippy::type_complexity)]
    fn omega_run<W>(
        &self,
        word: W,
    ) -> OmegaRunResult<Self::Alphabet, Self::StateIndex, Self::StateColor, Self::EdgeColor>
    where
        W: OmegaWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.omega_run_from(word, self.initial())
    }

    /// Runs the given `word` on the transition system, starting from `state`.
    #[allow(clippy::type_complexity)]
    fn omega_run_from<W, Idx>(
        &self,
        word: W,
        origin: Idx,
    ) -> OmegaRunResult<Self::Alphabet, Self::StateIndex, Self::StateColor, Self::EdgeColor>
    where
        Idx: Indexes<Self>,
        W: OmegaWord<SymbolOf<Self>>,
    {
        assert!(!word.cycle().is_empty(), "word must be infinite");
        let origin = origin
            .to_index(self)
            .expect("run must start in state that exists");
        let mut path = self.finite_run_from(word.spoke(), origin)?;
        let mut position = path.len();
        let mut seen = Map::default();

        loop {
            match seen.insert(path.reached(), position) {
                Some(p) => {
                    return Ok(path.loop_back_to(p));
                }
                None => match self.finite_run_from(word.cycle(), path.reached()) {
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

    /// Returns a string representation of the transition table of the transition system.
    fn build_transition_table<'a, SD, ED>(
        &'a self,
        state_decorator: SD,
        edge_decorator: ED,
    ) -> String
    where
        SD: Fn(Self::StateIndex, StateColor<Self>) -> String,
        ED: Fn(Self::EdgeRef<'a>) -> String,
    {
        let mut builder = tabled::builder::Builder::default();
        builder.push_record(
            std::iter::once("State".to_string())
                .chain(self.alphabet().universe().map(|s| format!("{:?}", s))),
        );
        for id in self.state_indices().sorted() {
            let mut row = vec![format!(
                "{}",
                state_decorator(
                    id,
                    self.state_color(id)
                        .expect("Every state should be colored!")
                )
            )];
            for sym in self.alphabet().universe() {
                if let Some(edge) = self.transition(id, sym) {
                    row.push(edge_decorator(edge));
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

    /// Returns the color of the state that is reached when running `word` from the state indexed by `from`.
    /// If the run is unsuccessful, `None` is returned.
    fn reached_state_color_from<W, Idx>(&self, word: W, from: Idx) -> Option<Self::StateColor>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Idx: Indexes<Self>,
    {
        self.finite_run_from(word, from)
            .ok()
            .map(|p| p.reached_state_color())
    }

    /// Returns the color of the state that is reached when running `word` from the initial state. If the run
    /// is unsuccessful, `None` is returned.
    fn reached_state_color<W>(&self, word: W) -> Option<Self::StateColor>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.reached_state_color_from(word, self.initial())
    }

    /// Returns the state that is reached by running the given `word` on the transition system,
    /// starting from the initial state. If the run is unsuccessful, `None` is returned.
    fn reached_state_index<W>(&self, word: W) -> Option<Self::StateIndex>
    where
        Self: Pointed,
        W: FiniteWord<SymbolOf<Self>>,
    {
        self.reached_state_index_from(word, self.initial())
    }

    /// Tries to run the given `word` starting in the state indexed by `origin`. If
    /// no state is indexed, then `None` is immediately returned. Otherwise, the
    /// word is run and the index of the reached state is returned. If the run is
    /// unsuccessful, the function returns `None`.
    fn reached_state_index_from<I, W>(&self, word: W, origin: I) -> Option<Self::StateIndex>
    where
        Self: Sized,
        I: Indexes<Self>,
        W: FiniteWord<SymbolOf<Self>>,
    {
        self.finite_run_from(word, origin).ok().map(|p| p.reached())
    }

    /// Builds a new [`RightCongruence`] from `self`, which is like viewing only the right congruence underlying
    /// `self`. This procedure erases the state and edge colors.
    fn collect_right_congruence_bare(&self) -> RightCongruence<Self::Alphabet>
    where
        Self: Pointed,
    {
        RightCongruence::from_ts(self.erase_state_colors().erase_edge_colors())
    }

    /// Builds a new [`RightCongruence`] from `self`, which is like viewing only the right congruence underlying
    /// `self`. This procedure keeps the state and edge colors.
    fn collect_right_congruence(
        &self,
    ) -> RightCongruence<Self::Alphabet, Self::StateColor, Self::EdgeColor>
    where
        Self: Pointed,
    {
        RightCongruence::from_ts(self)
    }

    /// Collects `self` into a new [`DTS`] over the same alphabet. This is used, for example, after a chain of
    /// manipulations on a transition system, to obtain a condensed version that is then faster to work with.
    ///
    /// By default, the implementation is naive and slow, it simply inserts all states one after the other and
    /// subsequently inserts all transitions, see [`Sproutable::collect_from`] for details.
    fn collect_dts(self) -> DTS<Self::Alphabet, Self::StateColor, Self::EdgeColor> {
        use crate::ts::Sproutable;
        let (ts, _map) = DTS::collect_from(self);
        ts
    }

    /// Collects `self` into a new [`HashTs`] over the same alphabet. This is used, for example, after a chain of
    /// manipulations on a transition system, to obtain a condensed version that is then faster to work with.
    ///
    /// By default, the implementation is naive and slow, it simply inserts all states one after the other and
    /// subsequently inserts all transitions, see [`Sproutable::collect_from`] for details.
    fn collect_hash_ts(self) -> IntoHashTs<Self>
    where
        EdgeColor<Self>: Hash + Eq,
        StateColor<Self>: Hash + Eq,
    {
        use crate::ts::Sproutable;
        let (ts, _map) = HashTs::collect_from(self);
        ts
    }

    /// Collects `self` into a new transition system. This procedure also completes the collected transition
    /// system with a sink (a state that cannot be left) and for each state of the ts that does not have an
    /// outgoing transition on some symbol, a new transition into the sink is added.
    fn collect_complete_with_initial(
        &self,
        sink_color: Self::StateColor,
        edge_color: Self::EdgeColor,
    ) -> Initialized<DTS<Self::Alphabet, Self::StateColor, Self::EdgeColor>>
    where
        Self: Pointed,
        Self::Alphabet: IndexedAlphabet,
    {
        let mut out: Initialized<DTS<_, _, _>> = self.trim_collect();
        out.complete_with_colors(sink_color, edge_color);
        out
    }

    /// Variant of [`Self::collect()`] which also considers the initial state.
    fn collect_pointed<Ts>(&self) -> (Initialized<Ts>, Bijection<Self::StateIndex, Ts::StateIndex>)
    where
        Self: Pointed,
        Ts: Sproutable<Alphabet = Self::Alphabet>,
        EdgeColor<Self>: Into<EdgeColor<Ts>>,
        StateColor<Self>: Into<StateColor<Ts>>,
    {
        let (ts, map) = self.collect::<Ts>();
        (
            ts.with_initial(
                *map.get_by_left(&self.initial())
                    .expect("Initial state did not get collected"),
            ),
            map,
        )
    }

    /// Returns true if `self` is accessible, meaning every state is reachable from the initial state.
    /// This is done by counting whether the number of minimal representatives matches the number of states.
    fn is_accessible(&self) -> bool
    where
        Self: Pointed,
    {
        self.size() == self.minimal_representatives().count()
    }

    /// Collects into a transition system of type `Ts`, but only considers states that
    /// are reachable from the initial state. Naturally, this means that `self` must
    /// be a pointed transition system.
    fn trim_collect(&self) -> Initialized<DTS<Self::Alphabet, Self::StateColor, Self::EdgeColor>>
    where
        Self: Pointed,
    {
        let reachable_indices = self.reachable_state_indices().collect::<Set<_>>();
        let restricted = self.restrict_state_indices(|idx| reachable_indices.contains(&idx));
        let (out, _map) = restricted.collect_pointed();
        out
    }

    /// Collects `self` into a new transition system of type `Ts` with the same alphabet, state indices
    /// and edge colors. **This does not consider the initial state.**
    fn collect<Ts>(&self) -> (Ts, Bijection<Self::StateIndex, Ts::StateIndex>)
    where
        Ts: Sproutable<Alphabet = Self::Alphabet>,
        EdgeColor<Self>: Into<EdgeColor<Ts>>,
        StateColor<Self>: Into<StateColor<Ts>>,
    {
        Sproutable::collect_from(self)
    }

    /// Collects `self` into a new transition system of type `Ts` with the same alphabet, state indices
    /// and edge colors.
    fn collect_old<
        Ts: TransitionSystem<
                StateColor = Self::StateColor,
                EdgeColor = Self::EdgeColor,
                Alphabet = Self::Alphabet,
            > + super::Sproutable,
    >(
        &self,
    ) -> Ts {
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
                if let Some(edge) = self.transition(index, sym) {
                    ts.add_edge(
                        *map.get(&index).unwrap(),
                        <Self::Alphabet as Alphabet>::expression(sym),
                        *map.get(&edge.target()).unwrap(),
                        edge.color().clone(),
                    );
                }
            }
        }
        ts
    }
}

impl<D: Deterministic> Deterministic for &D {
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        D::transition(self, state.to_index(self)?, symbol)
    }
}

impl<D: Deterministic> Deterministic for &mut D {
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        D::transition(self, state.to_index(self)?, symbol)
    }
}

impl<A: Alphabet, Q: Clone, C: Clone> Deterministic for RightCongruence<A, Q, C> {
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        self.ts().transition(state.to_index(self)?, symbol)
    }
    fn edge_color<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.ts().edge_color(state.to_index(self)?, expression)
    }
}

impl<A: Alphabet, IdType: IndexType, Q: Clone, C: Hash + Eq + Clone> Deterministic
    for HashTs<A, Q, C, IdType>
{
    fn edge_color<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.raw_state_map()
            .get(&state.to_index(self)?)
            .and_then(|o| o.edge_map().get(expression).map(|(_, c)| c.clone()))
    }

    fn transition<X: Indexes<Self>>(
        &self,
        state: X,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        let source = state.to_index(self)?;
        self.raw_state_map()
            .get(&source)
            .and_then(|o| A::search_edge(o.edge_map(), symbol))
            .map(|(e, (q, c))| EdgeReference::new(source, e, c, *q))
    }
}

impl<L, R> Deterministic for MatchingProduct<L, R>
where
    L: Deterministic,
    R: Deterministic<Alphabet = L::Alphabet>,
    L::StateColor: Clone,
    R::StateColor: Clone,
{
    fn edge_color<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        let ProductIndex(l, r) = state.to_index(self)?;
        let left = self.0.edge_color(l, expression)?;
        let right = self.1.edge_color(r, expression)?;
        Some((left, right))
    }

    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        let ProductIndex(l, r) = state.to_index(self)?;

        let ll = self.0.transition(l, symbol)?;
        let rr = self.1.transition(r, symbol)?;
        Some(ProductTransition::new(
            ProductIndex(l, r),
            ll.expression(),
            (ll.color(), rr.color()),
            ProductIndex(ll.target(), rr.target()),
        ))
    }
}

impl<D, Ts, F> Deterministic for MapStateColor<Ts, F>
where
    D: Clone,
    Ts: Deterministic,
    F: Fn(Ts::StateColor) -> D,
{
    fn collect_dts(self) -> DTS<Self::Alphabet, Self::StateColor, Self::EdgeColor> {
        let (ts, f) = self.into_parts();
        let (alphabet, states, edges) = ts.collect_dts().into_parts();
        let states = states.into_iter().map(|q| q.recolor(&f)).collect();
        DTS::from_parts(alphabet, states, edges)
    }

    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        self.ts().transition(state.to_index(self)?, symbol)
    }
}

impl<D, Ts, F> Deterministic for MapEdgeColor<Ts, F>
where
    D: Clone,
    Ts: Deterministic,
    F: Fn(Ts::EdgeColor) -> D,
{
    fn collect_dts(self) -> DTS<Self::Alphabet, Self::StateColor, Self::EdgeColor> {
        let (ts, f) = self.into_parts();
        let (alphabet, states, edges) = ts.collect_dts().into_parts();
        let edges = edges.into_iter().map(|e| e.recolor(&f)).collect();
        DTS::from_parts(alphabet, states, edges)
    }

    fn edge_color<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.ts()
            .edge_color(state.to_index(self)?, expression)
            .map(|c| (self.f())(c))
    }

    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        Some(MappedTransition::new(
            self.ts().transition(state.to_index(self)?, symbol)?,
            self.f(),
        ))
    }
}

impl<Ts: Deterministic, F> Deterministic for RestrictByStateIndex<Ts, F>
where
    F: StateIndexFilter<Ts::StateIndex>,
{
    fn edge_color<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        let state = state.to_index(self)?;
        self.ts()
            .edge_color(state, expression)
            .filter(|_| (self.filter()).is_unmasked(state))
    }
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        let q = state.to_index(self)?;
        self.ts()
            .transition(q, symbol)
            .filter(|t| self.filter().is_unmasked(q) && self.filter().is_unmasked(t.target()))
    }
}

impl<Ts, D, F> Deterministic for MapEdges<Ts, F>
where
    Ts: Deterministic,
    D: Clone,
    F: Fn(Ts::StateIndex, &ExpressionOf<Ts>, Ts::EdgeColor, Ts::StateIndex) -> D,
{
    fn transition<Idx: crate::ts::transition_system::Indexes<Self>>(
        &self,
        state: Idx,
        symbol: crate::prelude::SymbolOf<Self>,
    ) -> Option<Self::EdgeRef<'_>> {
        Some(MappedEdge::new(
            self.ts().transition(state.to_index(self)?, symbol)?,
            state.to_index(self)?,
            self.f(),
        ))
    }
}
