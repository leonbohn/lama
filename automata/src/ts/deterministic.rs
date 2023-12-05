#[cfg(test)]
use pretty_assertions::{assert_eq, assert_ne};
use std::marker::PhantomData;

use itertools::Itertools;

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
use super::sproutable::{IndexedAlphabet, Sproutable};
use super::Path;

pub trait Deterministic: TransitionSystem {
    /// For a given `state` and `symbol`, returns the transition that is taken, if it exists.
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>>;

    /// Returns just the [`Self::StateIndex`] of the successor that is reached on the given `symbol`
    /// from `state`. If no suitable transition exists, `None` is returned.
    fn successor_index(
        &self,
        state: Self::StateIndex,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::StateIndex> {
        self.transition(state, symbol).map(|t| t.target())
    }

    /// Returns the color of an edge starting in the given `state` and labeled with the given
    /// `expression`, if it exists. Otherwise, `None` is returned.
    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        // TODO: this is horrible!
        let mut symbols = expression.symbols();
        let sym = symbols.next().unwrap();
        assert_eq!(symbols.next(), None);
        Some(self.transition(state, sym)?.color())
    }

    fn minimal_representative<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Vec<SymbolOf<Self>>>
    where
        Self: Pointed,
    {
        let q = state.to_index(self)?;
        self.minimal_representatives()
            .find_map(|(rep, p)| if p == q { Some(rep) } else { None })
    }

    /// Runs the given `word` on the transition system, starting from the initial state. The result is
    /// - [`Ok`] if the run is successful (i.e. for all symbols of `word` a suitable transition
    ///  can be taken),
    /// - [`Err`] if the run is unsuccessful, meaning a symbol is encountered for which no
    /// transition exists.
    #[allow(clippy::type_complexity)]
    fn finite_run<W: FiniteWord<SymbolOf<Self>>>(
        &self,
        word: W,
    ) -> Result<Path<Self::Alphabet, Self::StateIndex>, Path<Self::Alphabet, Self::StateIndex>>
    where
        Self: Pointed,
    {
        self.finite_run_from(word, self.initial())
    }

    fn reached_from<W, Idx>(&self, word: W, origin: Idx) -> Option<Self::StateIndex>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Idx: Indexes<Self>,
    {
        self.finite_run_from(word, origin.to_index(self)?)
            .ok()
            .map(|x| x.reached())
    }

    fn reached<W>(&self, word: W) -> Option<Self::StateIndex>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.finite_run_from(word, self.initial())
            .ok()
            .map(|x| x.reached())
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
    ) -> Result<Path<Self::Alphabet, Self::StateIndex>, Path<Self::Alphabet, Self::StateIndex>>
    where
        Self: Sized,
        W: FiniteWord<SymbolOf<Self>>,
        Idx: Indexes<Self>,
    {
        let mut current = origin
            .to_index(self)
            .expect("run must start in state that exists");
        let mut path = Path::with_capacity(current, word.len());
        for symbol in word.symbols() {
            if let Some(o) = path.extend_in(&self, symbol) {
                current = o.target();
            } else {
                return Err(path);
            }
        }
        Ok(path)
    }

    fn recurrent_state_indices_from<W: OmegaWord<SymbolOf<Self>>, Idx: Indexes<Self>>(
        &self,
        word: W,
        origin: Idx,
    ) -> Option<Set<Self::StateIndex>> {
        self.omega_run_from(word, origin)
            .ok()
            .map(|p| p.recurrent_state_indices())
    }

    fn recurrent_state_indices<W: OmegaWord<SymbolOf<Self>>>(
        &self,
        word: W,
    ) -> Option<Set<Self::StateIndex>>
    where
        Self: Pointed,
    {
        self.recurrent_state_indices_from(word, self.initial())
    }

    fn recurrent_state_colors_from<W: OmegaWord<SymbolOf<Self>>, Idx: Indexes<Self>>(
        &self,
        word: W,
        origin: Idx,
    ) -> Option<Set<Self::StateColor>> {
        self.omega_run_from(word, origin)
            .ok()
            .map(|p| p.recurrent_state_colors(self))
    }

    fn recurrent_state_colors<W: OmegaWord<SymbolOf<Self>>>(
        &self,
        word: W,
    ) -> Option<Set<Self::StateColor>>
    where
        Self: Pointed,
    {
        self.recurrent_state_colors_from(word, self.initial())
    }

    fn infinity_set_from<W, Idx>(&self, word: W, origin: Idx) -> Option<Set<Self::EdgeColor>>
    where
        W: OmegaWord<SymbolOf<Self>>,
        Idx: Indexes<Self>,
    {
        self.omega_run_from(word, origin)
            .ok()
            .map(|p| p.infinity_set(self))
    }

    fn infinity_set<W>(&self, word: W) -> Option<Set<Self::EdgeColor>>
    where
        W: OmegaWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.infinity_set_from(word, self.initial())
    }

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

    fn visited_state_sequence<W>(&self, word: W) -> Option<Vec<Self::StateIndex>>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.visited_state_sequence_from(word, self.initial())
    }

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
            .map(|p| p.state_colors(self).collect())
    }

    fn visited_state_colors<W>(&self, word: W) -> Option<Vec<Self::StateColor>>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.visited_state_colors_from(word, self.initial())
    }

    fn visited_edge_colors_from<W, Idx>(&self, word: W, origin: Idx) -> Option<Vec<Self::EdgeColor>>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Idx: Indexes<Self>,
    {
        self.finite_run_from(word, origin)
            .ok()
            .map(|p| p.edge_colors(self).collect())
    }

    fn visited_edge_colors<W>(&self, word: W) -> Option<Vec<Self::EdgeColor>>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Self: Pointed,
    {
        self.visited_edge_colors_from(word, self.initial())
    }

    fn last_edge_color_from<W, Idx>(&self, word: W, origin: Idx) -> Option<Self::EdgeColor>
    where
        Idx: Indexes<Self>,
        W: FiniteWord<SymbolOf<Self>>,
    {
        self.finite_run_from(word, origin.to_index(self)?)
            .ok()
            .and_then(|p| p.last_transition_color(self))
    }

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
    ) -> Result<Lasso<Self::Alphabet, Self::StateIndex>, Path<Self::Alphabet, Self::StateIndex>>
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
    ) -> Result<Lasso<Self::Alphabet, Self::StateIndex>, Path<Self::Alphabet, Self::StateIndex>>
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
    fn build_transition_table<SD>(&self, state_decorator: SD) -> String
    where
        SD: Fn(Self::StateIndex, StateColor<Self>) -> String,
    {
        let mut builder = tabled::builder::Builder::default();
        builder.set_header(
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
                    row.push(format!("{} : {}", edge.target(), edge.color().show()));
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

    fn reached_state_color_from<W, Idx>(&self, word: W, from: Idx) -> Option<Self::StateColor>
    where
        W: FiniteWord<SymbolOf<Self>>,
        Idx: Indexes<Self>,
    {
        self.finite_run_from(word, from)
            .ok()
            .map(|p| p.reached_state_color(self))
    }

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

    fn collect_right_congruence(
        &self,
    ) -> RightCongruence<Self::Alphabet, Self::StateColor, Self::EdgeColor>
    where
        Self: Pointed,
    {
        RightCongruence::from_ts(self)
    }

    /// Collects `self` into a new [`BTS`] with the same alphabet, state colors and edge colors.
    fn collect_ts(&self) -> BTS<Self::Alphabet, Self::StateColor, Self::EdgeColor> {
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

    fn collect_complete_with_initial(
        &self,
        sink_color: Self::StateColor,
        edge_color: Self::EdgeColor,
    ) -> WithInitial<BTS<Self::Alphabet, Self::StateColor, Self::EdgeColor>>
    where
        Self: Pointed,
        Self::Alphabet: IndexedAlphabet,
        Self::StateColor: Default,
    {
        let mut out: WithInitial<BTS<_, _, _>> = self.collect_initialized();
        out.complete_with_colors(sink_color, edge_color);
        out
    }

    /// Variant of [`Self::collect()`] which also considers the initial state.
    fn collect_with_initial<Ts>(&self) -> Ts
    where
        Self: Pointed,
        Ts: TransitionSystem<
                StateColor = Self::StateColor,
                EdgeColor = Self::EdgeColor,
                Alphabet = Self::Alphabet,
            > + super::Sproutable
            + Pointed,
    {
        let mut ts = Ts::new_for_alphabet(self.alphabet().clone());
        ts.set_initial_color(self.initial_color());

        let (l, r) = self.state_indices().filter(|o| o != &self.initial()).tee();
        let map: Map<Self::StateIndex, Ts::StateIndex> = l
            .zip(ts.extend_states(r.map(|q| self.state_color(q).unwrap())))
            .chain(std::iter::once((self.initial(), ts.initial())))
            .collect();
        for index in self.state_indices() {
            let q = *map.get(&index).unwrap();
            self.edges_from(index).unwrap().for_each(|tt| {
                ts.add_edge(
                    q,
                    tt.expression().clone(),
                    *map.get(&tt.target()).unwrap(),
                    tt.color(),
                );
            });
        }
        ts
    }

    /// Collects into a transition system of type `Ts`, but only considers states that
    /// are reachable from the initial state. Naturally, this means that `self` must
    /// be a pointed transition system.
    fn trim_collect(&self) -> WithInitial<BTS<Self::Alphabet, Self::StateColor, Self::EdgeColor>>
    where
        Self: Pointed,
    {
        let mut ts = BTS::new_for_alphabet(self.alphabet().clone());
        let mut map = Map::default();
        let reachable = self.reachable_state_indices().collect_vec();
        for idx in &reachable {
            map.insert(
                idx,
                ts.add_state(self.state_color(*idx).expect("State must exist")),
            );
        }
        for idx in &reachable {
            for edge in self.edges_from(*idx).unwrap() {
                ts.add_edge(
                    *map.get(idx).unwrap(),
                    edge.expression().clone(),
                    *map.get(&edge.target()).unwrap(),
                    edge.color(),
                );
            }
        }
        ts.with_initial(*map.get(&self.initial()).unwrap())
    }

    fn collect_initialized(
        self,
    ) -> WithInitial<BTS<Self::Alphabet, Self::StateColor, Self::EdgeColor>>
    where
        Self: Pointed,
        Self::StateColor: Default,
    {
        self.collect_with_initial()
    }

    /// Collects `self` into a new transition system of type `Ts` with the same alphabet, state indices
    /// and edge colors.
    fn collect<
        Ts: TransitionSystem<
                StateColor = Self::StateColor,
                EdgeColor = Self::EdgeColor,
                Alphabet = Self::Alphabet,
            > + super::Sproutable,
    >(
        &self,
    ) -> Ts {
        let mut ts = Ts::new_for_alphabet(self.alphabet().clone());

        let (l, r) = self.state_indices().tee();
        let map: Map<_, _> = l
            .zip(ts.extend_states(r.map(|q| self.state_color(q).unwrap())))
            .collect();
        for index in self.state_indices() {
            let q = *map.get(&index).unwrap();
            self.edges_from(index).unwrap().for_each(|tt| {
                ts.add_edge(
                    q,
                    tt.expression().clone(),
                    *map.get(&tt.target()).unwrap(),
                    tt.color(),
                );
            });
        }
        ts
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
    ) -> Option<Self::TransitionRef<'_>> {
        D::transition(self, state.to_index(self)?, symbol)
    }
}

impl<D: Deterministic> Deterministic for &mut D {
    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        D::transition(self, state.to_index(self)?, symbol)
    }
}

impl<A: Alphabet, Q: Color, C: Color> Deterministic for RightCongruence<A, Q, C> {
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
    ) -> Option<crate::ts::EdgeColor<Self>> {
        self.ts().edge_color(state, expression)
    }
}

impl<A: Alphabet, Idx: IndexType, Q: Color, C: Color> Deterministic for BTS<A, Q, C, Idx> {
    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.raw_state_map()
            .get(&state)
            .and_then(|o| o.edge_map().get(expression).map(|(_, c)| c.clone()))
    }

    fn transition<X: Indexes<Self>>(
        &self,
        state: X,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        self.raw_state_map()
            .get(&state.to_index(self)?)
            .and_then(|o| A::search_edge(o.edge_map(), symbol))
    }
}

impl<L, R> Deterministic for MatchingProduct<L, R>
where
    L: Deterministic,
    R: Deterministic<Alphabet = L::Alphabet>,
    L::StateColor: Clone,
    R::StateColor: Clone,
{
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

    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        let ProductIndex(l, r) = state.to_index(self)?;

        let ll = self.0.transition(l, symbol)?;
        let rr = self.1.transition(r, symbol)?;
        Some(ProductTransition::new(
            ll.expression(),
            ProductIndex(ll.target(), rr.target()),
            (ll.color(), rr.color()),
        ))
    }
}

impl<D, Ts, F> Deterministic for MapStateColor<Ts, F>
where
    D: Color,
    Ts: Deterministic,
    F: Fn(Ts::StateColor) -> D,
{
    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.ts().edge_color(state, expression)
    }

    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        self.ts().transition(state.to_index(self)?, symbol)
    }
}

impl<D, Ts, F> Deterministic for MapEdgeColor<Ts, F>
where
    D: Color,
    Ts: Deterministic,
    F: Fn(Ts::EdgeColor) -> D,
{
    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<EdgeColor<Self>> {
        self.ts()
            .edge_color(state, expression)
            .map(|c| (self.f())(c))
    }

    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
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
    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<crate::ts::EdgeColor<Self>> {
        self.ts()
            .edge_color(state, expression)
            .filter(|_| (self.filter()).is_unmasked(state))
    }

    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        let q = state.to_index(self)?;
        self.ts()
            .transition(q, symbol)
            .filter(|t| self.filter().is_unmasked(q) && self.filter().is_unmasked(t.target()))
    }
}

impl<Ts, D, F> Deterministic for MapEdges<Ts, F>
where
    Ts: Deterministic,
    D: Color,
    F: Fn(Ts::StateIndex, &ExpressionOf<Ts>, Ts::EdgeColor, Ts::StateIndex) -> D,
{
    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &ExpressionOf<Self>,
    ) -> Option<crate::ts::EdgeColor<Self>> {
        todo!()
    }

    fn transition<Idx: crate::ts::transition_system::Indexes<Self>>(
        &self,
        state: Idx,
        symbol: crate::prelude::SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        Some(MappedEdge::new(
            self.ts().transition(state.to_index(self)?, symbol)?,
            state.to_index(self)?,
            self.f(),
        ))
    }
}
