use std::{
    collections::{BTreeSet, VecDeque},
    fmt::Debug,
};

use itertools::Itertools;

use crate::{
    alphabet::{Empty, HasAlphabet, SymbolOf},
    ts::{finite::SeenColors, CanInduce, FiniteState, HasFiniteStates, IndexType, Sproutable, BTS},
    Alphabet, Map, Pointed, Set, TransitionSystem,
};

use super::IsTransition;

#[derive(Debug, Clone)]
struct TarjanData {
    rootindex: Option<usize>,
}

/// Struct that is used to compute the strongly connected components of a transition system.
/// Many thanks go out to the authors of the petgraph crate <3.
#[derive(Debug, Clone)]
pub struct Tarjan<Idx> {
    index: usize,
    component_count: usize,
    stack: Vec<Idx>,
    data: Map<Idx, TarjanData>,
}

impl<Idx: IndexType> Tarjan<Idx> {
    pub fn new() -> Self {
        Self {
            index: 0,
            component_count: usize::MAX,
            stack: Vec::new(),
            data: Map::default(),
        }
    }

    pub fn visit<Ts, F>(&mut self, ts: &Ts, v: Idx, f: &mut F)
    where
        Ts: TransitionSystem<StateIndex = Idx>,
        F: FnMut(&[Idx]),
    {
        let mut node_v_is_root = true;
        let node_v_index = self.index;
        self.data.insert(
            v,
            TarjanData {
                rootindex: Some(node_v_index),
            },
        );
        self.index += 1;

        for &sym in ts.alphabet().universe() {
            if let Some(w) = ts.successor_index(v, sym) {
                if self.data.get(&w).and_then(|data| data.rootindex).is_none() {
                    self.visit(ts, w, f);
                }
                let w_index = self.data.get(&w).unwrap().rootindex;
                let mut v_mut = self.data.get_mut(&v).unwrap();
                if w_index < v_mut.rootindex {
                    v_mut.rootindex = w_index;
                    node_v_is_root = false;
                }
            }
        }

        if node_v_is_root {
            let mut adjust = 1;
            let c = self.component_count;
            let node_data = &mut self.data;

            let start = self
                .stack
                .iter()
                .rposition(|w| {
                    if node_data.get(&v).unwrap().rootindex > node_data.get(w).unwrap().rootindex {
                        true
                    } else {
                        node_data.get_mut(w).unwrap().rootindex = Some(c);
                        adjust += 1;
                        false
                    }
                })
                .map(|x| x + 1)
                .unwrap_or_default();

            node_data.get_mut(&v).unwrap().rootindex = Some(c);
            self.stack.push(v);
            f(&self.stack[start..]);
            self.stack.truncate(start);
            self.index -= adjust;
            self.component_count -= 1;
        } else {
            self.stack.push(v);
        }
    }

    pub fn execute<Ts, F>(&mut self, ts: &Ts, mut f: F)
    where
        Ts: TransitionSystem<StateIndex = Idx> + FiniteState,
        F: FnMut(&[Idx]),
    {
        self.data.clear();

        for q in ts.state_indices() {
            if let (Some(TarjanData { rootindex: None }) | None) = self.data.get(&q) {
                self.visit(ts, q, &mut f);
            }
        }
        debug_assert!(self.stack.is_empty())
    }
}

#[derive(Debug, Clone)]
pub struct Scc<'a, Ts: TransitionSystem>(&'a Ts, Vec<Ts::StateIndex>);

impl<'a, Ts: TransitionSystem> IntoIterator for Scc<'a, Ts> {
    type IntoIter = std::vec::IntoIter<Ts::StateIndex>;
    type Item = Ts::StateIndex;
    fn into_iter(self) -> Self::IntoIter {
        self.1.into_iter()
    }
}

impl<'a, Ts: TransitionSystem> std::ops::Deref for Scc<'a, Ts> {
    type Target = Vec<Ts::StateIndex>;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<'a, Ts: TransitionSystem> PartialEq for Scc<'a, Ts> {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl<'a, Ts: TransitionSystem> Eq for Scc<'a, Ts> {}
impl<'a, Ts: TransitionSystem> PartialOrd for Scc<'a, Ts> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<'a, Ts: TransitionSystem> Ord for Scc<'a, Ts> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.1[0].cmp(&other.1[0])
    }
}
impl<'a, Ts: TransitionSystem> std::hash::Hash for Scc<'a, Ts> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.1.hash(state);
    }
}

impl<'a, Ts: TransitionSystem> Scc<'a, Ts> {
    pub fn new(ts: &'a Ts, indices: Vec<Ts::StateIndex>) -> Self {
        assert!(!indices.is_empty(), "Cannot have empty SCC!");
        let mut indices = indices;
        indices.sort();
        debug_assert!(
            indices
                .iter()
                .collect::<std::collections::HashSet<_>>()
                .len()
                == indices.len()
        );
        Self(ts, indices)
    }

    pub fn ts(&self) -> &'a Ts {
        self.0
    }

    pub fn colors(&self) -> Option<Vec<Ts::StateColor>> {
        debug_assert!(!self.is_empty());
        let maximal_word = self.maximal_word()?;
        let SeenColors(colors) = self.ts().run(&maximal_word, self.1[0]).ok()?.induce();
        Some(colors)
    }

    pub fn maximal_word(&self) -> Option<Vec<SymbolOf<Ts>>> {
        let ts = self.0;
        debug_assert!(self.len() > 0);

        let mut should_continue = false;
        let mut queue = Map::default();
        for state in self.iter() {
            for &sym in ts.alphabet().universe() {
                if let Some(succ) = ts.successor_index(*state, sym) {
                    if self.contains(&succ) {
                        should_continue = true;
                        queue
                            .entry(*state)
                            .or_insert_with(BTreeSet::new)
                            .insert(sym);
                    }
                }
            }
        }

        // This guards against the case where no transitions are available
        if !should_continue {
            return None;
        }

        let mut current = self[0];
        let mut word = Vec::new();

        while !queue.is_empty() {
            if queue.contains_key(&current) {
                if queue.get(&current).unwrap().is_empty() {
                    queue.remove(&current);
                    continue;
                } else {
                    let sym = *queue.get(&current).unwrap().iter().next().unwrap();
                    queue.get_mut(&current).unwrap().remove(&sym);
                    word.push(sym);
                    current = ts.successor_index(current, sym).unwrap();
                }
            } else {
                let q = *queue.keys().next().unwrap();
                if queue.get(&q).unwrap().is_empty() {
                    queue.remove(&q);
                    continue;
                }

                word.extend(
                    ts.word_from_to(current, q)
                        .expect("Such a word must exist as both states are in the same SCC"),
                );
                current = q;
            }
        }

        Some(word)
    }

    pub fn len(&self) -> usize {
        self.1.len()
    }

    pub fn is_singleton(&self) -> bool {
        self.1.len() == 1
    }

    pub fn is_trivial(&self) -> bool {
        !self.is_nontrivial()
    }

    pub fn is_nontrivial(&self) -> bool {
        self.1.iter().any(|&q| {
            self.0.alphabet().universe().any(|&sym| {
                self.0
                    .successor_index(q, sym)
                    .map(|q| self.contains(&q))
                    .unwrap_or(false)
            })
        })
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SccDecomposition<'a, Ts: TransitionSystem + FiniteState>(&'a Ts, Vec<Scc<'a, Ts>>);

impl<'a, Ts: TransitionSystem + FiniteState> std::ops::Deref for SccDecomposition<'a, Ts> {
    type Target = Vec<Scc<'a, Ts>>;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<'a, Ts: TransitionSystem + FiniteState> SccDecomposition<'a, Ts> {
    pub fn new(ts: &'a Ts, sccs: Vec<Scc<'a, Ts>>) -> Self {
        Self(ts, sccs)
    }
}

impl<'a, Ts: TransitionSystem + FiniteState + Debug> std::fmt::Debug for SccDecomposition<'a, Ts> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "SCC decomposition:  {{{}}} in\n{:?}",
            self.1
                .iter()
                .map(|scc| format!("[{}]", scc.1.iter().map(|q| format!("{:?}", q)).join(", ")))
                .join(", "),
            self.0
        )
    }
}

pub fn tarjan_scc<Ts>(ts: &Ts) -> SccDecomposition<'_, Ts>
where
    Ts: TransitionSystem + FiniteState,
{
    let mut sccs = Vec::new();
    {
        let mut tj = Tarjan::new();
        tj.execute(ts, |scc| sccs.push(Scc::new(ts, scc.to_vec())));
    }
    debug_assert!(
        sccs.iter().collect::<std::collections::HashSet<_>>().len() == sccs.len(),
        "All SCCs must be unique!"
    );
    sccs.sort();
    SccDecomposition::new(ts, sccs)
}

#[derive(Clone)]
pub struct TarjanTree<'a, Ts: TransitionSystem + Clone> {
    ts: &'a Ts,
    sccs: Vec<Scc<'a, Ts>>,
    edges: Vec<(usize, usize)>,
}

impl<'a, Ts: TransitionSystem + Clone + Debug> std::fmt::Debug for TarjanTree<'a, Ts> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "TarjanTree: {:?} Edges {:}",
            self.sccs
                .iter()
                .enumerate()
                .map(|(i, scc)| format!(
                    "[{}]",
                    scc.1.iter().map(|q| format!("{:?}", q)).join(", ")
                ))
                .join(", "),
            self.edges
                .iter()
                .map(|(k, v)| format!("{} -> {}", k, v))
                .join(", ")
        )
    }
}

impl<'a, Ts: TransitionSystem + Clone> TarjanTree<'a, Ts> {
    pub fn peel_off(&mut self) -> Vec<Scc<'a, Ts>> {
        let (terminal, non_terminal): (Vec<_>, Vec<_>) =
            self.sccs.iter().enumerate().partition(|(i, scc)| {
                self.ts
                    .reachable_state_indices_from(*scc.first().expect("Disallow non-empty SSCs"))
                    .all(|o| scc.1.contains(&o))
            });
        let out = terminal.into_iter().map(|(_, scc)| scc.clone()).collect();
        self.sccs = non_terminal
            .into_iter()
            .map(|(_, scc)| scc.clone())
            .collect();
        out
    }
}

impl<'a, Ts: TransitionSystem + FiniteState + Clone> From<SccDecomposition<'a, Ts>>
    for TarjanTree<'a, Ts>
{
    fn from(value: SccDecomposition<'a, Ts>) -> Self {
        let mut out = Vec::new();
        let mut cache = Map::default();
        for (l, ls) in value.1.iter().enumerate() {
            let l_reach = value
                .0
                .reachable_state_indices_from(*ls.first().unwrap())
                .collect::<Set<_>>();
            for (r, rs) in value.1.iter().enumerate().skip(l) {
                if l != r {
                    let r_reach = cache.entry(r).or_insert_with(|| {
                        value
                            .0
                            .reachable_state_indices_from(*rs.first().unwrap())
                            .collect::<Set<_>>()
                    });
                    if l_reach.is_superset(r_reach) {
                        out.push((l, r));
                    } else if l_reach.is_subset(r_reach) {
                        out.push((r, l));
                    }
                }
            }
        }
        Self {
            ts: value.0,
            sccs: value.1,
            edges: out,
        }
    }
}

// pub struct SCCBTS<Ts: TransitionSystem + Clone> {
//     ts: Ts,
//     sccs: Map<Ts::StateIndex, usize>,
//     edges: Vec<(usize, usize)>,
// }

// impl<Ts: TransitionSystem + Clone + Sproutable> Sproutable for SCCBTS<Ts> {
//     fn new_for_alphabet(alphabet: Self::Alphabet) -> Self {
//         Self {
//             ts: Ts::new_for_alphabet(alphabet),
//             sccs: Map::default(),
//             edges: Vec::new(),
//         }
//     }
//     fn add_state(&mut self, color: crate::ts::StateColor<Self>) -> Self::StateIndex {
//         let index = self.ts.add_state(color);
//         self.sccs.insert(index, self.sccs.len());
//         index
//     }

//     fn set_state_color(&mut self, index: Self::StateIndex, color: crate::ts::StateColor<Self>) {
//         self.ts.set_state_color(index, color)
//     }

//     fn add_edge<X, Y>(
//         &mut self,
//         from: X,
//         on: <Self::Alphabet as Alphabet>::Expression,
//         to: Y,
//         color: crate::ts::EdgeColor<Self>,
//     ) -> Option<(Self::StateIndex, Self::EdgeColor)>
//     where
//         X: Into<Self::StateIndex>,
//         Y: Into<Self::StateIndex>,
//     {
//         let source = from.into();
//         let target = to.into();
//         let out = self.ts.add_edge(source, on, target, color);
//         let source_scc = self.sccs.get(&source).copied().unwrap();
//         let target_scc = self.sccs.get(&target).copied().unwrap();
//         if target_scc < source_scc {
//             self.sccs.iter_mut().map(|o| {
//                 if *o.1 == source_scc {
//                     *o.1 = target_scc;
//                 }
//             });
//             self.edges
//                 .retain(|o| o.0 != source_scc && o.1 != source_scc);
//         } else {
//             self.edges.push((source_scc, target_scc));
//         }
//         out
//     }

//     fn remove_edge(
//         &mut self,
//         from: Self::StateIndex,
//         on: <Self::Alphabet as Alphabet>::Expression,
//     ) -> bool {
//         todo!()
//     }
// }

// impl<'b, Ts: TransitionSystem + Clone + HasFiniteStates<'b>> HasFiniteStates<'b> for SCCBTS<Ts> {
//     type StateIndicesIter = Ts::StateIndicesIter;
// }

// impl<Ts: TransitionSystem + Clone + FiniteState> FiniteState for SCCBTS<Ts> {
//     fn state_indices(&self) -> crate::ts::sealed::FiniteStatesIterType<'_, Self> {
//         self.ts.state_indices()
//     }
// }

// impl<Ts: TransitionSystem + Clone + Pointed> Pointed for SCCBTS<Ts> {
//     fn initial(&self) -> Self::StateIndex {
//         self.ts.initial()
//     }
// }

// impl<Ts: TransitionSystem + Clone> TransitionSystem for SCCBTS<Ts> {
//     type StateIndex = Ts::StateIndex;

//     type StateColor = Ts::StateColor;

//     type EdgeColor = Ts::EdgeColor;

//     type TransitionRef<'this> = Ts::TransitionRef<'this>
//     where
//         Self: 'this;

//     type EdgesFromIter<'this> = Ts::EdgesFromIter<'this>
//     where
//         Self: 'this;

//     fn sccs(&self) -> SccDecomposition<'_, Self::State>
//     where
//         Self: Sized + FiniteState,
//     {
//         let mut sccs = Vec::with_capacity(self.sccs.len());
//         for i in self.sccs.values() {
//             let mut scc = Scc::new(self, Vec::new());
//             for x in self.sccs.iter().filter(|(_, j)| i == *j).map(|(q, _)| *q) {
//                 scc.1.push(x);
//             }
//             sccs.push(Scc::new(
//                 self,
//                 self.sccs
//                     .iter()
//                     .filter(|(_, j)| i == *j)
//                     .map(|(q, _)| *q)
//                     .collect(),
//             ));
//         }
//         SccDecomposition::new(self, sccs)
//     }

//     fn transition(
//         &self,
//         state: Self::StateIndex,
//         symbol: SymbolOf<Self>,
//     ) -> Option<Self::TransitionRef<'_>> {
//         self.ts.transition(state, symbol)
//     }

//     fn edge_color(
//         &self,
//         state: Self::StateIndex,
//         expression: &crate::alphabet::ExpressionOf<Self>,
//     ) -> Option<crate::ts::EdgeColor<Self>> {
//         self.ts.edge_color(state, expression)
//     }

//     fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
//         self.ts.edges_from(state)
//     }

//     fn predecessors(
//         &self,
//         state: Self::StateIndex,
//     ) -> Vec<(
//         Self::StateIndex,
//         crate::alphabet::ExpressionOf<Self>,
//         crate::ts::EdgeColor<Self>,
//     )> {
//         self.ts.predecessors(state)
//     }

//     fn state_color(&self, state: Self::StateIndex) -> Self::StateColor {
//         self.ts.state_color(state)
//     }

//     fn with_initial(self, initial: Self::StateIndex) -> crate::automaton::WithInitial<Self>
//     where
//         Self: Sized,
//     {
//         (self, initial).into()
//     }

//     fn restrict_state_indices<F: Fn(Self::StateIndex) -> bool>(
//         self,
//         filter: F,
//     ) -> super::RestrictByStateIndex<Self, F>
//     where
//         Self: Sized,
//     {
//         super::RestrictByStateIndex::new(self, filter)
//     }

//     fn map_colors<D: crate::Color, F: Fn(Self::StateColor) -> D>(
//         self,
//         f: F,
//     ) -> crate::ts::operations::MapStateColor<Self, F>
//     where
//         Self: Sized,
//     {
//         crate::ts::operations::MapStateColor::new(self, f)
//     }

//     fn all_accepting_dfa(
//         self,
//     ) -> crate::ts::operations::MapStateColor<Self, fn(Self::StateColor) -> bool>
//     where
//         Self: Sized,
//     {
//         self.map_colors(|_| true)
//     }

//     fn tarjan_tree(&self) -> TarjanTree<'_, Self>
//     where
//         Self: Sized + FiniteState + Clone,
//     {
//         TarjanTree::from(tarjan_scc(self))
//     }

//     fn successor_index(
//         &self,
//         state: Self::StateIndex,
//         symbol: SymbolOf<Self>,
//     ) -> Option<Self::StateIndex> {
//         self.transition(state, symbol).map(|t| t.target())
//     }
// }

// impl<Ts: TransitionSystem + Clone> HasAlphabet for SCCBTS<Ts> {
//     type Alphabet = Ts::Alphabet;

//     fn alphabet(&self) -> &Self::Alphabet {
//         self.ts.alphabet()
//     }
// }

// impl<Ts: TransitionSystem + Clone> SCCBTS<Ts> {
//     pub fn new(ts: Ts, sccs: Map<Ts::StateIndex, usize>, edges: Vec<(usize, usize)>) -> Self {
//         Self { ts, sccs, edges }
//     }
// }

#[cfg(test)]
mod tests {
    use crate::{
        alphabet::Simple,
        simple,
        ts::{
            transition_system::sccs::{Scc, SccDecomposition},
            Sproutable,
        },
        Pointed, RightCongruence,
    };

    fn ts() -> RightCongruence<Simple> {
        let mut cong = RightCongruence::new(simple!('a', 'b'));
        let q0 = cong.initial();
        let q1 = cong.add_state(vec!['a'].into());
        let q2 = cong.add_state(vec!['b'].into());
        let q3 = cong.add_state(vec!['b', 'b'].into());
        cong.add_edge(q0, 'a', q1, ());
        cong.add_edge(q0, 'b', q2, ());
        cong.add_edge(q1, 'a', q1, ());
        cong.add_edge(q1, 'b', q1, ());
        cong.add_edge(q2, 'a', q3, ());
        cong.add_edge(q2, 'b', q2, ());
        cong.add_edge(q3, 'a', q3, ());
        cong.add_edge(q3, 'b', q2, ());
        cong
    }

    #[test]
    fn tarjan_tree() {
        let cong = ts();
        let sccs = super::tarjan_scc(&cong);
        let mut tree = super::TarjanTree::from(sccs);

        println!("{:?}", tree);
    }

    #[test]
    fn tarjan_scc_decomposition() {
        let cong = ts();
        let sccs = super::tarjan_scc(&cong);

        let scc1 = Scc::new(&cong, vec![0]);
        let scc2 = Scc::new(&cong, vec![1]);
        let scc3 = Scc::new(&cong, vec![2, 3]);

        assert_eq!(
            sccs,
            SccDecomposition::new(&cong, vec![scc1.clone(), scc2.clone(), scc3.clone()])
        );

        assert_eq!(scc1.maximal_word(), None);
        assert_eq!(scc2.maximal_word(), Some(vec!['a', 'b']));
        assert_eq!(scc3.maximal_word(), Some(vec!['a', 'a', 'b', 'b']))
    }
}
