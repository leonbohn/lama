use std::{collections::BTreeSet, fmt::Debug};

use itertools::Itertools;

use crate::{
    alphabet::{HasAlphabet, SymbolOf},
    ts::{finite::SeenColors, CanInduce, FiniteState, IndexType},
    Alphabet, Map, TransitionSystem,
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

impl<Idx: IndexType> Default for Tarjan<Idx> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Idx: IndexType> Tarjan<Idx> {
    /// Creates a new Tarjan SCC decomposition instance.
    pub fn new() -> Self {
        Self {
            index: 0,
            component_count: usize::MAX,
            stack: Vec::new(),
            data: Map::default(),
        }
    }

    pub(crate) fn visit<Ts, F>(&mut self, ts: &Ts, v: Idx, f: &mut F)
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
                let v_mut = self.data.get_mut(&v).unwrap();
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

    pub(crate) fn execute<Ts, F>(&mut self, ts: &Ts, mut f: F)
    where
        Ts: TransitionSystem<StateIndex = Idx> + FiniteState,
        F: FnMut(&[Idx]),
    {
        self.data.clear();

        for q in ts.state_indices() {
            if let Some(TarjanData { rootindex: None }) | None = self.data.get(&q) {
                self.visit(ts, q, &mut f);
            }
        }
        debug_assert!(self.stack.is_empty())
    }
}

/// Represents a strongly connected component of a transition system.
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
    /// Creates a new strongly connected component from a transition system and a vector of state indices.
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

    /// Returns a reference to the underlying transition system.
    pub fn ts(&self) -> &'a Ts {
        self.0
    }

    /// Returns a vector of the colors of the states in the SCC.
    pub fn colors(&self) -> Option<Vec<Ts::StateColor>> {
        debug_assert!(!self.is_empty());
        let maximal_word = self.maximal_word()?;
        let SeenColors(colors) = self.ts().run(&maximal_word, self.1[0]).ok()?.induce();
        Some(colors)
    }

    /// Attempts to compute a maximal word (i.e. a word visiting all states in the scc). If such a
    /// word exists, it is returned, otherwise the function returns `None`.
    pub fn maximal_word(&self) -> Option<Vec<SymbolOf<Ts>>> {
        let ts = self.0;
        debug_assert!(!self.is_empty());

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

    /// Returns the number of states in the SCC.
    pub fn len(&self) -> usize {
        self.1.len()
    }

    /// Returns `true` if and only if the SCC is empty.
    pub fn is_empty(&self) -> bool {
        if self.len() == 0 {
            panic!("SCCs can never be empty!");
        }
        false
    }

    /// Returns `true` iff the SCC consists of a single state.
    pub fn is_singleton(&self) -> bool {
        self.1.len() == 1
    }

    /// Returns `true` iff the SCC is left on every symbol of the alphabet.
    pub fn is_transient(&self) -> bool {
        !self.is_nontransient()
    }

    /// Returns `true` iff there is a transition from a state in the SCC to another state in the SCC,
    /// i.e. if there is a way of reading a non-empty word and staying in the SCC.
    pub fn is_nontransient(&self) -> bool {
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

/// Represents a decomposition of a transition system into strongly connected components.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SccDecomposition<'a, Ts: TransitionSystem + FiniteState>(&'a Ts, Vec<Scc<'a, Ts>>);

impl<'a, Ts: TransitionSystem + FiniteState> std::ops::Deref for SccDecomposition<'a, Ts> {
    type Target = Vec<Scc<'a, Ts>>;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<'a, Ts: TransitionSystem + FiniteState> SccDecomposition<'a, Ts> {
    /// Creates a new SCC decomposition from a transition system and a vector of SCCs.
    pub fn new(ts: &'a Ts, sccs: Vec<Scc<'a, Ts>>) -> Self {
        Self(ts, sccs)
    }

    /// Attepmts to find the index of a the SCC containing the given `state`. Returns this index if
    /// it exists, otherwise returns `None`.
    pub fn scc_of(&self, state: Ts::StateIndex) -> Option<usize> {
        self.1
            .iter()
            .enumerate()
            .find_map(|(i, scc)| if scc.contains(&state) { Some(i) } else { None })
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

pub(crate) fn tarjan_scc<Ts>(ts: &Ts) -> SccDecomposition<'_, Ts>
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

/// Represents a hierarchical view on the SCCs of a transition system.
#[derive(Clone)]
pub struct TarjanDAG<'a, Ts: TransitionSystem + Clone> {
    ts: &'a Ts,
    sccs: Vec<Scc<'a, Ts>>,
    edges: Vec<(usize, usize)>,
}

impl<'a, Ts: TransitionSystem + Clone + Debug> std::fmt::Debug for TarjanDAG<'a, Ts> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "TarjanTree: {:?} Edges {:}",
            self.sccs
                .iter()
                .enumerate()
                .map(|(_i, scc)| format!(
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

impl<'a, Ts: TransitionSystem + Clone> TarjanDAG<'a, Ts> {
    fn peel_off(&mut self) -> Vec<Scc<'a, Ts>> {
        let (terminal, non_terminal): (Vec<_>, Vec<_>) =
            self.sccs.iter().enumerate().partition(|(_i, scc)| {
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
    for TarjanDAG<'a, Ts>
{
    fn from(value: SccDecomposition<'a, Ts>) -> Self {
        let mut out = Vec::new();
        for (l, ls) in value.1.iter().enumerate() {
            out.extend(
                ls.iter()
                    .flat_map(|q| value.0.edges_from(*q).expect("We know this state exists!"))
                    .map(|o| (l, value.scc_of(o.target()).expect("Must be in some SCC")))
                    .unique(),
            );
        }
        Self {
            ts: value.0,
            sccs: value.1,
            edges: out,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        alphabet::Simple,
        simple,
        ts::{
            sccs::{Scc, SccDecomposition},
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
        let _tree = super::TarjanDAG::from(sccs);
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
