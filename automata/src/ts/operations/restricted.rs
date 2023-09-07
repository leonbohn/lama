use crate::{
    alphabet::{HasAlphabet, SymbolOf},
    ts::{
        transition_system::{IsPreTransition, IsTransition},
        HasStates,
    },
    Pointed, TransitionSystem,
};

pub struct RestrictByStateIndex<Ts: TransitionSystem, F> {
    ts: Ts,
    filter: F,
}

impl<Ts: TransitionSystem + Pointed, F> Pointed for RestrictByStateIndex<Ts, F>
where
    F: Fn(Ts::StateIndex) -> bool,
{
    fn initial(&self) -> Self::StateIndex {
        let initial = self.ts.initial();
        assert!((self.filter)(initial), "initial state is filtered out");
        initial
    }
}

impl<Ts: TransitionSystem, F> HasAlphabet for RestrictByStateIndex<Ts, F> {
    type Alphabet = Ts::Alphabet;
    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}

impl<Ts: TransitionSystem, F> RestrictByStateIndex<Ts, F> {
    pub fn new(ts: Ts, filter: F) -> Self {
        Self { ts, filter }
    }

    pub fn filter(&self) -> &F {
        &self.filter
    }

    pub fn ts(&self) -> &Ts {
        &self.ts
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RestrictedEdgesFromIter<'a, Ts: TransitionSystem + 'a, F> {
    filter: &'a F,
    it: Ts::EdgesFromIter<'a>,
}

impl<'a, Ts: TransitionSystem + 'a, F> RestrictedEdgesFromIter<'a, Ts, F> {
    pub fn new(it: Ts::EdgesFromIter<'a>, filter: &'a F) -> Self {
        Self { filter, it }
    }
}

impl<'a, Ts: TransitionSystem + 'a, F> Iterator for RestrictedEdgesFromIter<'a, Ts, F>
where
    F: Fn(Ts::StateIndex) -> bool,
{
    type Item = Ts::TransitionRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.it.by_ref().find(|edge| (self.filter)(edge.target()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RestrictedEdgesToIter<'a, Ts: TransitionSystem + 'a, F> {
    filter: &'a F,
    it: Ts::EdgesToIter<'a>,
}

impl<'a, Ts: TransitionSystem + 'a, F> Iterator for RestrictedEdgesToIter<'a, Ts, F>
where
    F: Fn(Ts::StateIndex) -> bool,
{
    type Item = Ts::PreTransitionRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.it.by_ref().find(|edge| (self.filter)(edge.source()))
    }
}

impl<'a, Ts: TransitionSystem + 'a, F> RestrictedEdgesToIter<'a, Ts, F> {
    pub fn new(it: Ts::EdgesToIter<'a>, filter: &'a F) -> Self {
        Self { filter, it }
    }
}

#[cfg(test)]
mod tests {
    use crate::{simple, ts::Sproutable, Acceptor, Pointed, TransitionSystem, DFA};

    #[test]
    fn restrict_ts_by_state_index() {
        let mut dfa = DFA::new(simple! {'a', 'b'});
        let q0 = dfa.initial();
        let q1 = dfa.add_state(false);
        let q2 = dfa.add_state(true);

        dfa.add_edge(q0, 'a', q1, ());
        dfa.add_edge(q0, 'b', q0, ());
        dfa.add_edge(q1, 'a', q2, ());
        dfa.add_edge(q1, 'b', q1, ());
        dfa.add_edge(q2, 'a', q0, ());
        dfa.add_edge(q2, 'b', q2, ());
        assert!(dfa.accepts("aa"));

        let restricted = dfa.restrict_state_indices(|idx| idx != q2);
        assert!(!restricted.accepts("aa"));
    }
}
