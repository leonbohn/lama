use crate::{
    alphabet::{HasAlphabet, SymbolOf},
    ts::HasStates,
    Pointed, Successor,
};

pub struct RestrictByStateIndex<Ts: Successor, F> {
    ts: Ts,
    filter: F,
}

impl<Ts: Successor + Pointed, F> Pointed for RestrictByStateIndex<Ts, F>
where
    F: Fn(Ts::StateIndex) -> bool,
{
    fn initial(&self) -> Self::StateIndex {
        let initial = self.ts.initial();
        assert!((self.filter)(initial), "initial state is filtered out");
        initial
    }
}

impl<Ts: Successor, F> HasAlphabet for RestrictByStateIndex<Ts, F> {
    type Alphabet = Ts::Alphabet;
    fn alphabet(&self) -> &Self::Alphabet {
        self.ts.alphabet()
    }
}

impl<Ts: Successor, F> Successor for RestrictByStateIndex<Ts, F>
where
    F: Fn(Ts::StateIndex) -> bool,
{
    type StateIndex = Ts::StateIndex;

    type Position = Ts::Position;

    type Color = Ts::Color;

    fn successor(
        &self,
        state: Self::StateIndex,
        symbol: crate::alphabet::SymbolOf<Self>,
    ) -> Option<
        crate::ts::Transition<
            Self::StateIndex,
            crate::alphabet::SymbolOf<Self>,
            crate::ts::EdgeColor<Self>,
        >,
    > {
        self.ts
            .successor(state, symbol)
            .filter(|successor| (self.filter)(state) && (self.filter)(successor.target()))
    }

    fn state_color(&self, state: Self::StateIndex) -> crate::ts::StateColor<Self> {
        assert!((self.filter)(state));
        self.ts.state_color(state)
    }
}

impl<Ts: Successor, F> RestrictByStateIndex<Ts, F> {
    pub fn new(ts: Ts, filter: F) -> Self {
        Self { ts, filter }
    }
}

#[cfg(test)]
mod tests {
    use crate::{simple, ts::Sproutable, Acceptor, Pointed, Successor, DFA};

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
