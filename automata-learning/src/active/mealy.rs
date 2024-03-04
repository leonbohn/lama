use automata::prelude::*;

use super::LStarHypothesis;

impl<A: Alphabet, C: Color + Default> LStarHypothesis for MooreMachine<A, C> {
    type Color = C;

    fn transform(&self, word: &[SymbolOf<Self>]) -> Self::Color {
        self.reached_state_color(word)
            .expect("Hypothesis is not complete!")
    }

    fn from_transition_system(
        ts: DTS<Self::Alphabet, Self::StateColor, Self::EdgeColor>,
        initial: usize,
    ) -> Self {
        ts.with_initial(initial).into_moore()
    }

    fn give_state_color(
        mr: &[SymbolOf<Self>],
        experiments: &super::Experiments<Self>,
        row: &[Self::Color],
    ) -> Self::StateColor {
        let Some(idx) = experiments.iter().position(|x| x.is_empty()) else {
            panic!(
                "experiment for emtpy word must be present to be able to deduce the state color"
            );
        };
        assert!(idx < row.len(), "Not enough experiments");
        row[idx].clone()
    }

    fn give_transition_color(
        mr: &[SymbolOf<Self>],
        a: SymbolOf<Self>,
        experiments: &super::Experiments<Self>,
        row: &[Self::Color],
    ) -> Self::EdgeColor {
        Void
    }

    fn mandatory_experiments(
        alphabet: &Self::Alphabet,
    ) -> impl IntoIterator<Item = Vec<SymbolOf<Self>>> {
        [vec![]]
    }
}

impl<C: Color + Default> LStarHypothesis for MealyMachine<CharAlphabet, C> {
    type Color = C;

    fn transform(&self, word: &[SymbolOf<Self>]) -> Self::Color {
        assert!(
            !word.is_empty(),
            "Mealy machine can only deal with non-empty words!"
        );
        self.last_edge_color(word)
            .expect("Hypothesis is not complete!")
    }

    fn from_transition_system(
        ts: DTS<Self::Alphabet, Self::StateColor, Self::EdgeColor>,
        initial: usize,
    ) -> Self {
        ts.with_initial(initial).into_mealy()
    }

    fn give_state_color(
        mr: &[SymbolOf<Self>],
        experiments: &super::Experiments<Self>,
        row: &[Self::Color],
    ) -> Self::StateColor {
        Void
    }

    fn give_transition_color(
        mr: &[SymbolOf<Self>],
        a: SymbolOf<Self>,
        experiments: &super::Experiments<Self>,
        row: &[Self::Color],
    ) -> Self::EdgeColor {
        let Some(idx) = experiments.iter().position(|x| x == &[a]) else {
            panic!("experiment for single letters must exist");
        };
        assert!(idx < row.len(), "not enough experiments");
        row[idx].clone()
    }

    fn mandatory_experiments(
        alphabet: &Self::Alphabet,
    ) -> impl IntoIterator<Item = Vec<SymbolOf<Self>>> {
        alphabet.universe().map(|a| vec![a])
    }
}

impl LStarHypothesis for DFA {
    type Color = bool;

    fn transform(&self, word: &[SymbolOf<Self>]) -> Self::Color {
        self.reached_state_color(word)
            .expect("Hypothesis must be complete")
    }

    fn mandatory_experiments(
        alphabet: &Self::Alphabet,
    ) -> impl IntoIterator<Item = Vec<SymbolOf<Self>>> {
        [vec![]]
    }

    fn from_transition_system(
        ts: DTS<Self::Alphabet, Self::StateColor, Self::EdgeColor>,
        initial: usize,
    ) -> Self {
        ts.with_initial(initial).into_dfa()
    }

    fn give_state_color(
        mr: &[SymbolOf<Self>],
        experiments: &super::Experiments<Self>,
        row: &[Self::Color],
    ) -> Self::StateColor {
        let empty: Vec<SymbolOf<Self>> = vec![];
        let Some(idx) = experiments.iter().position(|x| empty.eq(x)) else {
            panic!("empty experiment  must exist");
        };
        assert!(idx < row.len(), "not enough experiments");
        row[idx]
    }

    fn give_transition_color(
        mr: &[SymbolOf<Self>],
        a: SymbolOf<Self>,
        experiments: &super::Experiments<Self>,
        row: &[Self::Color],
    ) -> Self::EdgeColor {
        Void
    }
}
