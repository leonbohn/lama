use std::{cell::RefCell, fmt::Debug};

use automata::{prelude::*, word::Concat, Map, Set};
use fixedbitset::FixedBitSet;
use itertools::Itertools;
use tracing::{debug, info, trace};

use super::oracle::LStarOracle;

const ITERATION_THRESHOLD: usize = if cfg!(debug_assertions) { 300 } else { 200000 };

pub trait LStarHypothesis: Deterministic + Sproutable + Pointed {
    type Color: Color;

    fn transform(&self, word: &[SymbolOf<Self>]) -> Self::Color;

    fn from_transition_system(
        ts: DTS<Self::Alphabet, Self::StateColor, Self::EdgeColor>,
        initial: usize,
    ) -> Self;

    fn give_state_color(
        mr: &[SymbolOf<Self>],
        experiments: &Experiments<Self>,
        row: &[Self::Color],
    ) -> Self::StateColor;

    fn give_transition_color(
        mr: &[SymbolOf<Self>],
        a: SymbolOf<Self>,
        experiments: &Experiments<Self>,
        row: &[Self::Color],
    ) -> Self::EdgeColor;

    fn mandatory_experiments(
        alphabet: &Self::Alphabet,
    ) -> impl IntoIterator<Item = Vec<SymbolOf<Self>>>;
}

type Word<D> = Vec<SymbolOf<D>>;
pub type Experiments<D> = Vec<Word<D>>;

/// An implementation of the L* algorithm.
#[derive(Clone)]
pub struct LStar<D: LStarHypothesis, T: LStarOracle<D>> {
    // the alphabet of what we are learning
    alphabet: D::Alphabet,
    // a mapping containing all queries that have been posed so far, together with their output
    queries: RefCell<Map<Word<D>, D::Color>>,
    // the minimal access words forming the base states
    base: Vec<Word<D>>,
    // all known experiments
    experiments: Vec<Word<D>>,
    // mapping from input word to a bitset, where the i-th entry gives the value for
    // the output of concatenating input word and i-th experiment
    table: Map<Word<D>, Vec<D::Color>>,
    // the oracle
    oracle: T,
}

impl<T: LStarOracle<DFA>> LStar<DFA, T> {
    pub fn dfa(oracle: T) -> DFA {
        Self::new(oracle.alphabet(), oracle).infer()
    }
    pub fn for_dfa(alphabet: CharAlphabet, oracle: T) -> LStar<DFA, T> {
        Self::new(alphabet, oracle)
    }
}

impl<T: LStarOracle<MealyMachine<CharAlphabet>>> LStar<MealyMachine<CharAlphabet>, T> {
    pub fn mealy(oracle: T) -> MealyMachine<CharAlphabet> {
        Self::new(oracle.alphabet(), oracle).infer()
    }
    pub fn for_mealy(alphabet: CharAlphabet, oracle: T) -> LStar<MealyMachine<CharAlphabet>, T> {
        Self::new(alphabet, oracle)
    }
}

impl<T: LStarOracle<MooreMachine<CharAlphabet>>> LStar<MooreMachine<CharAlphabet>, T> {
    pub fn moore(alphabet: CharAlphabet, oracle: T) -> MooreMachine<CharAlphabet> {
        Self::new(alphabet, oracle).infer()
    }
    pub fn for_moore(alphabet: CharAlphabet, oracle: T) -> LStar<MooreMachine<CharAlphabet>, T> {
        Self::new(alphabet, oracle)
    }
}

impl<D: LStarHypothesis, T: LStarOracle<D>> LStar<D, T> {
    pub fn new(alphabet: D::Alphabet, oracle: T) -> Self {
        Self {
            experiments: D::mandatory_experiments(&alphabet).into_iter().collect(),
            alphabet,
            queries: RefCell::new(Map::default()),
            base: vec![vec![]],
            table: Map::default(),
            oracle,
        }
    }

    fn output(&self, w: &Word<D>) -> D::Color {
        if !self.queries.borrow().contains_key(w) {
            let c = self.oracle.output(w);
            assert!(self.queries.borrow_mut().insert(w.to_owned(), c).is_none());
        }
        self.queries.borrow().get(w).unwrap().clone()
    }

    fn update_table(&mut self) {
        let mut updates = vec![];
        let experiment_count = self.experiments.len();

        for (n, mr) in self.one_letter_extensions().enumerate() {
            let stored_experiment_count = self.table.get(&mr).map(|r| r.len()).unwrap_or(0);
            trace!("Have stored {stored_experiment_count} out of {experiment_count} experiments for {}", mr.as_string());
            if stored_experiment_count < experiment_count {
                for i in stored_experiment_count..experiment_count {
                    let concat = Concat(&mr, &self.experiments[i]).to_vec();
                    let output = self.output(&concat).clone();
                    trace!(
                        "Adding update that {} maps to {}",
                        concat.as_string(),
                        output.show()
                    );
                    updates.push((mr.clone(), i, output));
                }
            } else {
                assert_eq!(
                    stored_experiment_count,
                    experiment_count,
                    "Too many experiments present for {}",
                    mr.as_string()
                );
            }
        }

        for (mr, i, output) in updates {
            assert!(i < self.experiments.len());
            let mut row = self.table.entry(mr).or_default();
            row.push(output);
        }

        if cfg!(debug_assertions) {
            for mr in self.one_letter_extensions() {
                let Some(val) = self.table.get(&mr) else {
                    panic!("No table entry for {}", mr.as_string());
                };
                if val.len() != experiment_count {
                    panic!(
                        "Row for {} does not have enough entries, has {} of {experiment_count}",
                        mr.show(),
                        val.len()
                    )
                }
            }
        }

        trace!("After update the table is\n{:?}", self);
    }

    pub fn infer(&mut self) -> D {
        let start = std::time::Instant::now();
        let mut iteration = 0;
        let threshold = std::env::var("MAX_ITERATIONS")
            .unwrap_or(format!("{ITERATION_THRESHOLD}"))
            .parse()
            .unwrap();

        'outer: while iteration < threshold {
            self.update_table();
            iteration += 1;
            trace!("LStar iteration {iteration} with table\n{:?}", self);
            let todo = self.rows_to_promote();
            trace!(
                "Have to promote rows: {}",
                todo.iter().map(FiniteWord::as_string).join(", ")
            );

            if !todo.is_empty() {
                let mut queries = Set::default();
                for r in todo {
                    self.base.push(r.clone());
                    queries.insert(r.clone());
                }
                // TODO optimize this call!
                self.update_table();
                continue 'outer;
            }

            let hypothesis = self.hypothesis();

            if let Err((counterexample, color)) = self.oracle.equivalence(&hypothesis) {
                assert!(hypothesis.transform(&counterexample) != color);
                self.process_counterexample(counterexample, color);
                continue 'outer;
            }

            let duration = start.elapsed().as_millis();
            info!("Execution of LStar took {duration}ms");
            return hypothesis;
        }

        panic!("Iteration threshold exceeded!")
    }

    fn process_counterexample(&mut self, word: Word<D>, color: D::Color) {
        for i in 0..(word.len()) {
            let suffix = word[i..].to_vec();
            assert!(!suffix.is_empty());
            if !self.experiments.contains(&suffix) {
                trace!("Adding counterexample {}", suffix.as_string());
                self.experiments.push(suffix)
            }
        }
    }

    fn state_color(&self, mr: &Word<D>) -> D::StateColor {
        D::give_state_color(
            mr,
            &self.experiments,
            self.table
                .get(mr)
                .expect("Cannot give color for non-existent word"),
        )
    }

    fn edge_color(&self, mr: &Word<D>, sym: SymbolOf<D>) -> D::EdgeColor {
        D::give_transition_color(mr, sym, &self.experiments, self.table.get(mr).unwrap())
    }

    fn hypothesis(&self) -> D {
        let start = std::time::Instant::now();

        let mut ts: DTS<_, _, _> = DTS::new_for_alphabet(self.alphabet.clone());
        let mut state_map = Map::default();
        let mut observations = Map::default();

        for mr in &self.base {
            let color = self.state_color(mr);
            let id = ts.add_state(color);
            state_map.insert(mr, id);

            let observed = self.table.get(mr).unwrap();
            observations.insert(observed, mr);
        }

        for (mr, i) in &state_map {
            for a in self.alphabet.universe() {
                let color = self.edge_color(mr, a);
                let obs = self
                    .table
                    .get(&Concat(mr, [a]).to_vec())
                    .expect("Can only work if table is closed!");
                let target = observations.get(obs).unwrap();

                let added = ts.add_edge(
                    *i,
                    <D::Alphabet as Alphabet>::expression(a),
                    *state_map.get(target).unwrap(),
                    color,
                );
                assert!(added.is_none());
            }
        }

        let duration = start.elapsed().as_micros();
        debug!("Building hypothesis took {duration} microseconds");

        D::from_transition_system(ts, *state_map.get(&vec![]).unwrap())
    }

    fn inconsistent_words(&self) -> Option<Word<D>> {
        for (i, left) in self.base.iter().enumerate() {
            'middle: for right in &self.base[i..] {
                if self.table.get(left) != self.table.get(right) {
                    continue 'middle;
                }

                for sym in self.alphabet.universe() {
                    let left_ext = left
                        .iter()
                        .chain(std::iter::once(&sym))
                        .cloned()
                        .collect_vec();
                    let right_ext = right
                        .iter()
                        .chain(std::iter::once(&sym))
                        .cloned()
                        .collect_vec();

                    let l = self.table.get(&left_ext).expect("Should be present!");
                    let r = self.table.get(&right_ext).expect("Should be present!");

                    if l == r {
                        continue 'middle;
                    }
                    'inner: for (j, e) in self.experiments.iter().enumerate() {
                        assert!(j < std::cmp::min(l.len(), r.len()));
                        if l[j] == r[j] {
                            continue 'inner;
                        }
                        return Some(Concat(vec![sym], e).to_vec());
                    }
                }
            }
        }
        None
    }

    fn one_letter_extensions(&self) -> impl Iterator<Item = Word<D>> + '_ {
        self.base
            .iter()
            .flat_map(|w| {
                std::iter::once(w.clone()).chain(self.alphabet.universe().filter_map(|a| {
                    let mut x = w.clone();
                    x.push(a);
                    if !self.base.contains(&x) {
                        Some(x)
                    } else {
                        None
                    }
                }))
            })
            .unique()
    }

    fn rows_to_promote(&self) -> Set<Word<D>> {
        let start = std::time::Instant::now();

        let known = automata::Set::from_iter(self.base.iter().map(|b| {
            self.table.get(b).unwrap_or_else(|| {
                panic!(
                    "Experiment {} must be present",
                    owo_colors::OwoColorize::blue(&b.as_string())
                )
            })
        }));
        let mut seen = automata::Set::default();
        let mut out = Set::default();

        for word in self.one_letter_extensions() {
            trace!("Considering one letter extension {}", word.as_string());
            let seq = self.table.get(&word).unwrap();
            if !known.contains(seq) && seen.insert(seq) {
                out.insert(word);
            }
        }

        debug!(
            "Finding rows to promote took {} microseconds",
            start.elapsed().as_micros()
        );
        out
    }
}

impl<D: LStarHypothesis, T: LStarOracle<D>> std::fmt::Debug for LStar<D, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = tabled::builder::Builder::default();
        let mut header = vec!["MR".to_string()];

        for e in &self.experiments {
            header.push(e.as_string());
        }
        builder.push_record(header);

        for (i, mr) in self.base.iter().enumerate() {
            let mut row = vec![mr.as_string()];
            for color in self
                .table
                .get(mr)
                .unwrap_or_else(|| panic!("No table entry for {}", mr.as_string()))
            {
                row.push(color.show());
            }
            builder.push_record(row);
        }

        write!(f, "{}", builder.build())
    }
}

#[cfg(test)]
mod tests {
    use automata::prelude::*;
    use oracle::MealyOracle;
    use owo_colors::OwoColorize;

    use crate::{
        active::{
            oracle,
            oracle::{DFAOracle, LStarOracle},
        },
        passive::FiniteSample,
    };

    use super::LStar;

    struct ModkAmodlB(CharAlphabet);
    struct WordLenModk(CharAlphabet, usize);

    impl LStarOracle<MooreMachine<CharAlphabet>> for ModkAmodlB {
        fn output<W: FiniteWord<char>>(&self, word: W) -> usize {
            let (count_a, count_b) = word.symbols().fold((0, 0), |(a, b), c| match c {
                'a' => (a + 1, b),
                'b' => (a, b + 1),
                _ => unreachable!(),
            });

            if count_a % 2 == 0 && count_b % 2 == 0 {
                1
            } else {
                0
            }
        }

        fn equivalence(
            &self,
            hypothesis: &MooreMachine<CharAlphabet>,
        ) -> Result<(), (Vec<char>, usize)> {
            for word in [
                "aa", "bb", "bab", "aba", "abba", "bbab", "", "b", "a", "abaaabab", "bbababa",
            ] {
                let output = self.output(word);
                if output
                    != hypothesis
                        .try_moore_map(word)
                        .expect("Hypothesis should be complete")
                {
                    return Err((word.chars().collect(), output));
                }
            }
            Ok(())
        }

        fn alphabet(&self) -> CharAlphabet {
            CharAlphabet::from_iter(['a', 'b'])
        }
    }

    #[cfg(test)]
    impl LStarOracle<MooreMachine<CharAlphabet, usize>> for WordLenModk {
        fn output<W: FiniteWord<char>>(&self, word: W) -> usize {
            word.len() % self.1
        }

        fn equivalence(
            &self,
            hypothesis: &MooreMachine<CharAlphabet, usize>,
        ) -> Result<(), (Vec<char>, usize)> {
            for word in [
                "aa", "bb", "bab", "bbabba", "aba", "abba", "bbab", "", "b", "a",
            ] {
                let output = self.output(word);
                if output != hypothesis.try_moore_map(word).unwrap() {
                    return Err((word.to_vec(), output));
                }
            }
            Ok(())
        }

        fn alphabet(&self) -> CharAlphabet {
            vec!['a', 'b'].into()
        }
    }

    #[test]
    fn lstar_word_len_mod_k() {
        let alphabet = CharAlphabet::from_iter(vec!['a', 'b']);

        for k in (3..10) {
            let time_start = std::time::Instant::now();
            let oracle = WordLenModk(alphabet.clone(), k);
            let mut lstar = super::LStar::for_moore(alphabet.clone(), oracle);
            let mm = lstar.infer();
            let time_taken = time_start.elapsed().as_micros();
            assert_eq!(mm.size(), k);
            println!("Took {:>6}μs for k={}", time_taken, k);
        }
    }

    #[test]
    fn lstar_even_a_even_b() {
        let alphabet = CharAlphabet::from_iter(vec!['a', 'b']);
        let mut oracle = ModkAmodlB(alphabet.clone());
        let mut lstar = super::LStar::for_moore(alphabet, oracle);

        let mm = lstar.infer();

        assert_eq!(mm.try_moore_map("abba").unwrap(), 1);
        assert_eq!(mm.try_moore_map("ab").unwrap(), 0);
        assert_eq!(mm.try_moore_map("").unwrap(), 1)
    }

    fn test_dfa() -> DFA {
        let alphabet = CharAlphabet::from_iter(['a', 'b', 'c']);
        let mut dfa = DFA::new_for_alphabet(alphabet);
        let q0 = dfa.add_state(true);
        let q1 = dfa.add_state(false);
        let q2 = dfa.add_state(true);
        let q3 = dfa.add_state(false);
        dfa.add_edge(q0, 'a', q1, Void);
        dfa.add_edge(q0, 'b', q3, Void);
        dfa.add_edge(q0, 'c', q0, Void);
        dfa.add_edge(q1, 'a', q0, Void);
        dfa.add_edge(q1, 'b', q2, Void);
        dfa.add_edge(q1, 'c', q0, Void);
        dfa.add_edge(q2, 'a', q2, Void);
        dfa.add_edge(q2, 'b', q2, Void);
        dfa.add_edge(q2, 'c', q0, Void);
        dfa.add_edge(q3, 'a', q3, Void);
        dfa.add_edge(q3, 'b', q3, Void);
        dfa.add_edge(q3, 'c', q0, Void);
        dfa
    }

    #[test]
    fn lstar_dfa() {
        let oracle = DFAOracle::new(test_dfa());
        let dfa = LStar::dfa(oracle);

        assert_eq!(dfa.size(), 4);

        if let Some(witness) = dfa.moore_witness_non_bisimilarity(test_dfa()) {
            panic!("DFAs do not treat {} the same way", witness.as_string());
        }
    }

    #[test]
    fn lstar_mealy() {
        let mm = NTS::builder()
            .default_color(())
            .with_transitions([
                (0, 'a', 0, 0),
                (0, 'b', 1, 1),
                (0, 'c', 2, 2),
                (1, 'a', 0, 1),
                (1, 'b', 1, 0),
                (1, 'c', 2, 1),
                (2, 'a', 0, 0),
                (2, 'b', 1, 1),
                (2, 'c', 3, 0),
            ])
            .deterministic()
            .with_initial(0)
            .into_mealy();
        let oracle = MealyOracle::new(mm.clone(), None);

        let learned = LStar::mealy(oracle);

        assert_eq!(learned.size(), 3);
    }

    #[test]
    fn lstar_moore_vs_mealy() {
        let alphabet = alphabet!(simple 'a', 'b');
        let classified_words = [
            "a", "b", "aa", "ab", "ba", "bb", "aaa", "aab", "aba", "abb", "baa", "bab", "bba",
            "bbb",
        ]
        .into_iter()
        .map(|w| (w.chars(), if w.ends_with('a') { 0 } else { 1 }));
        let sample = FiniteSample::new_finite(alphabet.clone(), classified_words);
        let oracle = oracle::SampleOracle::new(sample, 0);

        let mealy = super::LStar::for_mealy(alphabet.clone(), oracle.clone()).infer();
        let moore = super::LStar::for_moore(alphabet, oracle).infer();

        assert!(mealy.size() <= moore.size());
        tracing::debug!(
            "Mealy size is {} while Moore size is {}",
            mealy.size(),
            moore.size()
        )
    }
}
