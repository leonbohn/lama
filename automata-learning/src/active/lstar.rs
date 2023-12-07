use std::{cell::RefCell, fmt::Debug};

use automata::{prelude::*, word::Concat, Map, Set};
use fixedbitset::FixedBitSet;
use itertools::Itertools;
use tracing::trace;

use super::oracle::LStarOracle;

const ITERATION_THRESHOLD: usize = if cfg!(debug_assertions) { 200 } else { 200000 };

pub trait LStarHypothesis:
    Deterministic + Sproutable + Pointed + FiniteWordTransformer<SymbolOf<Self>, Self::Color>
{
    type Color: Color;
}

type Word<D> = Vec<SymbolOf<D>>;

/// An implementation of the L* algorithm.
#[derive(Debug, Clone)]
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

impl<T: LStarOracle<MealyMachine<Simple>>> LStar<MealyMachine<Simple>, T> {
    pub fn mealy(alphabet: Simple, oracle: T) -> MealyMachine<Simple> {
        Self::new(alphabet, oracle).infer()
    }
    pub fn for_mealy(alphabet: Simple, oracle: T) -> LStar<MealyMachine<Simple>, T> {
        Self::new(alphabet, oracle)
    }
}

impl<T: LStarOracle<MooreMachine<Simple>>> LStar<MooreMachine<Simple>, T> {
    pub fn moore(alphabet: Simple, oracle: T) -> MooreMachine<Simple> {
        Self::new(alphabet, oracle).infer()
    }
    pub fn for_moore(alphabet: Simple, oracle: T) -> LStar<MooreMachine<Simple>, T> {
        Self::new(alphabet, oracle)
    }
}

impl<D: LStarHypothesis, T: LStarOracle<D>> LStar<D, T> {
    pub fn new(alphabet: D::Alphabet, oracle: T) -> Self {
        Self {
            experiments: alphabet.universe().map(|a| vec![a]).collect(),
            alphabet,
            queries: RefCell::new(Map::default()),
            base: Vec::default(),
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

        for (n, mr) in self.base.iter().enumerate().rev() {
            assert!(
                self.table.get(mr).is_some(),
                "table does not contain experiment!"
            );
            let stored_experiment_count = self.table.get(mr).unwrap().len();
            if stored_experiment_count < experiment_count {
                for i in stored_experiment_count..experiment_count {
                    let output = self.output(mr).clone();
                    updates.push((n, i, output));
                }
            } else {
                panic!("Too many experiment results stored!")
            }
        }

        for (n, i, output) in updates {
            assert!(n < self.base.len());
            assert!(i < self.experiments.len());
            let mut row = self.table.get_mut(&self.base[n]).unwrap();
            assert_eq!(row.len(), i - 1);
            row.push(output);
        }
    }

    pub fn infer(&mut self) -> D {
        let start = std::time::Instant::now();
        let mut iteration = 0;

        'outer: while iteration < ITERATION_THRESHOLD {
            let todo = self.unclosed_rows();
            while !todo.is_empty() {
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
                assert!(hypothesis.transform_finite(&counterexample) != color);
                self.process_counterexample(counterexample, color);
                continue 'outer;
            }

            return hypothesis;
        }

        panic!("Iteration threshold exceeded!")
    }

    fn process_counterexample(&mut self, word: Word<D>, color: D::Color) {
        todo!()
    }

    fn hypothesis(&self) -> D {
        todo!()
    }

    fn one_letter_extensions(&self) -> impl Iterator<Item = Word<D>> + '_ {
        self.base.iter().flat_map(|w| {
            self.alphabet.universe().filter_map(|a| {
                let mut x = w.clone();
                x.push(a);
                if !self.base.contains(&x) {
                    Some(x)
                } else {
                    None
                }
            })
        })
    }

    fn unclosed_rows(&self) -> Set<Word<D>> {
        let known = automata::Set::from_iter(
            self.base
                .iter()
                .map(|b| self.table.get(b).expect("Experiment must be present")),
        );
        let mut seen = automata::Set::default();
        let mut out = Set::default();

        for word in self.one_letter_extensions() {
            let seq = self.table.get(&word).unwrap();
            if !known.contains(seq) {
                if seen.insert(seq) {
                    out.insert(word);
                }
            }
        }
        out
    }
}

//     /// Run the L* algorithm and obtain a hypothesis that is equivalent to the target.
//     pub fn infer(&mut self) -> T::Hypothesis {
//         loop {
//             let base_size = self.table.recompute_base();
//             if base_size >= ITERATION_THRESHOLD {
//                 panic!("Too many iterations, probably an infinite loop!");
//             }
//             match self.table.hypothesis() {
//                 crate::active::table::LStarHypothesisResult::Success(hyp) => {
//                     match self.teacher.equivalence(&hyp) {
//                         Ok(_) => return hyp,
//                         Err(conflict) => {
//                             trace!(
//                                 "Obtained counterexample \"{}\" with classification {:?}",
//                                 conflict.0.iter().map(|sym| format!("{:?}", sym)).join(""),
//                                 conflict.1
//                             );
//                             self.logger.log_iter(self.table.accept_counterexample(
//                                 &hyp,
//                                 conflict,
//                                 &self.teacher,
//                             ));
//                             self.logger.log_iter(self.table.fill_rows(&self.teacher));
//                         }
//                     }
//                 }
//                 crate::active::table::LStarHypothesisResult::MissingRow(row) => {
//                     self.table.add_row(row);
//                     let queries = self.table.fill_rows(&self.teacher);
//                     for query in queries {
//                         self.logger.log(query);
//                     }
//                 }
//                 crate::active::table::LStarHypothesisResult::PromoteRow(row_index) => {
//                     self.table.promote_row(row_index);
//                     let queries = self.table.fill_rows(&self.teacher);
//                     for query in queries {
//                         self.logger.log(query);
//                     }
//                 }
//             }
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use automata::prelude::*;
    use owo_colors::OwoColorize;
    use tracing_test::traced_test;

    use crate::{
        active::{
            oracle,
            oracle::{DFAOracle, LStarOracle},
        },
        passive::FiniteSample,
    };

    struct ModkAmodlB(Simple);
    struct WordLenModk(Simple, usize);

    impl LStarOracle<MooreMachine<Simple>> for ModkAmodlB {
        fn output<W: FiniteWord<char>>(&self, word: W) -> usize {
            let (count_a, count_b) = word.symbols().fold((0, 0), |(a, b), c| match c {
                'a' => (a + 1, b),
                'b' => (a, b + 1),
                _ => unreachable!(),
            });

            if count_a % 2 == 0 && count_b % 2 == 0 {
                0
            } else {
                1
            }
        }

        fn equivalence(&self, hypothesis: &MooreMachine<Simple>) -> Result<(), (Vec<char>, usize)> {
            for word in ["aa", "bb", "bab", "aba", "abba", "bbab", "", "b", "a"] {
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

        type Length = FiniteLength;
    }

    #[cfg(test)]
    impl LStarOracle<MooreMachine<Simple, usize>> for WordLenModk {
        type Length = FiniteLength;

        fn output<W: FiniteWord<char>>(&self, word: W) -> usize {
            word.len() % self.1
        }

        fn equivalence(
            &self,
            hypothesis: &MooreMachine<Simple, usize>,
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
    }

    #[test]
    fn lstar_word_len_mod_k() {
        let alphabet = Simple::from_iter(vec!['a', 'b']);

        for k in (30..=50) {
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
    #[traced_test]
    fn lstar_even_a_even_b() {
        let alphabet = Simple::from_iter(vec!['a', 'b']);
        let mut oracle = ModkAmodlB(alphabet.clone());
        let mut lstar = super::LStar::for_moore(alphabet, oracle);

        let mm = lstar.infer();

        assert_eq!(mm.try_moore_map("abba").unwrap(), 1);
        assert_eq!(!mm.try_moore_map("ab").unwrap(), 0);
        assert_eq!(mm.try_moore_map("").unwrap(), 1)
    }

    fn test_dfa() -> DFA {
        let alphabet = Simple::from_iter(['a', 'b', 'c']);
        let mut dfa = DFA::new(alphabet);
        let q0 = dfa.initial();
        dfa.set_initial_color(true);
        let q1 = dfa.add_state(false);
        let q2 = dfa.add_state(true);
        let q3 = dfa.add_state(false);
        dfa.add_edge(q0, 'a', q1, ());
        dfa.add_edge(q0, 'b', q3, ());
        dfa.add_edge(q0, 'c', q0, ());
        dfa.add_edge(q1, 'a', q0, ());
        dfa.add_edge(q1, 'b', q2, ());
        dfa.add_edge(q1, 'c', q0, ());
        dfa.add_edge(q2, 'a', q2, ());
        dfa.add_edge(q2, 'b', q2, ());
        dfa.add_edge(q2, 'c', q0, ());
        dfa.add_edge(q3, 'a', q3, ());
        dfa.add_edge(q3, 'b', q3, ());
        dfa.add_edge(q3, 'c', q0, ());
        dfa
    }

    #[test]
    #[traced_test]
    fn moore_vs_mealy() {
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
