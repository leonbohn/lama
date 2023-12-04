use std::fmt::Debug;

use automata::prelude::*;
use itertools::Itertools;
use tracing::trace;

use super::{
    logging::{LStarLogbook, LStarLogger, LStarQuery},
    oracle::LStarOracle,
    table::{
        LStarExample, LStarExampleFor, LStarExperiments, LStarRow, LStarRows, LStarTable,
        LStarTarget,
    },
};

const ITERATION_THRESHOLD: usize = if cfg!(debug_assertions) { 200 } else { 200000 };

/// An implementation of the L* algorithm.
#[derive(Debug, Clone)]
pub struct LStar<
    A: Alphabet,
    O,
    T: LStarTarget<FOR_MEALY>,
    L: LStarLogger<FOR_MEALY, T> = LStarLogbook<false, T>,
    const FOR_MEALY: bool = false,
> {
    teacher: O,
    alphabet: A,
    table: T,
    logger: L,
}

impl<
        A: Alphabet,
        C: Color + Debug + Default,
        O: LStarOracle<MooreMachine<A, C>, Length = FiniteLength, Output = C>,
    > LStar<A, O, LStarTable<A, C, false>, (), false>
{
    /// Creates a new L* instance for a Moore machine, which does not log the queries.
    pub fn moore_unlogged(
        teacher: O,
        alphabet: A,
    ) -> LStar<A, O, LStarTable<A, C, false>, (), false> {
        LStar::new(teacher, alphabet)
    }
}

impl<
        A: Alphabet,
        C: Color + Debug + Default,
        O: LStarOracle<MooreMachine<A, C>, Length = FiniteLength, Output = C>,
    > LStar<A, O, LStarTable<A, C, false>, LStarLogbook<false, LStarTable<A, C, false>>, false>
{
    /// Creates a new L* instance for a Moore machine, which logs the queries.
    #[allow(clippy::type_complexity)]
    pub fn moore_logged(
        teacher: O,
        alphabet: A,
    ) -> LStar<A, O, LStarTable<A, C, false>, LStarLogbook<false, LStarTable<A, C, false>>, false>
    {
        LStar::new(teacher, alphabet)
    }
}
impl<
        A: Alphabet,
        O: LStarOracle<MealyMachine<A, usize>, Length = FiniteLength, Output = usize>,
    > LStar<A, O, LStarTable<A, usize, true>, (), true>
{
    /// Creates a new L* instance for a Mealy machine, which does not log the queries.
    pub fn mealy_unlogged(
        teacher: O,
        alphabet: A,
    ) -> LStar<A, O, LStarTable<A, usize, true>, (), true> {
        LStar::new(teacher, alphabet)
    }
}

impl<
        A: Alphabet,
        O: LStarOracle<MealyMachine<A, usize>, Length = FiniteLength, Output = usize>,
    >
    LStar<A, O, LStarTable<A, usize, true>, LStarLogbook<true, LStarTable<A, usize, true>>, true>
{
    /// Creates a new L* instance for a Mealy machine, which logs the queries.
    #[allow(clippy::type_complexity)]
    pub fn mealy_logged(
        teacher: O,
        alphabet: A,
    ) -> LStar<A, O, LStarTable<A, usize, true>, LStarLogbook<true, LStarTable<A, usize, true>>, true>
    {
        LStar::new(teacher, alphabet)
    }
}

impl<
        A: Alphabet,
        T: LStarTarget<FOR_MEALY, Alphabet = A>,
        O: LStarOracle<T::Hypothesis, Length = FiniteLength, Output = T::Output>,
        L: LStarLogger<FOR_MEALY, T>,
        const FOR_MEALY: bool,
    > LStar<A, O, T, L, FOR_MEALY>
{
    fn new(teacher: O, alphabet: A) -> Self {
        let experiments = if FOR_MEALY {
            either::Either::Right(std::iter::empty())
        } else {
            either::Either::Left(std::iter::once(vec![]))
        }
        .chain(alphabet.universe().map(|sym| vec![sym]))
        .collect_vec();

        let mut logger = L::create();

        let mut table = T::new_table(alphabet.clone(), experiments);
        let queries = table.fill_rows(&teacher);
        for query in queries {
            logger.log(query);
        }

        LStar {
            teacher,
            table,
            alphabet,
            logger: L::create(),
        }
    }

    /// Obtain a reference to the logger.
    pub fn logger(&self) -> &L {
        &self.logger
    }

    /// Run the L* algorithm and obtain a hypothesis that is equivalent to the target.
    pub fn infer(&mut self) -> T::Hypothesis {
        loop {
            let base_size = self.table.recompute_base();
            if base_size >= ITERATION_THRESHOLD {
                panic!("Too many iterations, probably an infinite loop!");
            }
            println!("{:?}", self.table);
            match self.table.hypothesis() {
                crate::active::table::LStarHypothesisResult::Success(hyp) => {
                    match self.teacher.equivalence(&hyp) {
                        Ok(_) => return hyp,
                        Err(conflict) => {
                            trace!(
                                "Obtained counterexample \"{}\" with classification {:?}",
                                conflict.0.iter().map(|sym| format!("{:?}", sym)).join(""),
                                conflict.1
                            );
                            let queries =
                                self.table
                                    .accept_counterexample(&hyp, conflict, &self.teacher);
                            for query in queries {
                                self.logger.log(query);
                            }
                        }
                    }
                }
                crate::active::table::LStarHypothesisResult::MissingRow(row) => {
                    self.table.add_row(row);
                    let queries = self.table.fill_rows(&self.teacher);
                    for query in queries {
                        self.logger.log(query);
                    }
                }
                crate::active::table::LStarHypothesisResult::PromoteRow(row_index) => {
                    self.table.promote_row(row_index);
                    let queries = self.table.fill_rows(&self.teacher);
                    for query in queries {
                        self.logger.log(query);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use automata::prelude::*;
    use owo_colors::OwoColorize;
    use tracing_test::traced_test;

    use crate::{
        active::{
            logging::LStarLogbook,
            oracle,
            oracle::{DFAOracle, LStarOracle},
        },
        passive::FiniteSample,
    };

    struct ModkAmodlB(Simple);
    struct WordLenModk(Simple, usize);

    impl LStarOracle<MooreMachine<Simple, bool>> for ModkAmodlB {
        type Output = bool;

        fn output<W: FiniteWord<char>>(&self, word: W) -> Self::Output {
            let (count_a, count_b) = word.symbols().fold((0, 0), |(a, b), c| match c {
                'a' => (a + 1, b),
                'b' => (a, b + 1),
                _ => unreachable!(),
            });

            count_a % 2 == 0 && count_b % 2 == 0
        }

        fn equivalence(
            &self,
            hypothesis: &MooreMachine<Simple, bool>,
        ) -> Result<(), (Vec<char>, Self::Output)> {
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
        type Output = usize;
        type Length = FiniteLength;

        fn output<W: FiniteWord<char>>(&self, word: W) -> Self::Output {
            word.len() % self.1
        }

        fn equivalence(
            &self,
            hypothesis: &MooreMachine<Simple, usize>,
        ) -> Result<(), (Vec<char>, Self::Output)> {
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
            let mut lstar = super::LStar::moore_unlogged(oracle, alphabet.clone());
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
        let mut lstar = super::LStar::moore_unlogged(oracle, alphabet);

        let mm = lstar.infer();

        assert!(mm.try_moore_map("abba").unwrap());
        assert!(!mm.try_moore_map("ab").unwrap());
        assert!(mm.try_moore_map("").unwrap())
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
    fn lstar_for_dfa() {
        let target = test_dfa();
        let oracle = DFAOracle::new(&target);

        let mut lstar = super::LStar::moore_unlogged(oracle, target.alphabet().clone());

        let learned = lstar.infer();
        assert!(learned.equivalent(&target));
    }

    #[test]
    #[traced_test]
    fn lstar_logged() {
        let target = test_dfa();
        let oracle = DFAOracle::new(&target);
        let mut lstar = super::LStar::moore_logged(oracle, target.alphabet().clone());
        let learned = lstar.infer();
        let logs = lstar.logger();
        for ex in logs.examples() {
            println!("{:?}", ex);
        }
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

        let mealy = super::LStar::mealy_unlogged(oracle.clone(), alphabet.clone()).infer();
        let moore = super::LStar::moore_unlogged(oracle, alphabet).infer();

        assert!(mealy.size() <= moore.size());
        tracing::debug!(
            "Mealy size is {} while Moore size is {}",
            mealy.size(),
            moore.size()
        )
    }
}
