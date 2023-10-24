use std::fmt::Debug;

use automata::prelude::*;
use itertools::Itertools;
use tracing::trace;

use super::{
    logging::{LStarLogbook, LStarLogger, LStarQuery},
    oracle::Oracle,
    table::{
        LStarExample, LStarExampleFor, LStarExperiments, LStarRow, LStarRows, LStarTable,
        LStarTarget,
    },
};

#[cfg(not(debug_assertions))]
const ITERATION_THRESHOLD: usize = 200000;
#[cfg(debug_assertions)]
const ITERATION_THRESHOLD: usize = 200;

#[derive(Debug, Clone)]
pub struct LStar<
    A: Alphabet,
    O,
    T: LStarTarget<ForMealy>,
    L: LStarLogger<ForMealy, T> = LStarLogbook<false, T>,
    const ForMealy: bool = false,
> {
    teacher: O,
    alphabet: A,
    table: T,
    logger: L,
}

impl<
        A: Alphabet,
        C: Color + Debug + Default,
        O: Oracle<MooreMachine<A, C>, Length = FiniteLength, Output = C>,
    > LStar<A, O, LStarTable<A, C, false>, (), false>
{
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
        O: Oracle<MooreMachine<A, C>, Length = FiniteLength, Output = C>,
    > LStar<A, O, LStarTable<A, C, false>, LStarLogbook<false, LStarTable<A, C, false>>, false>
{
    pub fn moore_logged(
        teacher: O,
        alphabet: A,
    ) -> LStar<A, O, LStarTable<A, C, false>, LStarLogbook<false, LStarTable<A, C, false>>, false>
    {
        LStar::new(teacher, alphabet)
    }
}
impl<A: Alphabet, O: Oracle<MealyMachine<A, usize>, Length = FiniteLength, Output = usize>>
    LStar<A, O, LStarTable<A, usize, true>, (), true>
{
    pub fn mealy_unlogged(
        teacher: O,
        alphabet: A,
    ) -> LStar<A, O, LStarTable<A, usize, true>, (), true> {
        LStar::new(teacher, alphabet)
    }
}

impl<A: Alphabet, O: Oracle<MealyMachine<A, usize>, Length = FiniteLength, Output = usize>>
    LStar<A, O, LStarTable<A, usize, true>, LStarLogbook<true, LStarTable<A, usize, true>>, true>
{
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
        T: LStarTarget<ForMealy, Alphabet = A>,
        O: Oracle<T::Hypothesis, Length = FiniteLength, Output = T::Output>,
        L: LStarLogger<ForMealy, T>,
        const ForMealy: bool,
    > LStar<A, O, T, L, ForMealy>
{
    fn new(teacher: O, alphabet: A) -> Self {
        let experiments = if ForMealy {
            either::Either::Right(std::iter::empty())
        } else {
            either::Either::Left(std::iter::once(vec![]))
        }
        .chain(alphabet.universe().map(|sym| vec![*sym]))
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

    pub fn logs(&self) -> &L {
        &self.logger
    }

    pub fn infer(&mut self) -> T::Hypothesis {
        loop {
            let base_size = self.table.recompute_base();
            if base_size >= ITERATION_THRESHOLD {
                panic!("Too many iterations, probably an infinite loop!");
            }
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

    use crate::active::{
        logging::LStarLogbook,
        oracle,
        oracle::{DFAOracle, Oracle},
    };

    struct ModkAmodlB(Simple);
    struct WordLenModk(Simple, usize);

    impl HasAlphabet for ModkAmodlB {
        type Alphabet = Simple;

        fn alphabet(&self) -> &Self::Alphabet {
            &self.0
        }
    }

    impl Oracle<MooreMachine<Simple, bool>> for ModkAmodlB {
        type Output = bool;

        fn output<
            W: automata::Word<Symbol = automata::alphabet::SymbolOf<Self>, Length = Self::Length>,
        >(
            &self,
            word: W,
        ) -> Self::Output {
            let (count_a, count_b) =
                word.finite_to_vec()
                    .into_iter()
                    .fold((0, 0), |(a, b), c| match c {
                        'a' => (a + 1, b),
                        'b' => (a, b + 1),
                        _ => unreachable!(),
                    });

            if count_a % 2 == 0 && count_b % 2 == 0 {
                true
            } else {
                false
            }
        }

        fn equivalence(
            &self,
            hypothesis: &MooreMachine<Simple, bool>,
        ) -> Result<(), (Vec<automata::alphabet::SymbolOf<Self>>, Self::Output)> {
            for word in ["aa", "bb", "bab", "aba", "abba", "bbab", "", "b", "a"] {
                let output = self.output(&word);
                if output != hypothesis.transform(&word) {
                    return Err((word.chars().collect(), output));
                }
            }
            Ok(())
        }

        type Length = FiniteLength;
    }

    impl HasAlphabet for WordLenModk {
        type Alphabet = Simple;

        fn alphabet(&self) -> &Self::Alphabet {
            &self.0
        }
    }

    #[cfg(test)]
    impl Oracle<MooreMachine<Simple, usize>> for WordLenModk {
        type Output = usize;
        type Length = FiniteLength;

        fn output<
            W: automata::Word<Length = Self::Length, Symbol = automata::alphabet::SymbolOf<Self>>,
        >(
            &self,
            word: W,
        ) -> Self::Output {
            word.length().0 % self.1
        }

        fn equivalence(
            &self,
            hypothesis: &MooreMachine<Simple, usize>,
        ) -> Result<(), (Vec<automata::alphabet::SymbolOf<Self>>, Self::Output)> {
            for word in [
                "aa", "bb", "bab", "bbabba", "aba", "abba", "bbab", "", "b", "a",
            ] {
                let word = OmegaWord::new_reverse_args(FiniteLength(word.len()), word);
                let output = self.output(&word);
                if output != hypothesis.transform(&word) {
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

        assert_eq!(mm.transform("abba"), true);
        assert_eq!(mm.transform("ab"), false);
        assert_eq!(mm.transform(""), true)
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
        let logs = lstar.logs();
        for ex in logs.examples() {
            println!("{:?}", ex);
        }
    }
}
