use automata::alphabet::{Alphabet, HasUniverse};
use automata::ts::OnStates;
use automata::{Color, FiniteLength, MooreMachine};

use super::table::Table;

use super::oracle::Oracle;

#[derive(Debug, Clone)]
pub struct LStar<A: Alphabet, C: Color, T: Oracle<Alphabet = A, Output = C>> {
    teacher: T,
    alphabet: A,
    table: Table<A, C>,
}

impl<
        A: HasUniverse,
        C: Color + Default,
        T: Oracle<OnStates, Length = FiniteLength, Alphabet = A, Output = C>,
    > LStar<A, C, T>
{
    pub fn new(teacher: T, alphabet: A) -> Self {
        let mut table = Table::new(alphabet.clone());
        let mut teach = teacher;
        table.fill(&mut teach);
        Self {
            teacher: teach,
            alphabet,
            table,
        }
    }

    pub fn close(&mut self) {
        self.table.close(&mut self.teacher)
    }

    pub fn fill(&mut self) -> bool {
        self.table.fill(&mut self.teacher)
    }

    pub fn equivalent(&mut self) -> bool {
        self.teacher
            .equivalence(
                &self
                    .table
                    .to_hypothesis()
                    .expect("Could not build hypothesis"),
            )
            .is_ok()
    }
}

#[cfg(test)]
mod tests {
    use automata::{
        alphabet::{Alphabet, HasAlphabet, Simple},
        word::RawWithLength,
        FiniteLength, Transformer, Word,
    };
    use tracing_test::traced_test;

    use crate::active::Oracle;

    struct EvenAEvenB(Simple);
    struct WordLenModThree(Simple);

    impl HasAlphabet for EvenAEvenB {
        type Alphabet = Simple;

        fn alphabet(&self) -> &Self::Alphabet {
            &self.0
        }
    }

    impl Oracle for EvenAEvenB {
        type Output = bool;

        fn output<
            W: automata::Word<Symbol = automata::alphabet::SymbolOf<Self>, Length = Self::Length>,
        >(
            &self,
            word: W,
        ) -> Self::Output {
            let (count_a, count_b) = word.symbols().fold((0, 0), |(a, b), c| match c {
                'a' => (a + 1, b),
                'b' => (a, b + 1),
                _ => unreachable!(),
            });

            count_a % 2 == 0 && count_b % 2 == 0
        }

        fn equivalence<H>(
            &self,
            hypothesis: H,
        ) -> Result<(), (Vec<automata::alphabet::SymbolOf<Self>>, Self::Output)>
        where
            H: automata::ts::Pointed
                + automata::ts::Successor<
                    Alphabet = Self::Alphabet,
                    Position = automata::ts::OnStates,
                    Color = Self::Output,
                > + automata::Transformer<
                    automata::alphabet::SymbolOf<Self>,
                    FiniteLength,
                    Output = Self::Output,
                >,
        {
            for word in ["aa", "bb", "bab", "aba", "abba", "bbab", "", "b", "a"] {
                let word = RawWithLength::new_reverse_args(FiniteLength(word.len()), word);
                let output = self.output(&word);
                if output != hypothesis.transform(&word) {
                    return Err((word.raw_as_vec(), output));
                }
            }
            Ok(())
        }

        type Length = FiniteLength;
    }

    impl HasAlphabet for WordLenModThree {
        type Alphabet = Simple;

        fn alphabet(&self) -> &Self::Alphabet {
            &self.0
        }
    }

    #[cfg(test)]
    impl Oracle for WordLenModThree {
        type Output = usize;
        type Length = FiniteLength;

        fn output<
            W: automata::Word<Length = Self::Length, Symbol = automata::alphabet::SymbolOf<Self>>,
        >(
            &self,
            word: W,
        ) -> Self::Output {
            word.length().0 % 3
        }

        fn equivalence<
            H: automata::ts::Pointed
                + automata::ts::Successor<
                    Alphabet = Self::Alphabet,
                    Position = automata::ts::OnStates,
                    Color = Self::Output,
                >,
        >(
            &self,
            hypothesis: H,
        ) -> Result<(), (Vec<automata::alphabet::SymbolOf<Self>>, Self::Output)> {
            for word in [
                "aa", "bb", "bab", "bbabba", "aba", "abba", "bbab", "", "b", "a",
            ] {
                let word = RawWithLength::new_reverse_args(FiniteLength(word.len()), word);
                let output = self.output(&word);
                if output != hypothesis.transform(&word) {
                    return Err((word.raw_as_vec(), output));
                }
            }
            Ok(())
        }
    }

    #[test]
    #[traced_test]
    fn lstar_word_len_mod_3() {
        let alphabet = Simple::from_iter(vec!['a', 'b']);
        let oracle = WordLenModThree(alphabet.clone());
        let mut lstar = super::LStar::new(oracle, alphabet);

        lstar.close();
        println!("{}", lstar.table);
    }
    #[test]
    fn lstar_even_a_even_b() {
        let alphabet = Simple::from_iter(vec!['a', 'b']);
        let mut oracle = EvenAEvenB(alphabet.clone());
        assert!(oracle.output("aa"));

        let mut lstar = super::LStar::new(oracle, alphabet);
        println!("{}", lstar.table);
        lstar.table.insert_extensions();
        lstar.fill();
        println!("{}", lstar.table);
    }
}
