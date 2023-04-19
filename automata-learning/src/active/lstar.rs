use super::table::Table;
use automata::{Class, Pointed, Symbol, Transducer};

use super::teacher::Oracle;

#[derive(Debug, Clone)]
pub struct LStar<In: Symbol, Out: Symbol, T: Oracle<Input = In, Output = Out>> {
    teacher: T,
    alphabet: Vec<In>,
    table: Table<In, Out>,
}

impl<Input: Symbol, Output: Symbol, T: Oracle<Input = Input, Output = Output>>
    LStar<Input, Output, T>
{
    pub fn new(teacher: T, alphabet: Vec<Input>) -> Self {
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
    use automata::{words::IsFinite, Class, Pointed, Transducer};

    use crate::{active::Oracle, tests::init_logger};

    struct EvenAEvenB();
    struct WordLenModThree();

    impl Oracle for EvenAEvenB {
        type Input = char;

        type Output = bool;

        fn output(&mut self, word: &automata::Class<Self::Input>) -> Self::Output {
            let (count_a, count_b) = word.iter().fold((0, 0), |(a, b), c| match c {
                'a' => (a + 1, b),
                'b' => (a, b + 1),
                _ => unreachable!(),
            });

            count_a % 2 == 0 && count_b % 2 == 0
        }

        fn equivalence<M: Pointed + Transducer<Input = Self::Input, Output = Self::Output>>(
            &mut self,
            hypothesis: &M,
        ) -> Result<(), automata::Class<Self::Input>> {
            for word in ["aa", "bb", "bab", "aba", "abba", "bbab", "", "b", "a"] {
                let word = Class::from(word);
                if Some(self.output(&word)) != hypothesis.output(word.iter()) {
                    return Err(word);
                }
            }
            Ok(())
        }
    }

    #[cfg(test)]
    impl Oracle for WordLenModThree {
        type Input = char;

        type Output = usize;

        fn output(&mut self, word: &Class<Self::Input>) -> Self::Output {
            word.length() % 3
        }

        fn equivalence<M: Pointed + Transducer<Input = Self::Input, Output = Self::Output>>(
            &mut self,
            hypothesis: &M,
        ) -> Result<(), Class<Self::Input>> {
            for word in [
                "aa", "bb", "bab", "bbabba", "aba", "abba", "bbab", "", "b", "a",
            ] {
                let word = Class::from(word);
                if Some(self.output(&word)) != hypothesis.output(word.iter()) {
                    return Err(word);
                }
            }
            Ok(())
        }
    }
    #[test]
    fn lstar_word_len_mod_3() {
        init_logger();

        let mut oracle = WordLenModThree();
        let mut lstar = super::LStar::new(oracle, vec!['a', 'b']);

        lstar.close();
        println!("{}", lstar.table);

        println!("{}", lstar.table.to_hypothesis().unwrap());
    }

    #[test]
    fn lstar_even_a_even_b() {
        init_logger();

        let mut oracle = EvenAEvenB();
        assert_eq!(oracle.output(&automata::Class::from("aa")), true);

        let mut lstar = super::LStar::new(oracle, vec!['a', 'b']);
        println!("{}", lstar.table);
        lstar.table.insert_extensions();
        lstar.fill();
        println!("{}", lstar.table);
        let x = lstar.table.equivalent_class(&Class::from("aa"));
        println!("{:?}", x);
        lstar.close();
        println!("{}", lstar.table);
        println!("{}", lstar.table.to_hypothesis().unwrap());
    }
}
