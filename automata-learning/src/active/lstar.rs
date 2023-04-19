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
struct EvenAEvenB();

#[cfg(test)]
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
mod tests {
    use automata::Class;
    use tracing::metadata::LevelFilter;

    use crate::active::Oracle;

    use super::EvenAEvenB;

    #[test]
    fn lstar_even_a_even_b() {
        // Set up the tracing subscriber
        let subscriber = tracing_subscriber::fmt::Subscriber::builder()
            .with_max_level(LevelFilter::TRACE)
            .finish();

        tracing::subscriber::set_global_default(subscriber)
            .expect("Unable to set global tracing subscriber");

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
