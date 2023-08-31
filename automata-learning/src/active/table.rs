use std::{
    collections::{BTreeMap, BTreeSet},
    default,
    fmt::Display,
};

use automata::{
    alphabet::{Alphabet, HasUniverse, Symbol},
    ts::{HasColorMut, HasMutableStates},
    word::RawWithLength,
    Color, FiniteLength, MealyMachine, MooreMachine,
};
use itertools::Itertools;
use owo_colors::OwoColorize;
use tabled::{builder::Builder, settings::Style};
use tracing::{debug, trace};

use super::oracle::Oracle;

/// Represents a row of an observation table. It stores a mapping that associates classes (which
/// are in essence words over `Input`) to outputs of type `Output`.
/// It should be ensured that each row maps every experiment to an output.
#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub(super) struct Row<C: Color>(Vec<C>);

impl<C: Color> Row<C> {
    pub fn create<
        T: Oracle<Length = FiniteLength, Output = C>,
        W: IntoIterator<Item = Vec<<T::Alphabet as Alphabet>::Symbol>>,
    >(
        oracle: &T,
        elements: W,
    ) -> Self {
        Self(elements.into_iter().map(|wrd| oracle.output(wrd)).collect())
    }
}

impl<C: Color> From<Vec<C>> for Row<C> {
    fn from(value: Vec<C>) -> Self {
        Self(value)
    }
}

impl<C: Color> std::ops::Deref for Row<C> {
    type Target = Vec<C>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// An observation table is used to store the obtained information during a run of
/// an active learning (in our case L*-esque) algorithm.
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Table<A: Alphabet, C: Color> {
    alphabet: A,
    experiments: Vec<Vec<A::Symbol>>,
    base: BTreeSet<Vec<A::Symbol>>,
    rows: BTreeMap<Vec<A::Symbol>, Row<C>>,
}

impl<A, C> Table<A, C>
where
    A: HasUniverse,
    C: Color + Default,
{
    pub fn new(alphabet: A) -> Self {
        let base_cases = std::iter::once(vec![])
            .chain(alphabet.universe().map(|sym| vec![*sym]))
            .collect_vec();
        Self {
            experiments: base_cases.clone(),
            rows: base_cases
                .iter()
                .map(|class| (class.clone(), Row::default()))
                .collect(),
            base: base_cases.into_iter().collect(),
            alphabet,
        }
    }

    pub(crate) fn insert_extensions(&mut self) {
        let mut missing = BTreeSet::new();
        for base in &self.base {
            for sym in self.alphabet.universe() {
                let extension = Row::create();
                if !self.base.iter().any(|b| extension.matches(b)) {
                    missing.insert(extension.to_vec());
                }
            }
        }
        self.base.extend(missing.into_iter())
    }

    pub(crate) fn reduce(&mut self) -> bool {
        let to_remove = self
            .base
            .iter()
            .rev()
            .filter(|class| {
                self.base
                    .iter()
                    .any(|other| other < class && self.rows.get(*class) == self.rows.get(other))
            })
            .cloned()
            .collect_vec();

        let changed = to_remove.is_empty();
        self.base.retain(|class| !to_remove.contains(class));

        changed
    }

    pub(crate) fn to_hypothesis(&self) -> Option<MooreMachine<A, C>> {
        if self.find_missing().is_some() {
            return None;
        }

        let mut mm = MooreMachine::with_capacity(self.alphabet.clone(), self.base.len());

        let epsilon = self
            .experiments
            .first()
            .expect("Epsilon experiment must exist!");
        for (i, b) in self.base.iter().enumerate() {
            let row = self.rows.get(b).expect("Row must exist!");
            let output = row.get(epsilon).expect("Output must exist!");
            mm.state_mut(i)
                .expect("This state should exist!")
                .set_color(output.clone());
        }
        todo!()
    }

    pub(crate) fn is_sane(&self) -> bool {
        println!("base: {:?}\nexp: {:?}", self.base, self.experiments);
        if !self.base.contains(&vec![]) || !self.experiments.contains(&vec![]) {
            debug!("Base or experiments do not contain base class.");
            return false;
        }
        if !self
            .alphabet
            .universe()
            .map(|chr| vec![*chr])
            .all(|class| self.experiments.contains(&class) && self.base.contains(&class))
        {
            debug!("Base or experiments do not contain all letters.");
            return false;
        };
        true
    }

    pub(crate) fn find_missing<T: Oracle<Alphabet = A, Length = FiniteLength, Output = C>>(
        &self,
        oracle: &T,
    ) -> Option<Vec<A::Symbol>> {
        trace!("Checking if the table is closed.");
        debug_assert!(self.is_sane());
        debug_assert!(self.rows_complete());

        for class in &self.base {
            for sym in self.alphabet.universe() {
                let extension = Row::create(oracle, class.iter().chain(std::iter::once(sym)));
                if !self.base.iter().any(|b| extension.matches(b)) {
                    debug!("No row for extension {:?} found.", extension);
                    return Some(extension.to_vec());
                }
            }
        }
        None
    }

    pub(crate) fn close<T: Oracle<Alphabet = A, Length = FiniteLength, Output = C>>(
        &mut self,
        oracle: &T,
    ) {
        while let Some(missing) = self.find_missing(oracle) {
            trace!("Table is not closed. Extending it.");
            self.base.insert(missing);
            self.fill(oracle);
        }
    }

    pub(crate) fn extend<T: Oracle<Alphabet = A, Length = FiniteLength, Output = C>>(
        &mut self,
        oracle: &mut T,
    ) -> bool {
        self.insert_extensions();
        self.fill(oracle)
    }

    pub(crate) fn fill<T: Oracle<Alphabet = A, Length = FiniteLength, Output = C>>(
        &mut self,
        oracle: &mut T,
    ) -> bool {
        trace!("Filling the table.");
        let mut changed = false;
        for (class, row) in self.rows.iter_mut() {
            for experiment in &self.experiments {
                if !row.outputs.contains_key(experiment) {
                    // update the row only if the experiment is not already present
                    let x: Vec<_> = class.iter().chain(experiment.iter()).cloned().collect();
                    let output =
                        oracle.output(RawWithLength::new_reverse_args(FiniteLength(x.len()), x));
                    row.outputs.insert(experiment.clone(), output);
                    changed |= true;
                }
            }
        }

        debug_assert!(self.rows_complete(), "Error in filling up rows.");
        changed
    }

    fn rows_complete(&self) -> bool {
        self.rows
            .values()
            .all(|row| row.outputs.len() == self.experiments.len())
    }
}

impl<A, C> Display for Table<A, C>
where
    A: Alphabet,
    A::Symbol: Display,
    C: Color + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = Builder::default();
        let header = std::iter::once("".to_string()).chain(self.experiments.iter().map(|class| {
            if class.is_empty() {
                "ε".to_string()
            } else {
                class.iter().join("")
            }
        }));
        builder.set_header(header);

        for row in self.rows.keys() {
            let row_name = if row.is_empty() {
                "ε".to_string()
            } else {
                row.iter().join("")
            };
            let printable_row_name = if self.base.contains(row) {
                row_name.bold().to_string()
            } else {
                row_name
            };
            let outputs = self
                .rows
                .get(row)
                .expect("row not found")
                .outputs()
                .into_iter()
                .map(|output| output.to_string());
            builder.push_record(std::iter::once(printable_row_name).chain(outputs));
        }

        let mut table = builder.build();
        table.with(Style::modern());
        write!(f, "{}", table)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn simple_observation_table() {}
}
