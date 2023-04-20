use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

use automata::{output::MealyCongruence, Class, Growable, Map, RightCongruence, Symbol};
use itertools::Itertools;
use owo_colors::OwoColorize;
use tabled::{builder::Builder, settings::Style};
use tracing::{debug, trace};

use super::oracle::Oracle;

/// Represents a row of an observation table. It stores a mapping that associates classes (which
/// are in essence words over `Input`) to outputs of type `Output`.
/// It should be ensured that each row maps every experiment to an output.
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Row<Input: Symbol, Output: Symbol> {
    outputs: BTreeMap<Class<Input>, Output>,
}

/// An observation table is used to store the obtained information during a run of
/// an active learning (in our case L*-esque) algorithm.
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Table<Input: Symbol, Output: Symbol> {
    alphabet: Vec<Input>,
    experiments: Vec<Class<Input>>,
    base: BTreeSet<Class<Input>>,
    rows: BTreeMap<Class<Input>, Row<Input, Output>>,
}

impl<Input, Output> Default for Row<Input, Output>
where
    Input: Symbol,
    Output: Symbol,
{
    fn default() -> Self {
        Self {
            outputs: BTreeMap::new(),
        }
    }
}

impl<Input, Output> Row<Input, Output>
where
    Input: Symbol,
    Output: Symbol,
{
    /// Returns a vec of the produced outputs.
    pub fn outputs(&self) -> Vec<Output> {
        self.outputs.values().cloned().collect_vec()
    }
}

impl<Input, Output> Table<Input, Output>
where
    Input: Symbol,
    Output: Symbol,
{
    pub fn new(alphabet: Vec<Input>) -> Self {
        let base_cases = std::iter::once(Class::epsilon())
            .chain(alphabet.iter().map(|sym| Class::letter(sym.clone())))
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
        for base in &self.base {
            for sym in &self.alphabet {
                let extension = base + sym;
                if !self.base.contains(&extension) {
                    self.rows.insert(extension, Row::default());
                }
            }
        }
    }

    pub(crate) fn reduce(&mut self) -> bool {
        let to_remove = self
            .base
            .iter()
            .rev()
            .filter(|class| {
                self.base
                    .iter()
                    .any(|other| other < class && self.rows.get(class) == self.rows.get(other))
            })
            .cloned()
            .collect_vec();

        let changed = to_remove.is_empty();
        self.base.retain(|class| !to_remove.contains(class));

        changed
    }

    pub(crate) fn to_hypothesis(&self) -> Option<MealyCongruence<Input, Output>> {
        if !self.is_closed() {
            return None;
        }

        let mut ts = RightCongruence::empty_trivial();
        let mut outputs = Map::new();

        for class in &self.base {
            ts.add_state(class);
        }

        for class in &self.base {
            for sym in &self.alphabet {
                let extension = class + sym;
                let target = self.equivalent_class(&extension).expect("class not found");
                ts.add_transition(class, sym.clone(), target);

                let output = self
                    .rows
                    .get(&extension)
                    .expect("row not found")
                    .outputs
                    .get(&Class::epsilon())
                    .expect("output not found");
                outputs.insert((class.clone(), sym.clone()), output.clone());
            }
        }

        Some(MealyCongruence::from_parts(ts, Class::epsilon(), outputs))
    }

    pub(crate) fn is_sane(&self) -> bool {
        if !self.base.contains(&Class::epsilon()) || !self.experiments.contains(&Class::epsilon()) {
            debug!("Base or experiments do not contain base class.");
            return false;
        }
        if !self
            .alphabet
            .iter()
            .map(|chr| Class::letter(chr))
            .all(|class| self.experiments.contains(&class) && self.base.contains(&class))
        {
            debug!("Base or experiments do not contain all letters.");
            return false;
        };
        true
    }

    pub(crate) fn missing_base(&self) -> Option<Class<Input>> {
        trace!("Checking if the table is closed.");

        for class in &self.base {
            for sym in &self.alphabet {
                let extension = class + sym;
                if self.equivalent_class(&extension).is_none() {
                    debug!("No row for extension {} found.", extension);
                    return Some(extension);
                }
            }
        }
        None
    }

    fn is_closed(&self) -> bool {
        self.missing_base().is_none() && self.is_sane() && self.rows_complete()
    }

    pub(crate) fn close<T: Oracle<Input = Input, Output = Output>>(&mut self, oracle: &mut T) {
        while !self.is_closed() {
            trace!("Table is not closed. Extending it.");
            self.extend(oracle);
            self.fill(oracle);
            if let Some(missing) = self.missing_base() {
                self.base.insert(missing);
            }
        }
    }

    pub(crate) fn equivalent_class(&self, class: &Class<Input>) -> Option<&Class<Input>> {
        self.base.iter().find(|base| {
            debug_assert!(self.rows.contains_key(base));
            self.rows.get(base) == self.rows.get(class)
        })
    }

    pub(crate) fn extend<T: Oracle<Input = Input, Output = Output>>(
        &mut self,
        oracle: &mut T,
    ) -> bool {
        self.insert_extensions();
        self.fill(oracle)
    }

    pub(crate) fn fill<T: Oracle<Input = Input, Output = Output>>(
        &mut self,
        oracle: &mut T,
    ) -> bool {
        trace!("Filling the table.");
        let mut changed = false;
        for (class, row) in self.rows.iter_mut() {
            for experiment in &self.experiments {
                if !row.outputs.contains_key(experiment) {
                    // update the row only if the experiment is not already present
                    let output = oracle.output(&(class + experiment));
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

impl<Input, Output> Display for Table<Input, Output>
where
    Input: Symbol,
    Output: Symbol,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = Builder::default();
        let header = std::iter::once("".to_string())
            .chain(self.experiments.iter().map(|class| format!("{}", class.0)));
        builder.set_header(header);

        for row in self.rows.keys() {
            let row_name = if self.base.contains(row) {
                row.0.bold().to_string()
            } else {
                row.0.to_string()
            };
            let outputs = self
                .rows
                .get(row)
                .expect("row not found")
                .outputs()
                .into_iter()
                .map(|output| output.to_string());
            builder.push_record(std::iter::once(row_name).chain(outputs));
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
