use automata::{output::MealyCongruence, Class, Growable, Mapping, RightCongruence, Set, Symbol};

use super::teacher::Oracle;

#[derive(Debug, Clone, Eq, PartialEq)]
struct Row<Input: Symbol, Output: Symbol> {
    outputs: Mapping<Class<Input>, Output>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct SquareTable<Input: Symbol, Output: Symbol> {
    alphabet: Vec<Input>,
    experiments: Vec<Class<Input>>,
    base: Set<Class<Input>>,
    rows: Mapping<Class<Input>, Row<Input, Output>>,
}

impl<Input, Output> Row<Input, Output>
where
    Input: Symbol,
    Output: Symbol,
{
    pub fn empty() -> Self {
        Self {
            outputs: Mapping::new(),
        }
    }
}

impl<Input, Output> SquareTable<Input, Output>
where
    Input: Symbol,
    Output: Symbol,
{
    fn to_hypothesis(&self) -> Option<MealyCongruence<Input, Output>> {
        if !self.is_closed() {
            return None;
        }

        let mut ts = RightCongruence::empty_trivial();
        let mut outputs = Mapping::new();

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

    fn is_closed(&self) -> bool {
        if !self.base.contains(&Class::epsilon()) || !self.experiments.contains(&Class::epsilon()) {
            return false;
        }

        if !self
            .alphabet
            .iter()
            .map(|chr| Class::letter(chr))
            .all(|class| self.experiments.contains(&class) && self.base.contains(&class))
        {
            return false;
        };

        for class in &self.base {
            for sym in &self.alphabet {
                let extension = class + sym;
                if let Some(row) = self.rows.get(&extension) {
                    // if the row is not complete, then the table is not closed
                    if row.outputs.len() != self.experiments.len() {
                        return false;
                    }

                    // If something equivalent to the row is not yet present in the base, then the table is not closed.
                    if !self
                        .base
                        .iter()
                        .any(|base| self.rows.get(base).expect("base class not found") == row)
                    {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }
        true
    }

    fn close<T: Oracle<Input = Input, Output = Output>>(&mut self, oracle: &mut T) {
        while !self.is_closed() {
            self.extend(oracle);
        }
    }

    fn equivalent_class(&self, class: &Class<Input>) -> Option<&Class<Input>> {
        self.base.iter().find(|base| {
            Some(self.rows.get(base).expect("base class not found")) == self.rows.get(class)
        })
    }

    fn extend<T: Oracle<Input = Input, Output = Output>>(&mut self, oracle: &mut T) -> bool {
        let mut changed = self.base.insert(Class::epsilon());

        for class in &self.base {
            for sym in &self.alphabet {
                let extension = class + sym;

                // ensure that the extension is a valid row
                if !self.rows.contains_key(&extension) {
                    changed |= true;
                    self.rows.insert(extension.clone(), Row::empty());
                }
            }
        }
        self.fill(oracle);
        changed
    }

    fn fill<T: Oracle<Input = Input, Output = Output>>(&mut self, oracle: &mut T) {
        for (class, row) in self.rows.iter_mut() {
            for experiment in &self.experiments {
                if !row.outputs.contains_key(experiment) {
                    // update the row only if the experiment is not already present
                    let output = oracle.output(&(class + experiment));
                    row.outputs.insert(experiment.clone(), output);
                }
            }
        }
    }
}

pub trait ObservationTable<S: Symbol> {
    fn new(alphabet: Vec<S>) -> Self;
    fn alphabet(&self) -> &[S];
}

#[cfg(test)]
mod tests {
    #[test]
    fn simple_observation_table() {}
}
