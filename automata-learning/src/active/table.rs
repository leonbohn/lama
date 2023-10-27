use std::{collections::BTreeSet, fmt::Debug};

use automata::prelude::*;
use itertools::Itertools;
use tracing::trace;

use crate::active::logging::LStarQuery;

use super::{logging::LStarLogger, oracle::Oracle};

pub type LStarExample<A, Q> = (Vec<<A as Alphabet>::Symbol>, Q);
pub type LStarExampleFor<Cong> = (Vec<SymbolOf<Cong>>, StateColorOf<Cong>);

pub type RowIndex = usize;

pub enum LStarHypothesisResult<const FOR_MEALY: bool, T: LStarTarget<FOR_MEALY>> {
    Success(T::Hypothesis),
    MissingRow(LStarRow<<T::Alphabet as Alphabet>::Symbol, T::Output, FOR_MEALY>),
    PromoteRow(RowIndex),
}

impl<const FOR_MEALY: bool, T: LStarTarget<FOR_MEALY>> LStarHypothesisResult<FOR_MEALY, T> {
    pub fn to_hypothesis(&self) -> Option<T::Hypothesis> {
        match self {
            LStarHypothesisResult::Success(hypothesis) => Some(hypothesis.clone()),
            _ => None,
        }
    }
}

pub trait LStarTarget<const FOR_MEALY: bool>: Sized + std::fmt::Debug {
    type Alphabet: Alphabet;
    type Output: Color + Debug;
    type Hypothesis: Congruence<Alphabet = Self::Alphabet>
        + Morphism<<Self::Alphabet as Alphabet>::Symbol, Output = Self::Output>
        + Clone;
    fn hypothesis(&self) -> LStarHypothesisResult<FOR_MEALY, Self>;
    fn accept_counterexample<
        O: Oracle<Self::Hypothesis, Length = FiniteLength, Output = Self::Output>,
    >(
        &mut self,
        hypothesis: &Self::Hypothesis,
        counterexample: LStarExample<Self::Alphabet, Self::Output>,
        oracle: &O,
    ) -> Vec<LStarQuery<FOR_MEALY, Self>>;
    fn new_table(
        alphabet: Self::Alphabet,
        experiments: Vec<Vec<<Self::Alphabet as Alphabet>::Symbol>>,
    ) -> Self;
    fn fill_rows<O: Oracle<Self::Hypothesis, Length = FiniteLength, Output = Self::Output>>(
        &mut self,
        oracle: &O,
    ) -> Vec<LStarQuery<FOR_MEALY, Self>>;
    fn promote_row(&mut self, row_index: usize);
    fn add_row(
        &mut self,
        row: LStarRow<<Self::Alphabet as Alphabet>::Symbol, Self::Output, FOR_MEALY>,
    );
    fn find_equivalent_row_index(&self, outputs: &[Self::Output]) -> Option<usize>;
    fn recompute_base(&mut self) -> usize;
}

pub struct LStarTable<A: Alphabet, C: Color, const FOR_MEALY: bool> {
    pub(crate) rows: LStarRows<A, C, FOR_MEALY>,
    pub(crate) experiments: LStarExperiments<A>,
    pub(crate) alphabet: A,
}

impl<A: Alphabet, C: Color, const FOR_MEALY: bool> HasAlphabet for LStarTable<A, C, FOR_MEALY> {
    type Alphabet = A;
    fn alphabet(&self) -> &Self::Alphabet {
        &self.alphabet
    }
}

impl<A: Alphabet, C: Color + Debug + Default> LStarTarget<false> for LStarTable<A, C, false> {
    type Alphabet = A;

    type Output = C;

    type Hypothesis = MooreMachine<A, C>;

    fn add_row(
        &mut self,
        row: LStarRow<<Self::Alphabet as Alphabet>::Symbol, Self::Output, false>,
    ) {
        self.rows.push(row)
    }

    fn recompute_base(&mut self) -> usize {
        trace!(
            "Recomputing base, currently have {} rows {}",
            self.rows.len(),
            self.rows
                .iter()
                .map(|row| format!(
                    "\"{}\"{}",
                    row.base.iter().map(|sym| format!("{:?}", sym)).join(""),
                    if row.minimal { "*" } else { "" }
                ))
                .join(", ")
        );
        debug_assert!(self.rows[0].minimal);
        let mut base_size = 1;
        for i in 1..self.rows.len() {
            if self.rows[i].minimal {
                base_size += 1;
                continue;
            } else if let Some(equivalent_row_idx) =
                self.find_equivalent_row_index(&self.rows[i].outputs)
            {
                assert!(equivalent_row_idx <= i);
                if equivalent_row_idx == i {
                    // this is actually a minimal row
                    self.rows[i].minimal = true;
                    base_size += 1;
                } else {
                    self.rows[i].minimal = false;
                }
            } else {
                base_size += 1;
                self.rows[i].minimal = true
            }
        }
        base_size
    }

    fn promote_row(&mut self, row_index: usize) {
        assert!(self.rows.len() > row_index);
        self.rows[row_index].minimal = true;
        for sym in self.alphabet.universe() {
            let mut extension: Vec<_> = self.rows[row_index].base.clone();
            extension.push(*sym);
            if !self.rows.iter().any(|row| row.base == extension) {
                self.rows.push(LStarRow::without_outputs(extension, false));
            }
        }
    }

    fn hypothesis(&self) -> LStarHypothesisResult<false, Self> {
        trace!(
            "Computing Moore hypothesis with experiments {{{}}}",
            self.experiments
                .iter()
                .map(|experiment| experiment.iter().map(|sym| format!("{:?}", sym)).join(""))
                .join(", ")
        );

        assert!(!self.rows.is_empty(), "Cannot have no states!");
        assert!(
            !self.rows[0].outputs.is_empty(),
            "Cannot have no experiments!"
        );

        let mut out: MooreMachine<A, C> =
            MooreMachine::new(self.alphabet.clone(), self.rows[0].outputs[0].clone());
        let state_mapping: Vec<_> = std::iter::once((0, out.initial()))
            .chain((1..self.rows.len()).filter_map(|i| {
                let row = &self.rows[i];
                if row.minimal {
                    Some((i, out.add_state(row.outputs[0].clone())))
                } else {
                    None
                }
            }))
            .collect();
        trace!(
            "Considering {} base states {{{}}}",
            state_mapping.len(),
            state_mapping
                .iter()
                .map(|(i, _)| format!(
                    "{} | {}",
                    self.rows[*i]
                        .base
                        .iter()
                        .map(|sym| format!("{:?}", sym))
                        .join(""),
                    self.rows
                        .iter()
                        .find_map(|row| if row.base == self.rows[*i].base {
                            Some(
                                row.outputs
                                    .iter()
                                    .map(|sym| format!("{:?}", sym))
                                    .join(", "),
                            )
                        } else {
                            None
                        })
                        .unwrap()
                ))
                .join(", ")
        );
        for i in 0..state_mapping.len() {
            let (i, state) = &state_mapping[i];
            'transition: for sym in self.alphabet.universe() {
                let mut extension: Vec<_> = self.rows[*i]
                    .base
                    .iter()
                    .cloned()
                    .chain(std::iter::once(*sym))
                    .collect();
                match self
                    .rows
                    .iter()
                    .enumerate()
                    .find(|(idx, r)| r.base() == extension)
                {
                    None => {
                        return LStarHypothesisResult::MissingRow(LStarRow::without_outputs(
                            extension, false,
                        ));
                    }
                    Some((row_index, found_row)) => {
                        trace!(
                            "Searching equivalent row for {}|[{}]",
                            found_row
                                .base
                                .iter()
                                .map(|sym| format!("{:?}", sym))
                                .join(""),
                            found_row
                                .outputs
                                .iter()
                                .map(|sym| format!("{:?}", sym))
                                .join(", ")
                        );
                        if let Some(equivalent_row_idx) =
                            self.find_equivalent_row_index(&found_row.outputs)
                        {
                            assert!(equivalent_row_idx < self.rows.len());
                            let equivalent_row = self.rows[equivalent_row_idx].clone();
                            trace!(
                                "Found equivalent row {}|[{}] with index {equivalent_row_idx}",
                                equivalent_row
                                    .base
                                    .iter()
                                    .map(|sym| format!("{:?}", sym))
                                    .join(""),
                                equivalent_row
                                    .outputs
                                    .iter()
                                    .map(|sym| format!("{:?}", sym))
                                    .join(", "),
                            );

                            let target_state_idx = state_mapping
                                .iter()
                                .find_map(|(i, j)| {
                                    if *i == equivalent_row_idx {
                                        Some(*j)
                                    } else {
                                        None
                                    }
                                })
                                .expect("We must have a base row available!");

                            out.add_edge(*state, A::expression(*sym), target_state_idx, ());
                            continue 'transition;
                        } else {
                            trace!("No equivalent row exists, promoting {row_index} to a state");
                            return LStarHypothesisResult::PromoteRow(row_index);
                        }
                    }
                }
            }
        }
        LStarHypothesisResult::Success(out)
    }

    fn accept_counterexample<
        O: Oracle<Self::Hypothesis, Length = FiniteLength, Output = Self::Output>,
    >(
        &mut self,
        hypothesis: &Self::Hypothesis,
        (counterexample, expected_color): LStarExample<Self::Alphabet, Self::Output>,
        oracle: &O,
    ) -> Vec<LStarQuery<false, Self>> {
        debug_assert!(
            hypothesis.morph(&counterexample) != expected_color,
            "This is not a real counterexample"
        );
        debug_assert!(
            !counterexample.is_empty(),
            "Counterexample must not be empty"
        );

        let mut queries = vec![];
        let mut state = hypothesis.initial();
        let mut loop_start = 0;

        let mut previous_color = expected_color;
        for i in loop_start..counterexample.len() {
            let sym = counterexample[i];
            let next_state = hypothesis
                .successor_index(state, sym)
                .expect("We assume the hypothesis to be deterministic!");
            let suffix = counterexample.offset(i);
            let next_color = hypothesis.state_color(next_state).unwrap();

            if next_color != previous_color {
                // this is the breakpoint
                let experiment = suffix.finite_to_vec();

                for row_index in 0..self.rows.len() {
                    let word = (&self.rows[row_index].base).append(&experiment);
                    let output = oracle.output(&word);
                    queries.push(LStarQuery::Membership((
                        word.finite_to_vec(),
                        output.clone(),
                    )));
                }

                trace!(
                    "We add \"{}\" to the experiments {}",
                    experiment.iter().map(|sym| format!("{:?}", sym)).join(""),
                    self.experiments
                        .iter()
                        .map(|experiment| format!(
                            "\"{}\"",
                            experiment.iter().map(|sym| format!("{:?}", sym)).join("")
                        ))
                        .join(", ")
                );
                assert!(!self.experiments.contains(&experiment));
                self.experiments.push(experiment);
                return queries;
            }
        }
        unreachable!("A breakpoint has to exist!");
    }

    fn new_table(
        alphabet: Self::Alphabet,
        experiments: Vec<Vec<<Self::Alphabet as Alphabet>::Symbol>>,
    ) -> Self {
        let rows = vec![LStarRow::without_outputs(vec![], true)];
        Self {
            rows,
            experiments,
            alphabet,
        }
    }

    fn fill_rows<O: Oracle<Self::Hypothesis, Length = FiniteLength, Output = Self::Output>>(
        &mut self,
        oracle: &O,
    ) -> Vec<LStarQuery<false, Self>> {
        let mut queries = vec![];
        for row in self.rows.iter_mut() {
            if row.len_outputs() == self.experiments.len() {
                continue;
            }
            for i in row.len_outputs()..self.experiments.len() {
                let word = (&row.base).append(&self.experiments[i]);
                let output = oracle.output(&word);
                trace!("Obtained output {:?} for word {:?}", output, word);
                queries.push(LStarQuery::Membership((
                    word.finite_to_vec(),
                    output.clone(),
                )));
                row.outputs.push(output);
            }
        }
        queries
    }

    fn find_equivalent_row_index(&self, outputs: &[Self::Output]) -> Option<usize> {
        self.rows.iter().position(|r| r.outputs == outputs)
    }
}

impl<A: Alphabet> LStarTarget<true> for LStarTable<A, usize, true> {
    type Alphabet = A;
    type Output = usize;
    type Hypothesis = MealyMachine<A, usize>;

    fn add_row(&mut self, row: LStarRow<<Self::Alphabet as Alphabet>::Symbol, Self::Output, true>) {
        self.rows.push(row)
    }

    fn recompute_base(&mut self) -> usize {
        trace!(
            "Recomputing base, currently have {} rows {}",
            self.rows.len(),
            self.rows
                .iter()
                .map(|row| format!(
                    "\"{}\"{}",
                    row.base.iter().map(|sym| format!("{:?}", sym)).join(""),
                    if row.minimal { "*" } else { "" }
                ))
                .join(", ")
        );
        debug_assert!(self.rows[0].minimal);
        let mut base_size = 1;
        for i in 1..self.rows.len() {
            if self.rows[i].minimal {
                base_size += 1;
                continue;
            } else if let Some(equivalent_row_idx) =
                self.find_equivalent_row_index(&self.rows[i].outputs)
            {
                assert!(equivalent_row_idx <= i);
                if equivalent_row_idx == i {
                    // this is actually a minimal row
                    self.rows[i].minimal = true;
                    base_size += 1;
                } else {
                    self.rows[i].minimal = false;
                }
            } else {
                base_size += 1;
                self.rows[i].minimal = true
            }
        }
        base_size
    }

    fn promote_row(&mut self, row_index: usize) {
        assert!(self.rows.len() > row_index);
        self.rows[row_index].minimal = true;
        for sym in self.alphabet.universe() {
            let mut extension: Vec<_> = self.rows[row_index].base.clone();
            extension.push(*sym);
            if !self.rows.iter().any(|row| row.base == extension) {
                self.rows.push(LStarRow::without_outputs(extension, false));
            }
        }
    }

    fn hypothesis(&self) -> LStarHypothesisResult<true, Self> {
        trace!(
            "Computing Moore hypothesis with experiments {{{}}}",
            self.experiments
                .iter()
                .map(|experiment| experiment.iter().map(|sym| format!("{:?}", sym)).join(""))
                .join(", ")
        );

        assert!(!self.rows.is_empty(), "Cannot have no states!");
        assert!(
            !self.rows[0].outputs.is_empty(),
            "Cannot have no experiments!"
        );

        let mut out: MealyMachine<A> = MealyMachine::new(self.alphabet.clone());
        let state_mapping: Vec<_> = std::iter::once((0, out.initial()))
            .chain((1..self.rows.len()).filter_map(|i| {
                let row = &self.rows[i];
                if row.minimal {
                    Some((i, out.add_state(())))
                } else {
                    None
                }
            }))
            .collect();
        trace!(
            "Considering {} base states {{{}}}",
            state_mapping.len(),
            state_mapping
                .iter()
                .map(|(i, _)| format!(
                    "{} | {}",
                    self.rows[*i]
                        .base
                        .iter()
                        .map(|sym| format!("{:?}", sym))
                        .join(""),
                    self.rows
                        .iter()
                        .find_map(|row| if row.base == self.rows[*i].base {
                            Some(
                                row.outputs
                                    .iter()
                                    .map(|sym| format!("{:?}", sym))
                                    .join(", "),
                            )
                        } else {
                            None
                        })
                        .unwrap()
                ))
                .join(", ")
        );
        for i in 0..state_mapping.len() {
            let (i, state) = &state_mapping[i];
            'transition: for sym in self.alphabet.universe() {
                let mut extension: Vec<_> = self.rows[*i]
                    .base
                    .iter()
                    .cloned()
                    .chain(std::iter::once(*sym))
                    .collect();

                let symbol_position = self
                    .experiments
                    .iter()
                    .position(|e| e.len() == 1 && e[0] == *sym)
                    .expect("Single symbol experiments must be present!");
                let output_color = self.rows[*i].outputs[symbol_position];

                match self
                    .rows
                    .iter()
                    .enumerate()
                    .find(|(idx, r)| r.base() == extension)
                {
                    None => {
                        return LStarHypothesisResult::MissingRow(LStarRow::without_outputs(
                            extension, false,
                        ));
                    }
                    Some((row_index, found_row)) => {
                        trace!(
                            "Searching equivalent row for {}|[{}]",
                            found_row
                                .base
                                .iter()
                                .map(|sym| format!("{:?}", sym))
                                .join(""),
                            found_row
                                .outputs
                                .iter()
                                .map(|sym| format!("{:?}", sym))
                                .join(", ")
                        );
                        if let Some(equivalent_row_idx) =
                            self.find_equivalent_row_index(&found_row.outputs)
                        {
                            assert!(equivalent_row_idx < self.rows.len());
                            let equivalent_row = self.rows[equivalent_row_idx].clone();
                            trace!(
                                "Found equivalent row {}|[{}] with index {equivalent_row_idx}",
                                equivalent_row
                                    .base
                                    .iter()
                                    .map(|sym| format!("{:?}", sym))
                                    .join(""),
                                equivalent_row
                                    .outputs
                                    .iter()
                                    .map(|sym| format!("{:?}", sym))
                                    .join(", "),
                            );

                            let target_state_idx = state_mapping
                                .iter()
                                .find_map(|(i, j)| {
                                    if *i == equivalent_row_idx {
                                        Some(*j)
                                    } else {
                                        None
                                    }
                                })
                                .expect("We must have a base row available!");

                            out.add_edge(
                                *state,
                                A::expression(*sym),
                                target_state_idx,
                                output_color,
                            );
                            continue 'transition;
                        } else {
                            trace!("No equivalent row exists, promoting {row_index} to a state");
                            return LStarHypothesisResult::PromoteRow(row_index);
                        }
                    }
                }
            }
        }
        LStarHypothesisResult::Success(out)
    }

    fn accept_counterexample<
        O: Oracle<Self::Hypothesis, Length = FiniteLength, Output = Self::Output>,
    >(
        &mut self,
        hypothesis: &Self::Hypothesis,
        (counterexample, expected_color): LStarExample<Self::Alphabet, Self::Output>,
        oracle: &O,
    ) -> Vec<LStarQuery<true, Self>> {
        debug_assert!(
            hypothesis.morph(&counterexample) != expected_color,
            "This is not a real counterexample"
        );
        debug_assert!(
            !counterexample.is_empty(),
            "Counterexample must not be empty"
        );

        let mut queries = vec![];
        let mut state = hypothesis.initial();
        let mut loop_start = 0;

        let mut previous_color = expected_color;
        for i in loop_start..counterexample.len() {
            let sym = counterexample[i];
            let transition = hypothesis
                .transition(state, sym)
                .expect("We assume the hypothesis to be deterministic!");
            let suffix = counterexample.offset(i);
            let next_color = transition.color();
            state = transition.target();

            if next_color != previous_color {
                // this is the breakpoint
                let experiment = suffix.finite_to_vec();

                for row_index in 0..self.rows.len() {
                    let word = (&self.rows[row_index].base).append(&experiment);
                    let output = oracle.output(&word);
                    queries.push(LStarQuery::Membership((word.finite_to_vec(), output)));
                }

                trace!(
                    "We add \"{}\" to the experiments {}",
                    experiment.iter().map(|sym| format!("{:?}", sym)).join(""),
                    self.experiments
                        .iter()
                        .map(|experiment| format!(
                            "\"{}\"",
                            experiment.iter().map(|sym| format!("{:?}", sym)).join("")
                        ))
                        .join(", ")
                );
                assert!(!self.experiments.contains(&experiment));
                self.experiments.push(experiment);
                return queries;
            }
        }
        unreachable!("A breakpoint has to exist!");
    }

    fn new_table(
        alphabet: Self::Alphabet,
        experiments: Vec<Vec<<Self::Alphabet as Alphabet>::Symbol>>,
    ) -> Self {
        let rows = vec![LStarRow::without_outputs(vec![], true)];
        Self {
            rows,
            experiments,
            alphabet,
        }
    }

    fn fill_rows<O: Oracle<Self::Hypothesis, Length = FiniteLength, Output = Self::Output>>(
        &mut self,
        oracle: &O,
    ) -> Vec<LStarQuery<true, Self>> {
        let mut queries = vec![];
        for row in self.rows.iter_mut() {
            if row.len_outputs() == self.experiments.len() {
                continue;
            }
            for i in row.len_outputs()..self.experiments.len() {
                let word = (&row.base).append(&self.experiments[i]);
                let output = oracle.output(&word);
                trace!("Obtained output {:?} for word {:?}", output, word);
                queries.push(LStarQuery::Membership((word.finite_to_vec(), output)));
                row.outputs.push(output);
            }
        }
        queries
    }

    fn find_equivalent_row_index(&self, outputs: &[Self::Output]) -> Option<usize> {
        self.rows.iter().position(|r| r.outputs == outputs)
    }
}

/// Represents an individual row in an L* table. Consists of a `base`, which can be viewed as
/// an access string to the state that corresponds/is equivalent to the row. The marker `minimal`
/// is `true` iff the row is minimal, i.e. there is no row with a llex-smaller `base` that has
/// equivalent outputs. Finally, the `outputs` are a vector of the outputs of the row for each
/// experiment. Formally, if the base is `b` and the `i`-th experiment is `e`, then `outputs[i]`
/// is equal to the output of the target for `be`.
#[derive(Debug, Clone)]
pub struct LStarRow<S: Symbol, C: Color, const FOR_MEALY: bool> {
    pub(crate) base: Vec<S>,
    pub(crate) minimal: bool,
    pub(crate) outputs: Vec<C>,
}

impl<S: Symbol, C: Color, const FOR_MEALY: bool> LStarRow<S, C, FOR_MEALY> {
    pub fn new(base: Vec<S>, outputs: Vec<C>) -> Self {
        Self {
            minimal: base.is_empty(),
            base,
            outputs,
        }
    }

    pub fn len_outputs(&self) -> usize {
        self.outputs.len()
    }

    pub fn without_outputs(base: Vec<S>, minimal: bool) -> Self {
        Self {
            minimal,
            base,
            outputs: vec![],
        }
    }

    pub fn get(&self, index: usize) -> Option<&C> {
        self.outputs.get(index)
    }

    pub fn base(&self) -> &[S] {
        &self.base
    }
}

pub type LStarExperiments<A> = Vec<Vec<<A as Alphabet>::Symbol>>;
pub type LStarRows<A, C, const FOR_MEALY: bool> =
    Vec<LStarRow<<A as Alphabet>::Symbol, C, FOR_MEALY>>;

impl<A: Alphabet, C: Color + Debug, const FM: bool> std::fmt::Debug for LStarTable<A, C, FM> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = tabled::builder::Builder::default();
        builder.set_header(
            std::iter::once("MR".to_string()).chain(
                self.experiments
                    .iter()
                    .map(|e| e.iter().map(|sym| format!("{:?}", sym)).join("")),
            ),
        );
        for r in &self.rows {
            let base_name = r.base.iter().map(|sym| format!("{:?}", sym)).join("");
            let mut table_row = vec![if r.minimal {
                owo_colors::OwoColorize::blue(&format!("[{}]", base_name)).to_string()
            } else {
                base_name
            }];
            for o in &r.outputs {
                table_row.push(format!("{:?}", o));
            }
            builder.push_record(table_row);
        }

        write!(
            f,
            "{}",
            builder.build().with(tabled::settings::Style::rounded())
        )
    }
}
