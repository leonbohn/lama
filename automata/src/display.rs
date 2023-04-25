use std::{
    fmt::{Display, Formatter},
    hash::Hash,
    ops::Rem,
};

use itertools::Itertools;
use owo_colors::OwoColorize;
use tabled::{builder::Builder, settings::Style};

use crate::{
    output::Mapping,
    ts::{HasStates, StateOf},
    BuchiCondition, Combined, Map, OmegaCondition, ParityCondition, Successor, Transformer, Value,
};

pub trait Annotates<X, Y> {
    fn annotate(&self, x: &(X, Y), z: &X) -> String;
}

pub trait DisplayState: Successor {
    fn display_state(&self, state: &Self::Q) -> String;
}

pub trait DisplaySymbol: Successor {
    fn display_symbol(&self, sym: &Self::Sigma) -> String;
}

impl<X, Y> Annotates<X, Y> for BuchiCondition<(X, Y)>
where
    X: Eq + Hash + Display,
    Y: Eq + Hash + Display,
{
    fn annotate(&self, x: &(X, Y), z: &X) -> String {
        match self.0.contains(x) {
            true => format!("{}", z.to_string().green().bold(),),
            false => format!("{}", z.to_string().red().bold(),),
        }
    }
}

impl<X, Y> Annotates<X, Y> for ParityCondition<(X, Y)>
where
    X: Eq + Hash + Display,
    Y: Eq + Hash + Display,
{
    fn annotate(&self, x: &(X, Y), z: &X) -> String {
        // if none is assigned, we take the highest value
        let priority = (0..self.0.len())
            .find(|i| self.0[*i].contains(x))
            .unwrap_or(self.0.len());
        match priority.rem(2) {
            0 => format!("{}", z.to_string().green().bold(),),
            1 => format!("{}", z.to_string().red().bold(),),
            _ => unreachable!(),
        }
    }
}

impl<X, Y> Annotates<X, Y> for OmegaCondition<(X, Y)>
where
    X: Eq + Hash + Display,
    Y: Eq + Hash + Display,
{
    fn annotate(&self, x: &(X, Y), z: &X) -> String {
        match self {
            OmegaCondition::Parity(c) => c.annotate(x, z),
            OmegaCondition::Buchi(c) => c.annotate(x, z),
        }
    }
}

impl<TS> DisplayState for TS
where
    TS: Successor,
    TS::Q: Display,
{
    fn display_state(&self, state: &Self::Q) -> String {
        format!("{}", state.blue())
    }
}

impl<TS: Successor> DisplaySymbol for TS {
    fn display_symbol(&self, sym: &Self::Sigma) -> String {
        format!("{}", sym.italic().purple())
    }
}

impl<X, Y, Z> Annotates<X, Y> for Map<(X, Y), Z>
where
    (X, Y): Eq + Hash,
    Y: Display,
    X: Display,
    Z: Display,
{
    fn annotate(&self, x: &(X, Y), z: &X) -> String {
        match self.get(x) {
            Some(v) => format!("{}@{}", z, v.to_string().bright_blue()),
            None => format!("{}", z.to_string().bright_cyan()),
        }
    }
}

impl<X, Y, O> Annotates<X, Y> for Mapping<(X, Y), O>
where
    (X, Y): Value,
    Y: Display,
    X: Display,
    O: Display + Value,
{
    fn annotate(&self, x: &(X, Y), z: &X) -> String {
        format!("{}@{}", z, self.apply(x).to_string().bright_blue())
    }
}

impl<Acc: Annotates<TS::Q, TS::Sigma>, TS: Successor + DisplayState + DisplaySymbol> Display
    for Combined<TS, Acc>
where
    StateOf<TS>: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut builder = Builder::default();
        let alphabet = self.ts().input_alphabet().cloned().sorted().collect_vec();
        builder.set_header(
            vec!["δ".bright_yellow().to_string()].into_iter().chain(
                alphabet
                    .iter()
                    .map(|s| s.purple().to_string())
                    .collect::<Vec<String>>(),
            ),
        );
        for state in self.states().sorted() {
            let mut row = vec![state.to_string().blue().bold().to_string()];
            for sym in &alphabet {
                if let Some(target) = self.ts().successor(state, sym) {
                    row.push(
                        self.acceptance()
                            .annotate(&(state.clone(), sym.clone()), &target),
                    );
                } else {
                    row.push("-".dimmed().to_string());
                }
            }
            builder.push_record(row);
        }
        let mut transition_table = builder.build();
        transition_table.with(Style::modern());
        write!(f, "{}", transition_table)
    }
}
