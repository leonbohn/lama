use std::{
    fmt::{Display, Formatter},
    hash::Hash,
    ops::Rem,
};

use itertools::Itertools;
use owo_colors::OwoColorize;
use tabled::{builder::Builder, settings::Style};

use crate::{
    BuchiCondition, Combined, Map, OmegaCondition, ParityCondition, StateIterable, TransitionSystem,
};

pub trait Annotates<X, Y> {
    fn annotate(&self, x: &(X, Y), z: &X) -> String;
}

pub trait DisplayState: TransitionSystem {
    fn display_state(&self, state: &Self::State) -> String;
}

pub trait DisplaySymbol: TransitionSystem {
    fn display_symbol(&self, sym: &Self::Input) -> String;
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
    TS: TransitionSystem,
    TS::State: Display,
{
    fn display_state(&self, state: &Self::State) -> String {
        format!("{}", state.blue())
    }
}

impl<TS: TransitionSystem> DisplaySymbol for TS {
    fn display_symbol(&self, sym: &Self::Input) -> String {
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

impl<
        Acc: Annotates<TS::State, TS::Input>,
        TS: TransitionSystem + StateIterable + DisplayState + DisplaySymbol,
    > Display for Combined<TS, Acc>
where
    <TS as TransitionSystem>::State: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut builder = Builder::default();
        let alphabet = self.ts().vec_alphabet().into_iter().sorted().collect_vec();
        builder.set_header(
            vec!["δ".bright_yellow().to_string()].into_iter().chain(
                alphabet
                    .iter()
                    .map(|s| s.purple().to_string())
                    .collect::<Vec<String>>(),
            ),
        );
        for state in self.states_iter().sorted() {
            let mut row = vec![state.to_string().blue().bold().to_string()];
            for sym in &alphabet {
                if let Some(target) = self.ts().succ(state, sym) {
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
