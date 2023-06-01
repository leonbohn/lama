use std::{
    fmt::{Display, Formatter},
    hash::Hash,
    ops::Rem,
};

use itertools::Itertools;
use owo_colors::OwoColorize;
use tabled::{builder::Builder, settings::Style};

use crate::{
    congruence::CongruenceTrigger,
    output::Mapping,
    ts::{HasInput, HasStates, IntoStates, StateOf, StateReference},
    BuchiCondition, Class, Combined, Map, OmegaCondition, ParityCondition, Pointed, State,
    Successor, Symbol, Transformer, TransitionSystem, Value,
};

use super::{AnnotatesState, AnnotatesTransitionTarget, DisplayState, DisplaySymbol};

impl<
        Acc: AnnotatesTransitionTarget<TS::Q, TS::Sigma> + AnnotatesState<TS::Q>,
        TS: Successor + DisplayState + DisplaySymbol,
    > Display for Combined<TS, Acc>
where
    StateOf<TS>: Display + Ord,
    for<'a> &'a TS: IntoStates<Q = StateOf<TS>>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut builder = Builder::default();
        let alphabet = self.ts().input_alphabet().cloned().sorted().collect_vec();
        debug_assert!(
            alphabet.len() == self.ts().input_alphabet().unique().count(),
            "Alphabet contains duplicates"
        );

        builder.set_header(
            vec!["δ".bright_yellow().to_string()].into_iter().chain(
                alphabet
                    .iter()
                    .map(|s| s.purple().to_string())
                    .collect::<Vec<String>>(),
            ),
        );
        for state in self.ts().into_states().map(|s| s.state()).sorted() {
            let state_label = if state == self.initial() {
                format!("{}", self.acceptance().annotate_state(&state).underline())
            } else {
                self.acceptance().annotate_state(&state)
            };
            let mut row = vec![state_label];
            for sym in &alphabet {
                if let Some(target) = self.ts().successor(&state, sym) {
                    row.push(self.acceptance().annotate_transition_target((
                        state.clone(),
                        sym.clone(),
                        target,
                    )));
                } else {
                    row.push("⊥".dimmed().to_string());
                }
            }
            builder.push_record(row);
        }
        let mut transition_table = builder.build();
        transition_table.with(Style::modern());
        write!(f, "{}", transition_table)
    }
}

impl<Q: State + Display, S: Symbol + Display> Display for TransitionSystem<Q, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut builder = Builder::default();
        let alphabet = self.input_alphabet().cloned().sorted().collect_vec();
        debug_assert!(
            alphabet.len() == self.input_alphabet().unique().count(),
            "Alphabet contains duplicates"
        );

        builder.set_header(
            vec!["δ".bright_yellow().to_string()].into_iter().chain(
                alphabet
                    .iter()
                    .map(|s| s.purple().to_string())
                    .collect::<Vec<String>>(),
            ),
        );
        for state in self.into_states().map(|s| s.state()).sorted() {
            let mut row = vec![self.display_state(&state)];
            for sym in &alphabet {
                if let Some(target) = self.successor(&state, sym) {
                    row.push(self.display_state(&target));
                } else {
                    row.push("⊥".dimmed().to_string());
                }
            }
            builder.push_record(row);
        }
        let mut transition_table = builder.build();
        transition_table.with(Style::modern());
        write!(f, "{}", transition_table)
    }
}
