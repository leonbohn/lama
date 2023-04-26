//! This crate provides a parser for the HOA format.
#![warn(missing_docs)]
mod body;
mod display;
mod format;
mod header;
mod label_expression;
mod lexer;
mod value;

use std::fmt::Display;

pub use label_expression::LabelExpression;

use ariadne::{Color, Fmt, ReportKind, Source};

#[allow(unused_imports)]
use chumsky::prelude::*;
pub use format::*;

use chumsky::{prelude::Simple, Parser, Stream};
pub use format::{
    AcceptanceCondition, AcceptanceInfo, AcceptanceName, AcceptanceSignature, AliasName, Property,
};

pub use body::{Body, Edge, HoaSymbol, Label, State};
pub use header::{Header, HeaderItem};

use itertools::Itertools;
use lexer::Token;

/// The type of identifier used for states.
pub type Id = u32;

/// Represents a trigger as it is encoded in a HOA automaton.
pub type HoaTrigger = (Id, HoaSymbol);

/// Represents a transition as it is encoded in a HOA automaton.
pub type HoaTransition = (Id, HoaSymbol, Id);

/// Represents the different types of error that can be encountered when parsing a [`HoaAutomaton`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FromHoaError {
    /// The version string does not match, we only support v1.
    UnsupportedVersion(String),
    /// Encapsulates that an unsupported acceptance condition was used.
    UnsupportedAcceptanceCondition,
    /// An error occurred when parsing the acceptance condition.
    ParseAcceptanceCondition(String),
    /// There was an error in the body.
    UnsupportedBody,
    /// Lexer encountered an error, contains detailed report.
    LexerError(String),
    /// Parser encountered an error, contains detailed report.
    ParserError(String),
}

impl Display for FromHoaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FromHoaError::UnsupportedVersion(version) => {
                write!(f, "Unsupported HOA version ({})", version)
            }
            FromHoaError::UnsupportedAcceptanceCondition => {
                write!(f, "Unsupported acceptance condition")
            }
            FromHoaError::UnsupportedBody => write!(f, "Unsupported body"),
            FromHoaError::ParseAcceptanceCondition(message) => {
                write!(f, "Could not parse acceptance condition: {}", message)
            }
            FromHoaError::LexerError(rep) => write!(f, "Lexer error: {}", rep),
            FromHoaError::ParserError(rep) => write!(f, "Parser error: {}", rep),
        }
    }
}

/// Represents a parsed HOA automaton. It consists of a the version string,
/// a [`Header`] and a [`Body`].
/// The header contains all the information about the automaton (e.g. the number of states, the
/// acceptance condition, aliases etc.) and the body contains the actual transitions.
#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct HoaAutomaton {
    header: Header,
    body: Body,
}

/// Represents an acceptance condition as it is encoded in a HOA automaton.
pub type HoaAcceptance = (usize, AcceptanceCondition);

/// Stores information on aliases, it holds a vector of pairs of alias
/// names and label expression. This can be used to [unalias](HoaAutomaton::unalias) an automaton.
pub type Aliases = Vec<(AliasName, LabelExpression)>;

impl HoaAutomaton {
    /// Adds the given state.
    pub fn add_state(&mut self, state: State) {
        self.body.push(state);
    }

    /// Returns the version of the HOA file.
    pub fn version(&self) -> String {
        self.header.get_version().expect("Version must be set!")
    }

    /// Returns the header of the HOA file.
    pub fn header(&self) -> &Header {
        &self.header
    }

    /// Returns the body of the HOA file.
    pub fn body(&self) -> &Body {
        &self.body
    }

    fn from_parsed((header, body): (Header, Body)) -> Self {
        Self::from_parts(header, body)
    }

    /// Parses a HOA automaton from a string.
    pub fn parser() -> impl Parser<Token, Self, Error = Simple<Token>> {
        Header::parser()
            .then(Body::parser())
            .then_ignore(end())
            .map(HoaAutomaton::from_parsed)
    }

    /// Creates a new HOA automaton from the given version, header and
    /// body. This function will also [unalias](HoaAutomaton::unalias) the
    /// automaton.
    pub fn from_parts(header: Header, body: Body) -> Self {
        let mut out = Self { header, body };
        out.unalias();
        out
    }

    /// Verifies that the automaton is well-formed. This means that
    /// - the number of states is set correctly
    /// - all states are defined exactly once
    pub fn verify(&self) -> Result<(), String> {
        let mut errors = Vec::new();
        let mut states = Vec::new();
        for state in self.body().iter() {
            if states.contains(&state.id()) {
                errors.push(format!("State {} is defined more than once!", state.id()));
            }
            states.push(state.id());
        }
        if let Some(num_states) = self.num_states() {
            if states.len() != num_states {
                errors.push(format!(
                    "The number of states is set to {} but there are {} states!",
                    num_states,
                    states.len()
                ));
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors.join("\n"))
        }
    }

    /// Returns the number of states in the automaton.
    pub fn num_states(&self) -> Option<usize> {
        debug_assert!(
            self.header()
                .iter()
                .filter(|item| matches!(item, HeaderItem::States(_)))
                .count()
                == 1,
            "The number of states must be set exactly once!"
        );
        self.header().iter().find_map(|item| match item {
            HeaderItem::States(id) => Some(*id as usize),
            _ => None,
        })
    }

    /// Returns the number of edges in the automaton.
    pub fn start(&self) -> Vec<&StateConjunction> {
        debug_assert!(
            self.header()
                .iter()
                .filter(|item| matches!(item, HeaderItem::Start(_)))
                .count()
                >= 1,
            "At least one initial state conjunction has to be present!"
        );
        self.header()
            .iter()
            .filter_map(|item| match item {
                HeaderItem::Start(start) => Some(start),
                _ => None,
            })
            .collect()
    }

    /// Returns the set of all atomic propositions in the automaton.
    pub fn aps(&self) -> &Vec<String> {
        let aps = self
            .header()
            .iter()
            .filter_map(|item| match item {
                HeaderItem::AP(ap) => Some(ap),
                _ => None,
            })
            .collect_vec();
        debug_assert!(aps.len() == 1, "There must be exactly one AP header!");
        aps.first().unwrap()
    }

    /// Counts the number of atomic propositions in the automaton.
    pub fn num_aps(&self) -> usize {
        self.aps().len()
    }

    /// Returns the acceptance condition of the automaton.
    pub fn acceptance(&self) -> HoaAcceptance {
        debug_assert!(
            self.header()
                .iter()
                .filter(|item| matches!(item, HeaderItem::Acceptance(..)))
                .count()
                == 1,
            "There must be exactly one Acceptance header!"
        );
        self.header()
            .iter()
            .find_map(|item| match item {
                HeaderItem::Acceptance(acceptance_sets, condition) => {
                    Some((*acceptance_sets as usize, condition.clone()))
                }
                _ => None,
            })
            .expect("Acceptance header is missing!")
    }

    /// Returns the aliases of the automaton.
    pub fn aliases(&self) -> Vec<(AliasName, LabelExpression)> {
        self.header()
            .iter()
            .filter_map(|item| match item {
                HeaderItem::Alias(name, expr) => Some((name.clone(), expr.clone())),
                _ => None,
            })
            .collect()
    }

    /// Returns the acceptance name of the automaton.
    pub fn acceptance_name(&self) -> Option<(&AcceptanceName, &Vec<AcceptanceInfo>)> {
        debug_assert!(
            self.header()
                .iter()
                .filter(|item| matches!(item, HeaderItem::AcceptanceName(..)))
                .count()
                == 1,
            "There must be exactly one AcceptanceName header!"
        );
        self.header().iter().find_map(|item| match item {
            HeaderItem::AcceptanceName(name, info) => Some((name, info)),
            _ => None,
        })
    }

    /// Performs an unaliasing, which replaces each occurrence of an
    /// alias in a label expression with the corresponding label
    /// as given by the [aliases](HoaAutomaton::aliases) mapping.
    pub fn unalias(&mut self) {
        let aliases = self.aliases();
        for state in self.body.iter_mut() {
            for edge in state.edges_mut() {
                edge.label_mut().unalias(&aliases);
            }
        }
    }

    /// Adds a header item to the automaton.
    pub fn add_header_item(&mut self, item: HeaderItem) {
        self.header.push(item);
    }
}

// fn reporter<D: std::fmt::Display>(input: &str) -> impl Fn(D)

impl TryFrom<&str> for HoaAutomaton {
    type Error = FromHoaError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let input = value;
        let tokens = lexer::tokenizer()
            .parse(input)
            .map_err(|error_list| {
                build_error_report(
                    input,
                    error_list.into_iter().map(|err| err.map(|c| c.to_string())),
                )
            })
            .map_err(FromHoaError::LexerError)?;

        let length = input.chars().count();
        HoaAutomaton::parser()
            .parse(Stream::from_iter(length..length + 1, tokens.into_iter()))
            .map_err(|error_list| {
                build_error_report(
                    input,
                    error_list.into_iter().map(|err| err.map(|c| c.to_string())),
                )
            })
            .map_err(FromHoaError::ParserError)
    }
}

fn build_error_report<I: Iterator<Item = Simple<String>>>(input: &str, errs: I) -> String {
    errs.into_iter()
        .map(|e| {
            let report = ariadne::Report::build(ReportKind::Error, (), e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
                        "{}, expected {}",
                        if e.found().is_some() {
                            "Unexpected token in input"
                        } else {
                            "Unexpected end of input"
                        },
                        if e.expected().len() == 0 {
                            "something else".to_string()
                        } else {
                            e.expected()
                                .map(|expected| match expected {
                                    Some(expected) => expected.to_string(),
                                    None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    ))
                    .with_label(
                        ariadne::Label::new(e.span())
                            .with_message(format!(
                                "Unexpected token {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_label(
                        ariadne::Label::new(span.clone())
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Yellow)
                            ))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        ariadne::Label::new(e.span())
                            .with_message(format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                    ariadne::Label::new(e.span())
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            };

            let mut report_output = Vec::new();
            report
                .finish()
                .write(Source::from(input), &mut report_output)
                .unwrap();

            std::str::from_utf8(&report_output)
                .unwrap_or("Could not parse error report")
                .to_string()
        })
        .join("\n")
}

#[cfg(test)]
fn print_error_report<I: Iterator<Item = Simple<String>>>(input: &str, errs: I) {
    eprintln!("{}", build_error_report(input, errs))
}

#[cfg(test)]
mod tests {
    use crate::{
        body::{Edge, State},
        header::Header,
        AcceptanceAtom, AcceptanceCondition, AcceptanceName, AcceptanceSignature, Body, HeaderItem,
        HoaAutomaton, Label, LabelExpression, StateConjunction,
    };

    #[test]
    fn real_test_1() {
        let contents = r#"HOA: v1
             AP: 1 "a"
             States: 3
             Start: 0
             acc-name: Buchi
             Acceptance: 1 Inf(0)
             --BODY--
             State: 0 {0}
              [0] 1
              [!0]  2
             State: 1  /* former state 0 */
              [0] 1
              [!0] 2
             State: 2  /* former state 1 */
              [0] 1
              [!0] 2
             --END--
             "#;
        let hoa_aut = HoaAutomaton::try_from(contents);

        if let Err(err) = hoa_aut {
            println!("Encountered paring error\n{}", err);
            return;
        }

        let header = Header::from_vec(vec![
            HeaderItem::Version("v1".to_string()),
            HeaderItem::AP(vec!["a".to_string()]),
            HeaderItem::States(3),
            HeaderItem::Start(StateConjunction(vec![0])),
            HeaderItem::AcceptanceName(AcceptanceName::Buchi, vec![]),
            HeaderItem::Acceptance(1, AcceptanceCondition::Inf(AcceptanceAtom::Positive(0))),
        ]);
        let q0 = State::from_parts(
            0,
            None,
            vec![
                Edge::from_parts(
                    Label(LabelExpression::Integer(0)),
                    StateConjunction(vec![1]),
                    AcceptanceSignature(vec![0]),
                ),
                Edge::from_parts(
                    Label(LabelExpression::Not(Box::new(LabelExpression::Integer(0)))),
                    StateConjunction(vec![2]),
                    AcceptanceSignature(vec![0]),
                ),
            ],
        );
        let q1 = State::from_parts(
            1,
            None,
            vec![
                Edge::from_parts(
                    Label(LabelExpression::Integer(0)),
                    StateConjunction(vec![1]),
                    AcceptanceSignature(vec![]),
                ),
                Edge::from_parts(
                    Label(LabelExpression::Not(Box::new(LabelExpression::Integer(0)))),
                    StateConjunction(vec![2]),
                    AcceptanceSignature(vec![]),
                ),
            ],
        );
        let q2 = State::from_parts(
            2,
            None,
            vec![
                Edge::from_parts(
                    Label(LabelExpression::Integer(0)),
                    StateConjunction(vec![1]),
                    AcceptanceSignature(vec![]),
                ),
                Edge::from_parts(
                    Label(LabelExpression::Not(Box::new(LabelExpression::Integer(0)))),
                    StateConjunction(vec![2]),
                    AcceptanceSignature(vec![]),
                ),
            ],
        );
        assert_eq!(
            hoa_aut,
            Ok(HoaAutomaton::from_parts(
                header,
                Body::from(vec![q0, q1, q2])
            ))
        )
    }
}
