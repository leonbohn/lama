mod body;
mod format;
mod header;
mod lexer;
mod value;

use ariadne::{Color, Fmt, ReportKind, Source};

#[allow(unused_imports)]
use chumsky::prelude::*;
pub use format::*;

use chumsky::{prelude::Simple, Parser, Stream};
pub use format::{
    AcceptanceCondition, AcceptanceInfo, AcceptanceName, AcceptanceSignature, AliasName,
    LabelExpression, Property,
};

pub use body::{Body, Label};
pub use header::{Header, HeaderItem};

use itertools::Itertools;
use lexer::Token;

pub type Id = u32;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct HoaAutomaton {
    version: String,
    header: Header,
    body: Body,
}

impl HoaAutomaton {
    fn from_parsed(((version, header), body): ((String, Header), Body)) -> Self {
        Self {
            version,
            header,
            body,
        }
    }

    pub fn parser() -> impl Parser<Token, Self, Error = Simple<Token>> {
        just(Token::Header("HOA".to_string()))
            .ignore_then(value::identifier())
            .then(Header::parser())
            .then(Body::parser())
            .then_ignore(end())
            .map(HoaAutomaton::from_parsed)
    }

    pub fn from_parts(version: String, header: Header, body: Body) -> Self {
        Self {
            version,
            header,
            body,
        }
    }
}

// fn reporter<D: std::fmt::Display>(input: &str) -> impl Fn(D)

impl TryFrom<&str> for HoaAutomaton {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let input = value;
        let tokens = lexer::tokenizer().parse(input).map_err(|error_list| {
            build_error_report(
                input,
                error_list.into_iter().map(|err| err.map(|c| c.to_string())),
            )
        })?;

        let length = input.chars().count();
        HoaAutomaton::parser()
            .parse(Stream::from_iter(length..length + 1, tokens.into_iter()))
            .map_err(|error_list| {
                build_error_report(
                    input,
                    error_list.into_iter().map(|err| err.map(|c| c.to_string())),
                )
            })
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
        AcceptanceAtom, AcceptanceCondition, AcceptanceName, Body, HeaderItem, HoaAutomaton, Label,
        LabelExpression,
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
             --END--
             "#;
        let hoa_aut = HoaAutomaton::try_from(contents);

        if let Err(err) = hoa_aut {
            println!("Encountered paring error\n{}", err);
            return;
        }

        let header = Header::from_vec(vec![
            HeaderItem::AP(vec!["a".to_string()]),
            HeaderItem::States(3),
            HeaderItem::Start(vec![0]),
            HeaderItem::AcceptanceName(AcceptanceName::Buchi, vec![]),
            HeaderItem::Acceptance(1, AcceptanceCondition::Inf(AcceptanceAtom::Positive(0))),
        ]);
        let q0 = State::from_parts(
            0,
            None,
            vec![
                Edge::from_parts(Label(LabelExpression::Integer(0)), vec![1], vec![0]),
                Edge::from_parts(
                    Label(LabelExpression::Not(Box::new(LabelExpression::Integer(0)))),
                    vec![2],
                    vec![0],
                ),
            ],
        );
        let q1 = State::from_parts(
            1,
            None,
            vec![
                Edge::from_parts(Label(LabelExpression::Integer(0)), vec![1], vec![]),
                Edge::from_parts(
                    Label(LabelExpression::Not(Box::new(LabelExpression::Integer(0)))),
                    vec![2],
                    vec![],
                ),
            ],
        );
        assert_eq!(
            hoa_aut,
            Ok(HoaAutomaton::from_parts(
                "v1".to_string(),
                header,
                Body::from(vec![q0, q1])
            ))
        )
    }
}
