use chumsky::prelude::*;

use crate::{
    format::{AtomicProposition, LabelExpression, StateConjunction},
    value, AcceptanceCondition, AcceptanceInfo, AcceptanceName, AliasName, Id, Property, Token,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum HeaderItem {
    States(Id),
    Start(StateConjunction),
    AP(Vec<AtomicProposition>),
    Alias(AliasName, LabelExpression),
    Acceptance(Id, AcceptanceCondition),
    AcceptanceName(AcceptanceName, Vec<AcceptanceInfo>),
    // Correspond to tool name and optional version number
    Tool(String, Option<String>),
    Name(String),
    Properties(Vec<Property>),
}

fn item() -> impl Parser<Token, HeaderItem, Error = Simple<Token>> {
    let states = just(Token::Header("States".to_string()))
        .ignore_then(value::integer())
        .map(HeaderItem::States);

    let acceptance_name = just(Token::Header("acc-name".to_string()))
        .ignore_then(value::identifier())
        .try_map(|identifier, span| {
            AcceptanceName::try_from(identifier).map_err(|e| Simple::custom(span, e))
        })
        .then(value::acceptance_info().repeated())
        .map(|(name, info)| HeaderItem::AcceptanceName(name, info));

    let start = just(Token::Header("Start".to_string()))
        .ignore_then(value::integer().separated_by(just(Token::Op('&'))))
        .map(HeaderItem::Start);

    let aps = just(Token::Header("AP".to_string()))
        .ignore_then(value::integer())
        .then(value::text().repeated())
        .try_map(|(num, prop_labels), span| {
            if (num as usize) == prop_labels.len() {
                Ok(HeaderItem::AP(prop_labels))
            } else {
                Err(Simple::custom(
                    span,
                    format!("Expected {} aps but got {}", num, prop_labels.len()),
                ))
            }
        });

    let acceptance = just(Token::Header("Acceptance".to_string()))
        .ignore_then(value::integer())
        .then(value::acceptance_condition())
        .map(|(count, condition)| HeaderItem::Acceptance(count, condition));

    let alias = just(Token::Header("Alias".to_string()))
        .ignore_then(value::alias_name())
        .then(value::label_expression())
        .map(|(aname, expression)| HeaderItem::Alias(aname, expression));

    let name = just(Token::Header("name".to_string()))
        .ignore_then(value::text())
        .map(HeaderItem::Name);

    let tool = just(Token::Header("tool".to_string()))
        .ignore_then(value::text())
        .then(value::text().or_not())
        .map(|(tool, version)| HeaderItem::Tool(tool, version));

    let properties = just(Token::Header("properties".to_string()))
        .ignore_then(
            value::identifier()
                .try_map(|p, span| Property::try_from(p).map_err(|err| Simple::custom(span, err)))
                .repeated()
                .at_least(1),
        )
        .map(HeaderItem::Properties);

    chumsky::primitive::choice((
        states,
        acceptance,
        acceptance_name,
        start,
        aps,
        alias,
        name,
        tool,
        properties,
    ))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Header(Vec<HeaderItem>);

impl Header {
    pub fn parser() -> impl Parser<Token, Self, Error = Simple<Token>> {
        item().repeated().map(Header)
    }

    pub fn from_vec(value: Vec<HeaderItem>) -> Self {
        Self(value)
    }
}

#[cfg(test)]
mod tests {
    #[cfg(test)]
    #[allow(clippy::unnecessary_unwrap)]
    pub fn process_header(input: &str) -> Result<Vec<HeaderItem>, String> {
        use chumsky::Stream;

        use crate::{build_error_report, lexer};

        let (tokens, errs) = lexer::tokenizer().parse_recovery(input);
        if let Some(tokens) = tokens {
            let len = input.chars().count();
            let (ast, parse_errs) = Header::parser()
                .then_ignore(end())
                .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

            if parse_errs.is_empty() && ast.is_some() {
                let out = ast.unwrap();
                Ok(out.0)
            } else {
                Err(build_error_report(
                    input,
                    errs.into_iter()
                        .map(|err| err.map(|c| c.to_string()))
                        .chain(parse_errs.into_iter().map(|err| err.map(|c| c.to_string()))),
                ))
            }
        } else {
            Err(build_error_report(
                input,
                errs.into_iter().map(|err| err.map(|c| c.to_string())),
            ))
        }
    }

    use super::*;

    fn assert_header(input: &str, cmp: &[HeaderItem]) {
        match process_header(input) {
            Ok(res) => assert_eq!(res, cmp),
            Err(err) => {
                eprintln!("{}", err);
                unreachable!()
            }
        }
    }

    fn assert_fails(input: &str) {
        assert!(process_header(input).is_err())
    }

    #[test]
    fn property() {
        assert_header(
            r#"properties: trans-labels state-labels"#,
            &[HeaderItem::Properties(vec![
                Property::TransLabels,
                Property::StateLabels,
            ])],
        );
        assert_fails(r#"properties: trans-labels statelabels"#);
        assert_fails(r#"properties: "#);
    }

    #[test]
    fn tool_and_name() {
        assert_header(
            r#"
                tool: "ltl-translate" "1.2-alpha"
                name: "BA for GFa & GFb"
            "#,
            &[
                HeaderItem::Tool("ltl-translate".to_string(), Some("1.2-alpha".to_string())),
                HeaderItem::Name("BA for GFa & GFb".to_string()),
            ],
        );
        assert_fails("tool: ltl-translate \"1.2-alpha\"");
    }

    #[test]
    fn ariadne() {
        assert_header(
            "acc-name: Rabin 3",
            &[HeaderItem::AcceptanceName(
                AcceptanceName::Rabin,
                vec![AcceptanceInfo::Int(3)],
            )],
        );
        assert_header("Start: 0 & 7", &[HeaderItem::Start(vec![0, 7])]);
    }

    #[test]
    fn aps() {
        assert_header(
            r#"AP: 3 "a" "proc@state" "a[x] >= 2""#,
            &[HeaderItem::AP(vec![
                "a".to_string(),
                "proc@state".to_string(),
                "a[x] >= 2".to_string(),
            ])],
        )
    }

    #[test]
    fn alias() {
        assert_header(
            "Alias: @a 0",
            &[HeaderItem::Alias(
                "a".to_string(),
                LabelExpression::Integer(0),
            )],
        );
        assert_header(
            "Alias: @a 0 & 1",
            &[HeaderItem::Alias(
                "a".to_string(),
                LabelExpression::And(
                    Box::new(LabelExpression::Integer(0)),
                    Box::new(LabelExpression::Integer(1)),
                ),
            )],
        );

        // & binds stronger
        assert_header(
            "Alias: @a 1 | 2 & 0",
            &[HeaderItem::Alias(
                "a".to_string(),
                LabelExpression::Or(
                    Box::new(LabelExpression::Integer(1)),
                    Box::new(LabelExpression::And(
                        Box::new(LabelExpression::Integer(2)),
                        Box::new(LabelExpression::Integer(0)),
                    )),
                ),
            )],
        );
        assert_header(
            "Alias: @a 0 & 1 | 2",
            &[HeaderItem::Alias(
                "a".to_string(),
                LabelExpression::Or(
                    Box::new(LabelExpression::And(
                        Box::new(LabelExpression::Integer(0)),
                        Box::new(LabelExpression::Integer(1)),
                    )),
                    Box::new(LabelExpression::Integer(2)),
                ),
            )],
        );

        assert_header(
            "Alias: @a (0 | 1) & 2",
            &[HeaderItem::Alias(
                "a".to_string(),
                LabelExpression::And(
                    Box::new(LabelExpression::Or(
                        Box::new(LabelExpression::Integer(0)),
                        Box::new(LabelExpression::Integer(1)),
                    )),
                    Box::new(LabelExpression::Integer(2)),
                ),
            )],
        );
    }

    #[test]
    fn multiple_headers() {}
}
