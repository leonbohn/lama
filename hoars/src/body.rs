use chumsky::prelude::*;

use crate::{lexer::Token, value, AcceptanceSignature, Id, LabelExpression, StateConjunction};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Label(pub(crate) LabelExpression);

#[derive(Clone, Debug)]
pub struct RawState(
    Option<Label>,
    Id,
    Option<String>,
    Option<AcceptanceSignature>,
);

#[derive(Clone, Debug, PartialEq, Eq)]
struct ExplicitEdge(Label, StateConjunction, Option<AcceptanceSignature>);

#[derive(Clone, Debug, PartialEq, Eq)]
struct ImplicitEdge(StateConjunction, Option<AcceptanceSignature>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Edge(Label, StateConjunction, AcceptanceSignature);

impl Edge {
    pub fn from_parts(
        label_expression: Label,
        state_conjunction: StateConjunction,
        acceptance_signature: AcceptanceSignature,
    ) -> Self {
        Self(label_expression, state_conjunction, acceptance_signature)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State(Id, Option<String>, Vec<Edge>);

impl State {
    pub fn from_parts(id: Id, comment: Option<String>, edges: Vec<Edge>) -> Self {
        Self(id, comment, edges)
    }
}

impl From<(Option<AcceptanceSignature>, ExplicitEdge)> for Edge {
    fn from((state_acc, edge): (Option<AcceptanceSignature>, ExplicitEdge)) -> Self {
        let acc = match (state_acc, &edge.2) {
            (None, None) => Vec::new(),
            (Some(acc), None) => acc,
            (None, Some(acc)) => acc.clone(),
            (Some(left), Some(right)) => left.into_iter().chain(right.iter().cloned()).collect(),
        };
        Edge(edge.0, edge.1, acc)
    }
}

impl TryFrom<(RawState, Vec<ExplicitEdge>)> for State {
    type Error = String;

    fn try_from((state, edges): (RawState, Vec<ExplicitEdge>)) -> Result<Self, Self::Error> {
        let mut out_edges = vec![];
        let RawState(state_label, id, state_text, state_acc) = state;

        if state_label.is_some() {
            return Err("Transformation from state-based to transition-based requires adding a new initial state etc (see example 'non-deterministic state-based Büchi Automaton')".to_string());
        }

        for raw_edge in edges {
            out_edges.push(Edge::from((state_acc.clone(), raw_edge)));
        }

        Ok(State(id, state_text, out_edges))
    }
}

pub fn label() -> impl Parser<Token, Label, Error = Simple<Token>> {
    just(Token::Paren('['))
        .ignore_then(value::label_expression())
        .then_ignore(just(Token::Paren(']')))
        .map(Label)
}

fn explicit_edge() -> impl Parser<Token, ExplicitEdge, Error = Simple<Token>> {
    label()
        .then(value::state_conjunction())
        .then(value::acceptance_signature().or_not())
        .map(|((label, state_conjunction), acceptance_signature)| {
            ExplicitEdge(label, state_conjunction, acceptance_signature)
        })
}

#[allow(unused)]
fn implicit_edge() -> impl Parser<Token, ImplicitEdge, Error = Simple<Token>> {
    value::state_conjunction()
        .then(value::acceptance_signature().or_not())
        .map(|(label, acceptance_signature)| ImplicitEdge(label, acceptance_signature))
}

pub fn state() -> impl Parser<Token, State, Error = Simple<Token>> {
    just(Token::Header("State".to_string()))
        .ignore_then(
            label()
                .or_not()
                .then(value::integer())
                .then(value::text().or_not())
                .then(value::acceptance_signature().or_not())
                .map(|(((l, i), t), a)| RawState(l, i, t, a)),
        )
        .then(explicit_edge().repeated())
        .try_map(|input, span| State::try_from(input).map_err(|err| Simple::custom(span, err)))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body(Vec<State>);

impl Body {
    pub fn parser() -> impl Parser<Token, Self, Error = Simple<Token>> {
        just(Token::BodyStart)
            .ignore_then(state().repeated())
            .map(Body)
            .then_ignore(just(Token::BodyEnd))
    }
}

impl From<Vec<State>> for Body {
    fn from(value: Vec<State>) -> Self {
        Body(value)
    }
}

#[cfg(test)]
mod tests {
    use chumsky::{primitive::end, Parser, Stream};

    use crate::{lexer, LabelExpression};

    use super::{Edge, Label, State};

    pub fn in_tags(input: &str) -> String {
        format!("--BODY--\n{}\n--END--", input)
    }

    #[cfg(test)]
    pub fn process_body(input: &str) -> Result<Vec<State>, ()> {
        use crate::{body::Body, print_error_report};

        let tokens = lexer::tokenizer().parse(input).map_err(|error_list| {
            print_error_report(
                input,
                error_list.into_iter().map(|err| err.map(|c| c.to_string())),
            )
        })?;

        for tok in &tokens {
            print!("{}", tok.0);
        }
        let len = input.chars().count();
        let ast = Body::parser()
            .then_ignore(end())
            .parse(Stream::from_iter(len..len + 1, tokens.into_iter()))
            .map_err(|error_list| {
                print_error_report(
                    input,
                    error_list.into_iter().map(|err| err.map(|c| c.to_string())),
                )
            })?;
        Ok(ast.0)
    }

    #[test]
    fn two_explicit_transitions_with_comment() {
        let hoa = r#"State: 0 "a U b"   /* An example of named state */
        [0 & !1] 0 {0}
        [1] 1 {0}"#;
        let t0 = Edge::from_parts(
            Label(LabelExpression::And(
                Box::new(LabelExpression::Integer(0)),
                Box::new(LabelExpression::Not(Box::new(LabelExpression::Integer(1)))),
            )),
            vec![0],
            vec![0],
        );
        let t1 = Edge::from_parts(Label(LabelExpression::Integer(1)), vec![1], vec![0]);
        let q0 = State::from_parts(0, Some("a U b".to_string()), vec![t0, t1]);
        assert_eq!(process_body(&in_tags(hoa)), Ok(vec![q0]));
    }

    #[test]
    fn one_transition_label_true() {
        let hoa = r#"
            State: 1
            [t] 1 {1}
        "#;
        let t0 = Edge::from_parts(Label(LabelExpression::Boolean(true)), vec![1], vec![1]);
        let q0 = State::from_parts(1, None, vec![t0]);
        assert_eq!(process_body(&in_tags(hoa)), Ok(vec![q0]));
    }

    #[test]
    fn no_transition_state() {
        let hoa = r#"State: 1"#;
        let q0 = State::from_parts(1, None, vec![]);
        assert_eq!(process_body(&in_tags(hoa)), Ok(vec![q0]));
    }
}
