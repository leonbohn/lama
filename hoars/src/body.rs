use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

pub use crate::label_expression::{DnfLabelExpression, LabelConjunct, LabelExpression};
use chumsky::prelude::*;

use crate::{
    lexer::Token, value, AcceptanceSignature, Aliases, AtomicProposition, Id, StateConjunction,
};

/// Newtype wrapper around a [`LabelExpression`], implements [`Deref`].
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Label(pub LabelExpression);

impl Deref for Label {
    type Target = LabelExpression;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Label {
    /// Turns the Label into a [`HoaSymbol`].
    pub fn symbol(&self) -> HoaSymbol {
        HoaSymbol::from(self.clone())
    }

    /// Performs an unaliasing of the label, using the given aliases in the
    /// form of a [`Aliases`] map.
    /// This should guarantee that the resulting [`LabelExpression`] does
    /// not contain any [`LabelExpression::Alias`]es.
    pub fn unalias(&self, aliases: &Aliases) -> LabelExpression {
        match &self.0 {
            LabelExpression::Alias(a) => aliases
                .iter()
                .find_map(
                    |(name, expr)| {
                        if name == a {
                            Some(expr.clone())
                        } else {
                            None
                        }
                    },
                )
                .unwrap_or_else(|| panic!("Invalid input, alias with name {} not defined", a)),
            otherwise => otherwise.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Alphabet(pub Vec<AtomicProposition>);

/// Used as a symbol in a parsed HOA automaton.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HoaSymbol(pub DnfLabelExpression);

impl PartialOrd for HoaSymbol {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl Ord for HoaSymbol {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl Display for HoaSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

impl From<Label> for HoaSymbol {
    fn from(label: Label) -> Self {
        Self(label.0.into())
    }
}

impl From<LabelExpression> for HoaSymbol {
    fn from(expr: LabelExpression) -> Self {
        Self(expr.into())
    }
}

impl From<DnfLabelExpression> for HoaSymbol {
    fn from(expr: DnfLabelExpression) -> Self {
        Self(expr)
    }
}

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

/// Represents an edge in a HOA automaton. It contains the [`LabelExpression`], the
/// [`StateConjunction`] and the [`AcceptanceSignature`] of the edge.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Edge(
    pub(crate) Label,
    pub(crate) StateConjunction,
    pub(crate) AcceptanceSignature,
);

impl Edge {
    /// Returns the label of the edge.
    pub fn label(&self) -> &Label {
        &self.0
    }

    /// Gives mutable access to the label of the edge.
    pub fn label_mut(&mut self) -> &mut Label {
        &mut self.0
    }

    /// Returns the state conjunction of the edge.
    pub fn state_conjunction(&self) -> &StateConjunction {
        &self.1
    }

    /// Returns the acceptance signature of the edge.
    pub fn acceptance_signature(&self) -> &AcceptanceSignature {
        &self.2
    }

    /// Tries to get the target (singular) of the transition. Returns `None` if the
    /// transition does not have a singular target.
    pub fn target(&self) -> Option<Id> {
        self.1.get_singleton()
    }

    /// Builds an edge from its parts.
    pub fn from_parts(
        label_expression: Label,
        state_conjunction: StateConjunction,
        acceptance_signature: AcceptanceSignature,
    ) -> Self {
        Self(label_expression, state_conjunction, acceptance_signature)
    }
}

/// Represents a state in a HOA automaton. It contains the [`Id`] of the state, an optional
/// comment and a list of outgoing edges.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State(
    pub(crate) Id,
    pub(crate) Option<String>,
    pub(crate) Vec<Edge>,
);

impl State {
    /// Constructs a new state from its parts.
    pub fn from_parts(id: Id, comment: Option<String>, edges: Vec<Edge>) -> Self {
        Self(id, comment, edges)
    }

    /// Extracts the id of the state.
    pub fn id(&self) -> Id {
        self.0
    }

    /// Extracts the comment of the state, if present.
    pub fn comment(&self) -> Option<&str> {
        self.1.as_deref()
    }

    /// Extracts the edges of the state.
    pub fn edges(&self) -> &[Edge] {
        &self.2
    }

    /// Gives mutable access to the edges of this state.
    pub fn edges_mut(&mut self) -> &mut [Edge] {
        &mut self.2
    }
}

impl From<(Option<AcceptanceSignature>, ExplicitEdge)> for Edge {
    fn from((state_acc, edge): (Option<AcceptanceSignature>, ExplicitEdge)) -> Self {
        let acc = match (state_acc, &edge.2) {
            (None, None) => AcceptanceSignature(Vec::new()),
            (Some(acc), None) => acc,
            (None, Some(acc)) => acc.clone(),
            (Some(left), Some(right)) => {
                AcceptanceSignature(left.iter().cloned().chain(right.iter().cloned()).collect())
            }
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

/// Represents the body of a HOA automaton. In essence, this is just a vector of [`State`]s.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Body(Vec<State>);

impl Body {
    /// Constructs a new empty body.
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn parser() -> impl Parser<Token, Self, Error = Simple<Token>> {
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

impl Deref for Body {
    type Target = Vec<State>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Body {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(test)]
mod tests {
    use chumsky::{primitive::end, Parser, Stream};

    use crate::{lexer, HoaBool, LabelExpression, StateConjunction};

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
            StateConjunction(vec![0]),
            crate::AcceptanceSignature(vec![0]),
        );
        let t1 = Edge::from_parts(
            Label(LabelExpression::Integer(1)),
            StateConjunction(vec![1]),
            crate::AcceptanceSignature(vec![0]),
        );
        let q0 = State::from_parts(0, Some("a U b".to_string()), vec![t0, t1]);
        assert_eq!(process_body(&in_tags(hoa)), Ok(vec![q0]));
    }

    #[test]
    fn one_transition_label_true() {
        let hoa = r#"
            State: 1
            [t] 1 {1}
        "#;
        let t0 = Edge::from_parts(
            Label(LabelExpression::Boolean(HoaBool(true))),
            StateConjunction(vec![1]),
            crate::AcceptanceSignature(vec![1]),
        );
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
