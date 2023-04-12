use chumsky::{prelude::*, select};

use crate::{
    format::LabelExpression, AcceptanceAtom, AcceptanceCondition, AcceptanceInfo,
    AcceptanceSignature, Id, StateConjunction, Token,
};

#[allow(unused)]
pub fn header() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! {
        Token::Header(hdr) => hdr,
    }
}

pub fn boolean() -> impl Parser<Token, bool, Error = Simple<Token>> + Clone {
    select! {
        Token::Identifier(id) if id == *"t" => true,
        Token::Identifier(id) if id == *"f" => false,
    }
}

pub fn integer() -> impl Parser<Token, Id, Error = Simple<Token>> + Clone {
    select! {
        Token::Int(n) => n.parse().unwrap(),
    }
}

pub fn text() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! {
        Token::Text(txt) => txt,
    }
}

pub fn identifier() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! { Token::Identifier(ident) => ident }
}

pub fn alias_name() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! { Token::Alias(aname) => aname }
}

pub fn state_conjunction() -> impl Parser<Token, StateConjunction, Error = Simple<Token>> {
    integer().separated_by(just(Token::Op('&'))).at_least(1)
}

pub fn acceptance_signature() -> impl Parser<Token, AcceptanceSignature, Error = Simple<Token>> {
    integer()
        .repeated()
        .delimited_by(just(Token::Paren('{')), just(Token::Paren('}')))
}

pub fn acceptance_info() -> impl Parser<Token, AcceptanceInfo, Error = Simple<Token>> {
    select! {
        Token::Identifier(ident) => AcceptanceInfo::Identifier(ident),
        Token::Int(n) => AcceptanceInfo::Int(n.parse().unwrap())
    }
}

pub fn label_expression() -> impl Parser<Token, LabelExpression, Error = Simple<Token>> {
    recursive(|label_expression| {
        let value = boolean()
            .map(LabelExpression::Boolean)
            .or(integer().map(LabelExpression::Integer))
            .or(alias_name().map(LabelExpression::Alias));

        let atom = value
            .or(label_expression.delimited_by(just(Token::Paren('(')), just(Token::Paren(')'))));

        let unary = just(Token::Op('!'))
            .or_not()
            .then(atom)
            .map(|(negated, expr)| {
                if negated.is_some() {
                    LabelExpression::Not(Box::new(expr))
                } else {
                    expr
                }
            });

        let conjunction = unary
            .clone()
            .then(
                just(Token::Op('&'))
                    .to(LabelExpression::And)
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (f, rhs)| f(Box::new(lhs), Box::new(rhs)));

        conjunction
            .clone()
            .then(
                just(Token::Op('|'))
                    .to(LabelExpression::Or)
                    .then(conjunction)
                    .repeated(),
            )
            .foldl(|lhs, (f, rhs)| f(Box::new(lhs), Box::new(rhs)))
    })
}

pub fn acceptance_condition() -> impl Parser<Token, AcceptanceCondition, Error = Simple<Token>> {
    recursive(|acceptance_condition| {
        let atom_value = just(Token::Op('!'))
            .or_not()
            .then(integer())
            .map(|(negated, integer)| {
                if negated.is_none() {
                    AcceptanceAtom::Positive(integer)
                } else {
                    AcceptanceAtom::Negative(integer)
                }
            });

        let fin_inf_atom = just(Token::Fin)
            .to(AcceptanceCondition::Fin as fn(_) -> _)
            .or(just(Token::Inf).to(AcceptanceCondition::Inf as fn(_) -> _))
            .then(atom_value.delimited_by(just(Token::Paren('(')), just(Token::Paren(')'))))
            .map(|(f, atom)| f(atom));

        let atom =
            boolean()
                .map(AcceptanceCondition::Boolean)
                .or(fin_inf_atom)
                .or(acceptance_condition
                    .delimited_by(just(Token::Paren('(')), just(Token::Paren(')'))));

        let conjunction = atom
            .clone()
            .then(
                just(Token::Op('&'))
                    .to(AcceptanceCondition::And)
                    .then(atom)
                    .repeated(),
            )
            .foldl(|lhs, (f, rhs)| f(Box::new(lhs), Box::new(rhs)));

        conjunction
            .clone()
            .then(
                just(Token::Op('|'))
                    .to(AcceptanceCondition::Or)
                    .then(conjunction)
                    .repeated(),
            )
            .foldl(|lhs, (f, rhs)| f(Box::new(lhs), Box::new(rhs)))
    })
}
