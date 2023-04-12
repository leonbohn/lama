use chumsky::prelude::*;

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Bool(bool),
    Int(String),
    Text(String),
    Identifier(String),
    Alias(String),
    Header(String),
    Op(char),
    Paren(char),
    BodyStart,
    BodyEnd,
    Abort,
    Fin,
    Inf,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Bool(b) => write!(f, "{}", b),
            Token::Int(n) => write!(f, "{}", n),
            Token::Text(txt) => write!(f, "{}", txt),
            Token::Identifier(id) => write!(f, "{}", id),
            Token::Alias(alias) => write!(f, "@{}", alias),
            Token::Header(hdr) => write!(f, "{}:", hdr),
            Token::Op(o) => write!(f, "{}", o),
            Token::Paren(c) => write!(f, "{}", c),
            Token::Fin => write!(f, "Fin"),
            Token::Inf => write!(f, "Inf"),
            Token::BodyEnd => write!(f, "--END--"),
            Token::BodyStart => write!(f, "--BODY--"),
            Token::Abort => write!(f, "--ABORT--"),
        }
    }
}

pub fn tokenizer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let int = text::int(10).map(Token::Int);

    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Text);

    let op = one_of("!|&").map(Token::Op);

    let paren = one_of(r#"(){}[]"#).map(Token::Paren);

    let raw_ident = filter(|c: &char| c.is_ascii_alphabetic() || *c == '_')
        .chain(filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_' || *c == '-').repeated())
        .collect::<String>();

    let ident = raw_ident.map(|ident: String| match ident.as_str() {
        "Fin" => Token::Fin,
        "Inf" => Token::Inf,
        _ => Token::Identifier(ident),
    });

    let alias = just('@').ignore_then(raw_ident).map(Token::Alias);

    let header = ident
        .then_ignore(just(':'))
        .map(|header_name| Token::Header(header_name.to_string()));

    let body = just("--BODY--").to(Token::BodyStart);
    let end = just("--END--").to(Token::BodyEnd);
    let abort = just("--ABORT--").to(Token::BodyEnd);

    let token = int
        .or(abort)
        .or(end)
        .or(body)
        .or(header)
        .or(str_)
        .or(op)
        .or(paren)
        .or(alias)
        .or(ident);

    let comment = just("/*").then(take_until(just("*/"))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}
