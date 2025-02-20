use crate::syntax::{
    Binary, Unary, Group, Literal, Node, Operator, Precedence, Associativity,
};
use crate::error::{Error, Result};
use crate::lexer::Lexer;
use crate::source::{Source, Span};
use crate::token::Token;

#[derive(Debug)]
pub struct Parser<'p> {
    source: &'p Source,
    lexer: Lexer<'p>,
    token: (Token, Span),
    prev: (Token, Span),
    peek: (Token, Span),
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p Source) -> Parser<'p> {
        let lexer = Lexer::new(&source.content);
        Parser {
            source,
            lexer,
            token: Default::default(),
            prev:  Default::default(),
            peek:  Default::default(),
        }
        .init()
    }

    fn init(mut self) -> Parser<'p> {
        self.bump();
        self.bump();
        self
    }

    fn bump(&mut self) {
        self.prev = self.token;
        self.token = self.peek;
        self.peek = self.lexer.next().unwrap_or_default();
    }

    fn kind(&self) -> Token {
        self.token.0
    }

    fn span(&self) -> Span {
        self.token.1
    }

    fn done(&self, start: Span) -> Span {
        Span::of(start, self.prev.1)
    }

    fn at(&self, kind: Token) -> bool {
        self.token.0 == kind
    }

    fn slice(&self, span: Span) -> &str {
        &self.source.slice(span)
    }

    fn fail<T>(&mut self, error: std::string::String) -> Result<T> {
        let span = self.span();
        self.recover();
        Err(Error::new(error, span))
    }

    fn recover(&mut self) {
        while !self.at(Token::EOF) { self.bump(); }
    }
}

impl Iterator for Parser<'_> {
    type Item = Result<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.at(Token::EOF) {
            None
        } else {
            Some(expr(self))
        }
    }
}

fn expr(p: &mut Parser) -> Result<Node> {
    binary(p, 0 as Precedence)
}

fn atom(p: &mut Parser) -> Result<Node> {
    match p.kind() {
        Token::Number => number(p),
        Token::Integer => integer(p),
        Token::LParen => parens(p),
        _ => p.fail(
            match p.kind() {
                Token::UnexpectedCharacter => "unexpected character",
                Token::UnterminatedString => "unterminated string",
                _ => "unexpected expression",
            }
            .to_string(),
        ),
    }
}

fn literal(p: &mut Parser, kind: Token, ctor: fn(Literal) -> Node) -> Result<Node> {
    let span = token(p, kind)?;

    Ok(ctor(Literal {
        value: p.slice(span).to_string(),
        span,
    }))
}

fn number(p: &mut Parser) -> Result<Node> {
    literal(p, Token::Number, Node::Number)
}

fn integer(p: &mut Parser) -> Result<Node> {
    literal(p, Token::Integer, Node::Integer)
}

fn parens(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, Token::LParen)?;
    let inner = expr(p)?;
    token(p, Token::RParen)?;

    let span = p.done(start);

    Ok(Node::Group(
        Group {
            inner, span
        }.into()
    ))
}

fn unary(p: &mut Parser) -> Result<Node> {
    if let Some((operator, span)) = operator(p, true) && operator.is_unary() {
        p.bump(); // operator
        let expr = unary(p)?;

        let span = Span::of(span, expr.span());

        Ok(Node::Unary(
            Unary {
                operator, expr, span,
            }
            .into(),
        ))
    } else {
        atom(p)
    }
}

fn binary(p: &mut Parser, precedence: Precedence) -> Result<Node> {
    let mut expr = unary(p)?;

    while let Some((operator, _)) = operator(p, false) && operator.precedence() >= precedence
    {
        p.bump(); // operator

        let precedence = match operator.associativity() {
            Associativity::None
          | Associativity::Left => operator.precedence() + 1,
            Associativity::Right => operator.precedence(),
        };

        let rexpr = binary(p, precedence)?;
        let lexpr = expr;

        let span = Span::of(lexpr.span(), rexpr.span());

        expr = Node::Binary(
            Binary {
                operator, lexpr, rexpr, span,
            }
            .into(),
        );
    }

    Ok(expr)
}

fn operator(p: &mut Parser, unary: bool) -> Option<(Operator, Span)> {
    let operator = if unary {
        match p.kind() {
            Token::Plus  => Operator::Pos,
            Token::Minus => Operator::Neg,
            Token::Not   => Operator::Not,
            _ => return None
        }
    } else {
        match p.kind() {
           Token::Plus  => Operator::Add,
           Token::Minus => Operator::Sub,
           Token::Star  => Operator::Mul,
           Token::Slash => Operator::Div,
           Token::Caret => Operator::Exp,
           Token::Mod   => Operator::Mod,
           Token::And   => Operator::And,
           Token::Or    => Operator::Or,
           Token::EqEq  => Operator::Eq,
           Token::NoEq  => Operator::Ne,
           Token::Lt    => Operator::Lt,
           Token::LtEq  => Operator::Le,
           Token::Gt    => Operator::Gt,
           Token::GtEq  => Operator::Ge,
           Token::Dots  => Operator::Range,
           _ => return None,
       }
    };

    Some((operator, p.span()))
}

fn newline(p: &mut Parser) {
    maybe(p, Token::NewLine);
}

fn endline(p: &mut Parser) -> Result<()> {
    if p.at(Token::NewLine) || p.at(Token::EOF) {
        p.bump();
        Ok(())
    } else {
        p.fail(unexpected(&[Token::NewLine]))
    }
}

fn token(p: &mut Parser, kind: Token) -> Result<Span> {
    let span = p.span();
    if p.at(kind) {
        p.bump();
        Ok(span)
    } else {
        p.fail(unexpected(&[kind]))
    }
}

fn maybe(p: &mut Parser, kind: Token) -> bool {
    if p.at(kind) {
        p.bump();
        true
    } else {
        false
    }
}

fn comma<F, T>(p: &mut Parser, until: Token, mut parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    let mut res = vec![];

    newline(p);
    while !p.at(until) {
        res.push(parse(p)?);
        newline(p);
        if !maybe(p, Token::Comma) {
            break;
        }
        newline(p);
    }

    Ok(res)
}

fn many<F, T>(p: &mut Parser, until: Token, mut parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    let mut res = vec![];

    newline(p);
    while !p.at(until) {
        res.push(parse(p)?);
        endline(p)?;
    }

    Ok(res)
}

fn unexpected(expected: &[Token]) -> std::string::String {
    let expected = expected.iter().map(|it| format!("`{it}`")).collect::<Vec<_>>().join(", ");
    format!("expected {expected}")
}
