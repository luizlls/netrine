use crate::syntax::{
    Node, Unary, Binary, Group, Operator, OperatorKind, Precedence, Associativity, Literal,
};
use crate::error::{Error, Result};
use crate::source::{Source, Span};
use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub struct Parser<'p> {
    source: &'p Source,
    tokens: &'p [Token],
    token: Token,
    index: usize,
}

impl<'p> Parser<'p> {
    fn new(source: &'p Source, tokens: &'p [Token]) -> Parser<'p> {
        Parser {
            source,
            tokens,
            token: tokens.first().copied().unwrap_or_default(),
            index: 0,
        }
    }

    fn bump(&mut self) {
        self.index += 1;
        self.token = self.nth(self.index);
    }

    fn nth(&self, index: usize) -> Token {
        self.tokens.get(index).copied().unwrap_or_default()
    }

    fn span(&self, start: Span) -> Span {
        let prev = self.nth(self.index.saturating_sub(1));
        Span::of(&start, &prev)
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    fn slice(&self, span: Span) -> &str {
        &self.source.slice(span)
    }

    fn fail<T>(&mut self, error: std::string::String) -> Result<T> {
        let span = self.token.span;
        self.recover();
        Err(Error::new(error, span))
    }

    fn recover(&mut self) {
        while !self.at(TokenKind::EOF) { self.bump(); }
    }
}

pub fn parse(source: &Source, tokens: &[Token]) -> Result<Vec<Node>> {
    let mut parser = Parser::new(source, tokens);
    let mut nodes = vec![];

    while !parser.at(TokenKind::EOF) {
        nodes.push(expr(&mut parser)?);
    }

    Ok(nodes)
}

fn expr(p: &mut Parser) -> Result<Node> {
    binary(p, 0 as Precedence)
}

fn atom(p: &mut Parser) -> Result<Node> {
    match p.token.kind {
        TokenKind::Number => number(p),
        TokenKind::Integer => integer(p),
        TokenKind::LParen => parens(p),
        _ => p.fail(
            match p.token.kind {
                TokenKind::UnexpectedCharacter => "unexpected character",
                TokenKind::UnterminatedString => "unterminated string",
                _ => "unexpected expression",
            }
            .to_string(),
        ),
    }
}

fn literal(p: &mut Parser, kind: TokenKind, ctor: fn(Literal) -> Node) -> Result<Node> {
    let span = token(p, kind)?;
    let value = p.slice(span).to_string();

    Ok(ctor(Literal { value, span }.into()))
}

fn number(p: &mut Parser) -> Result<Node> {
    literal(p, TokenKind::Number, Node::Number)
}

fn integer(p: &mut Parser) -> Result<Node> {
    literal(p, TokenKind::Integer, Node::Integer)
}

fn parens(p: &mut Parser) -> Result<Node> {
    let start = p.token.span;

    token(p, TokenKind::LParen)?;
    let inner = expr(p)?;
    token(p, TokenKind::RParen)?;

    let span = p.span(start);

    Ok(Node::Group(Group { inner, span }.into()))
}

fn unary(p: &mut Parser) -> Result<Node> {

    if let Some(operator) = operator(p, -1 as Precedence, true) {
        let expr = atom(p)?;
        let span = Span::of(&operator, &expr);

        Ok(Node::Unary(
            Unary {
                operator,
                expr,
                span,
            }.into()
        ))
    } else {
        atom(p)
    }
}

fn binary(p: &mut Parser, precedence: Precedence) -> Result<Node> {
    let mut expr = unary(p)?;

    while let Some(operator) = operator(p, precedence, false) {
        let precedence = match operator.associativity() {
            Associativity::None
          | Associativity::Left => operator.precedence() + 1,
            Associativity::Right => operator.precedence(),
        };

        let rexpr = binary(p, precedence)?;
        let lexpr = expr;
        let span = Span::of(&lexpr, &rexpr);

        expr = Node::Binary(
            Binary {
                operator,
                lexpr,
                rexpr,
                span,
            }.into()
        );
    }

    Ok(expr)
}

fn operator(p: &mut Parser, precedence: Precedence, unary: bool) -> Option<Operator> {
    let span = p.token.span;
    let kind = if unary {
        match p.token.kind {
            TokenKind::Plus  => OperatorKind::Pos,
            TokenKind::Minus => OperatorKind::Neg,
            TokenKind::Not   => OperatorKind::Not,
            _ => return None
        }
    } else {
        match p.token.kind {
           TokenKind::Plus  => OperatorKind::Add,
           TokenKind::Minus => OperatorKind::Sub,
           TokenKind::Star  => OperatorKind::Mul,
           TokenKind::Slash => OperatorKind::Div,
           TokenKind::Caret => OperatorKind::Exp,
           TokenKind::Mod   => OperatorKind::Mod,
           TokenKind::And   => OperatorKind::And,
           TokenKind::Or    => OperatorKind::Or,
           TokenKind::EqEq  => OperatorKind::Eq,
           TokenKind::NoEq  => OperatorKind::Ne,
           TokenKind::Lt    => OperatorKind::Lt,
           TokenKind::LtEq  => OperatorKind::Le,
           TokenKind::Gt    => OperatorKind::Gt,
           TokenKind::GtEq  => OperatorKind::Ge,
           TokenKind::Dots  => OperatorKind::Range,
           _ => return None,
       }
    };

    let operator = Operator::new(kind, span);

    if operator.precedence() >= precedence {
        p.bump();
        Some(operator)
    } else {
        None
    }
}

fn newline(p: &mut Parser) {
    maybe(p, TokenKind::NewLine);
}

fn endline(p: &mut Parser) -> Result<()> {
    if p.at(TokenKind::NewLine) || p.at(TokenKind::EOF) {
        p.bump();
        Ok(())
    } else {
        p.fail(unexpected(&[TokenKind::NewLine]))
    }
}

fn token(p: &mut Parser, kind: TokenKind) -> Result<Span> {
    let span = p.token.span;
    if p.at(kind) {
        p.bump();
        Ok(span)
    } else {
        p.fail(unexpected(&[kind]))
    }
}

fn maybe(p: &mut Parser, kind: TokenKind) -> bool {
    if p.at(kind) {
        p.bump();
        true
    } else {
        false
    }
}

fn comma<F, T>(p: &mut Parser, until: TokenKind, mut parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    let mut res = vec![];

    newline(p);
    while !p.at(until) {
        res.push(parse(p)?);
        newline(p);
        if !maybe(p, TokenKind::Comma) {
            break;
        }
        newline(p);
    }

    Ok(res)
}

fn many<F, T>(p: &mut Parser, until: TokenKind, mut parse: F) -> Result<Vec<T>>
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

fn unexpected(expected: &[TokenKind]) -> std::string::String {
    let expected = expected.iter().map(|it| format!("`{it}`")).collect::<Vec<_>>().join(", ");
    format!("expected {expected}")
}
