use crate::span::Span;
use crate::error::{Error, ErrorKind, Result};

use super::node::*;
use super::token::{
    Token,
    TokenKind::{self, *},
};

#[derive(Debug, Clone)]
pub struct Parser<'p> {
    source: &'p str,
    tokens: &'p [Token],
    token: Token,
    index: usize,
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p str, tokens: &'p [Token]) -> Parser<'p> {
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
        if self.token.is(NewLine) && (self.prev().non_terminal() || self.peek().non_terminal()) {
            self.index += 1;
            self.token = self.tokens[self.index];
        }
    }

    fn nth(&self, idx: usize) -> Token {
        if idx < self.tokens.len() {
            self.tokens[idx]
        } else {
            Token::default()
        }
    }

    fn prev(&self) -> Token {
        self.nth(self.index.saturating_sub(1))
    }

    fn peek(&self) -> Token {
        self.nth(self.index + 1)
    }

    fn span(&self) -> Span {
        self.token.span
    }

    fn end(&self) -> Span {
        self.prev().span
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    fn slice(&self, span: Span) -> &str {
        &self.source[span.range()]
    }
}

pub fn parse(source: &str, tokens: &[Token]) -> Result<Vec<Node>> {
    let mut parser = Parser::new(source, tokens);
    let nodes = many(&mut parser, EOF, top_level)?;
    Ok(nodes)
}

fn top_level(p: &mut Parser) -> Result<Node> {
    let lvalue = lvalue(p)?;
    token(p, Equals)?;
    let rvalue = rvalue(p)?;

    Ok(Node::def(lvalue, rvalue))
}

fn expr(p: &mut Parser) -> Result<Node> {
    binary(p, 0 as Precedence)
}

fn lvalue(p: &mut Parser) -> Result<Node> {
    expr(p)
}

fn rvalue(p: &mut Parser) -> Result<Node> {
    expr(p)
}

fn atom(p: &mut Parser) -> Result<Node> {
    let Token { span, kind } = p.token;
    match kind {
        Ident => ident(p),
        String => string(p),
        Number => number(p),
        Integer => integer(p),
        LParen => parens(p),
        LBrace => braces(p),
        LBracket => brackets(p),
        _ => {
            Err(Error::new(match kind {
                UnexpectedCharacter => ErrorKind::UnexpectedCharacter,
                UnterminatedString  => ErrorKind::UnterminatedString,
                _ => ErrorKind::ExpectedExpression,
            },
            span))
        }
    }
}

fn literal(p: &mut Parser, kind: TokenKind, ctor: fn(Literal) -> Node) -> Result<Node> {
    let span = token(p, kind)?;
    let value = p.slice(span).to_string();

    Ok(Node::literal(ctor, value, span))
}

fn ident(p: &mut Parser) -> Result<Node> {
    literal(p, Ident, Node::Name)
}

fn number(p: &mut Parser) -> Result<Node> {
    literal(p, Number, Node::Number)
}

fn integer(p: &mut Parser) -> Result<Node> {
    literal(p, Integer, Node::Integer)
}

fn string(p: &mut Parser) -> Result<Node> {
    literal(p, String, Node::String)
}

fn parens(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, LParen)?;
    let items = comma(p, RParen, expr)?;
    token(p, RParen)?;

    let span = start.to(p.end());

    if items.len() == 1 {
        Ok(Node::group(items, span))
    } else {
        Ok(Node::tuple(items, span))
    }
}

fn brackets(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, LBracket)?;
    let items = comma(p, RBracket, expr)?;
    token(p, RBracket)?;

    let end = p.end();

    Ok(Node::array(items, start.to(end)))
}

fn braces(p: &mut Parser) -> Result<Node> {
    block(p)
}

fn block(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, LBrace)?;
    let nodes = many(p, RBrace, top_level)?;
    token(p, RBrace)?;

    let end = p.end();

    Ok(Node::block(nodes, start.to(end)))
}

fn basic(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    let mut node = atom(p)?;
    loop {
        node = match p.token.kind {
            LParen => {
                token(p, LParen)?;
                let arguments = comma(p, RParen, expr)?;
                token(p, RParen)?;

                let end = p.end();

                Node::apply(node, arguments, start.to(end))
            }
            Dot => {
                token(p, Dot)?;
                let field = atom(p)?;

                Node::get(node, field)
            }
            _ => break,
        }
    }

    Ok(node)
}

fn unary(p: &mut Parser) -> Result<Node> {
    if let Some(operator) = operator(p, true) && operator.is_unary() {
        p.bump(); // operator
        let expr = unary(p)?;

        Ok(Node::unary(operator, expr))
    } else {
        basic(p)
    }
}

fn binary(p: &mut Parser, precedence: Precedence) -> Result<Node> {
    let mut expr = unary(p)?;

    while let Some(operator) = operator(p, false) && operator.precedence() >= precedence {
        p.bump(); // operator

        let precedence = match operator.associativity() {
            Associativity::None
          | Associativity::Left => operator.precedence() + 1,
            Associativity::Right => operator.precedence(),
        };

        let rexpr = binary(p, precedence)?;
        let lexpr = expr;

        expr = Node::binary(operator, lexpr, rexpr)
    }

    Ok(expr)
}

fn operator(p: &mut Parser, unary: bool) -> Option<Operator> {
    let span = p.span();

    let kind = match p.token.kind {
        Plus  if unary => OperatorKind::Pos,
        Minus if unary => OperatorKind::Neg,
        Not   if unary => OperatorKind::Not,
        Plus  => OperatorKind::Add,
        Minus => OperatorKind::Sub,
        Star  => OperatorKind::Mul,
        Slash => OperatorKind::Div,
        Caret => OperatorKind::Exp,
        Mod   => OperatorKind::Mod,
        And   => OperatorKind::And,
        Or    => OperatorKind::Or,
        Is    => OperatorKind::Is,
        EqEq  => OperatorKind::Eq,
        NoEq  => OperatorKind::Ne,
        Lt    => OperatorKind::Lt,
        LtEq  => OperatorKind::Le,
        Gt    => OperatorKind::Gt,
        GtEq  => OperatorKind::Ge,
        Dots  => OperatorKind::Range,
        _ => return None,
    };

    Some(Operator::new(kind, span))
}

fn newline(p: &mut Parser) {
    if p.at(NewLine) {
        p.bump();
    }
}

fn endline(p: &mut Parser) -> Result<()> {
    if p.at(NewLine) || p.at(Semi) || p.at(EOF) {
        p.bump();
        Ok(())
    } else {
        Err(unexpected(p, &[NewLine, Semi]))
    }
}

fn token(p: &mut Parser, kind: TokenKind) -> Result<Span> {
    let span = p.span();
    if p.at(kind) {
        p.bump();
        Ok(span)
    } else {
        Err(unexpected(p, &[kind]))
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

fn unexpected(p: &Parser, expected: &[TokenKind]) -> Error {
    let message = expected
        .iter()
        .map(|it| {
            format!("`{it}`")
        })
        .collect::<Vec<_>>()
        .join(" or ");

    Error::raw(message, p.span())
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
        if !maybe(p, Comma) {
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
