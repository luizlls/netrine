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

    fn last(&self) -> Span {
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
    let value = lvalue(p)?;
    match p.token.kind {
        Equals => define(p, value),
        RBrace => closure(p, value),
        _ => Ok(value)
    }
}

fn define(p: &mut Parser, value: Node) -> Result<Node> {
    if is_function(&value) {
        return function(p, value);
    }

    let start = value.span;

    let patt = Patt::try_from(value)?.structural()?;
    token(p, Equals)?;
    let value = rvalue(p)?;

    let end = value.span;

    Ok(Node::def(patt, value, start.to(end)))
}

pub fn is_function(value: &Node) -> bool {
    if let NodeKind::Apply(Apply { callee, .. }) = &*value.kind && let NodeKind::Name(_) = *callee.kind {
        true
    } else {
        false
    }
}

fn function(p: &mut Parser, value: Node) -> Result<Node> {
    if let NodeKind::Apply(apply) = *value.kind && let NodeKind::Name(name) = *apply.callee.kind {
        let start = name.span;

        let parameters = apply.arguments.into_iter().map(Parameter::try_from).collect::<Result<Vec<_>, _>>()?;
        token(p, Equals)?;
        let value = rvalue(p)?;

        let end = value.span;

        Ok(Node::function(name, parameters, value, start.to(end)))
    } else {
        unreachable!()
    }
}

fn closure(_p: &mut Parser, _value: Node) -> Result<Node> {
    todo!()
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
    match p.token.kind {
        Ident => ident(p),
        Integer => integer(p),
        Number => number(p),
        String => string(p),
        LParen => parens(p),
        LBrace => braces(p),
        LBracket => brackets(p),
        _ => {
            let error = match p.token.kind {
                UnexpectedCharacter => ErrorKind::UnexpectedCharacter,
                UnterminatedString => ErrorKind::UnterminatedString,
                _ => ErrorKind::ExpectedExpression,
            };
            Err(Error::new(error, p.token.span))
        }
    }
}

fn literal(p: &mut Parser, kind: TokenKind, ctor: fn(Literal) -> NodeKind) -> Result<Node> {
    let span = token(p, kind)?;
    let value = p.slice(span).to_string();
    Ok(Node::literal(ctor, value, span))
}

fn ident(p: &mut Parser) -> Result<Node> {
    literal(p, Ident, NodeKind::Name)
}

fn number(p: &mut Parser) -> Result<Node> {
    literal(p, Number, NodeKind::Number)
}

fn integer(p: &mut Parser) -> Result<Node> {
    literal(p, Integer, NodeKind::Integer)
}

fn string(p: &mut Parser) -> Result<Node> {
    literal(p, String, NodeKind::String)
}

fn parens(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, LParen)?;
    let mut items = comma(p, RParen, expr)?;
    token(p, RParen)?;

    let span = start.to(p.last());

    if items.len() == 1 {
        Ok(items.pop().unwrap())
    } else {
        Ok(Node::tuple(items, span))
    }
}

fn brackets(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, LBracket)?;
    let items = comma(p, RBracket, expr)?;
    token(p, RBracket)?;

    let end = p.last();

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

    let end = p.last();

    Ok(Node::block(nodes, start.to(end)))
}

fn argument(p: &mut Parser) -> Result<Argument> {
    let value = expr(p)?;
    let span = value.span;
    Ok(Argument::new(value, None, span))
}

fn basic(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    let mut node = atom(p)?;
    loop {
        node = match p.token.kind {
            LParen => {
                token(p, LParen)?;
                let arguments = comma(p, RParen, argument)?;
                token(p, RParen)?;
                let end = p.last();
                Node::apply(node, arguments, start.to(end))
            }
            Dot => {
                token(p, Dot)?;
                let field = atom(p)?;
                let end = p.last();
                Node::get(node, field, start.to(end))
            }
            _ => break,
        }
    }

    Ok(node)
}

fn unary(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    if let Some(operator) = operator(p, true) && operator.is_unary() {
        p.bump(); // operator
        let expr = basic(p)?;
        let end = p.last();
        Ok(Node::unary(operator, expr, start.to(end)))
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
        let span = lexpr.span.to(rexpr.span);

        expr = Node::binary(operator, lexpr, rexpr, span)
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
