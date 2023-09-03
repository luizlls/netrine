use crate::error::{Error, Result};
use crate::span::Span;

use super::node::*;
use super::token::{Token, TokenKind};

use std::ops::Fn;

#[derive(Debug, Clone, Copy)]
struct Marker(usize);


#[derive(Debug, Clone)]
pub struct Parser<'p> {
    source: &'p str,
    tokens: &'p [Token],
    index: usize,
    markers: Vec<Marker>,
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p str, tokens: &'p [Token]) -> Parser<'p> {
        Parser {
            source,
            tokens,
            index: 0,
            markers: vec![]
        }
    }

    fn bump(&mut self) {
        self.index += 1
    }

    fn nth(&self, idx: usize) -> Token {
        if idx < self.tokens.len() {
            self.tokens[idx]
        } else {
            Token::default()
        }
    }

    fn token(&self) -> TokenKind {
        self.nth(self.index).kind
    }

    fn peek(&self) -> TokenKind {
        self.nth(self.index + 1).kind
    }

    fn span(&self) -> Span {
        self.nth(self.index).span
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.token() == kind
    }

    fn slice(&self, span: Span) -> &str {
        &self.source[span.range()]
    }

    fn fail(&mut self, message: String, span: Span) -> Error {
        Error::new(message, span)
    }

    fn start(&mut self) -> Marker {
        let marker = Marker(self.index);
        self.markers.push(marker);
        marker
    }

    fn finish(&mut self) -> Span {
        let marker = self.markers
            .pop()
            .expect("ICE [Parser]: Called `finish` before `start`");

        self.finish_with(marker)
    }

    fn finish_with(&self, Marker(start): Marker) -> Span {
        let end = self.index;
        Span::from(&self.nth(start), &self.nth(end))
    }
}

pub fn parse(p: &mut Parser) -> Result<Vec<Node>> {
    let mut nodes = vec![];

    while !p.at(TokenKind::EOF) {
        nodes.push(parse_item(p)?);
    }

    Ok(nodes)
}

fn parse_item(p: &mut Parser) -> Result<Node> {
    match p.token() {
        TokenKind::If => parse_if(p),
        TokenKind::Match => parse_match(p),
        TokenKind::Yield => parse_yield(p),
        TokenKind::Return => parse_return(p),
        _ => parse_expr(p)
    }
}

fn parse_expr(p: &mut Parser) -> Result<Node> {
    match p.token() {
        TokenKind::If => parse_if(p),
        TokenKind::Match => parse_match(p),
        TokenKind::Mut => parse_mut(p),
        _ => parse_binary(p, 0 as Precedence),
    }
}

fn parse_atom(p: &mut Parser) -> Result<Node> {
    match p.token() {
        TokenKind::Ident => parse_ident(p),
        TokenKind::Number => parse_number(p),
        TokenKind::String => parse_string(p),
        TokenKind::LParen => parse_parens(p),
        TokenKind::LBrace => parse_braces(p),
        TokenKind::LBracket => parse_brackets(p),
        _ => return Err(p.fail("expected an expression".to_string(), p.span())),
    }
}

fn is_atom(p: &Parser) -> bool {
    matches!(p.token(),
             TokenKind::Ident | TokenKind::Number | TokenKind::String
           | TokenKind::LParen | TokenKind::LBrace | TokenKind::LBracket)
}

fn parse_def(p: &mut Parser) -> Result<Node> {
    p.start();

    let lvalue = parse_expr(p)?;

    let kind = match p.token() {
        TokenKind::Equals => {
            expect(p, TokenKind::Equals)?;
            LetKind::Equals
        }
        TokenKind::Arrow => {
            expect(p, TokenKind::Arrow)?;
            LetKind::Arrow
        }
        _ => return Ok(lvalue)
    };
    
    let rvalue = parse_expr(p)?;

    let constraints = parse_constraints(p)?;

    Ok(node(
        NodeKind::Let(Let { kind, lvalue, rvalue, constraints }),
        p.finish(),
    ))
}

fn parse_literal(p: &mut Parser, kind: TokenKind) -> Result<Literal> {
    let span = expect(p, kind)?;
    let value = p.slice(span).to_string();
    Ok(Literal { value, span })
}

fn parse_name(p: &mut Parser) -> Result<Name> {
    parse_literal(p, TokenKind::Ident)
}

fn parse_ident(p: &mut Parser) -> Result<Node> {
    let value = parse_literal(p, TokenKind::Ident)?;
    let span = value.span;
    Ok(node(NodeKind::Name(value), span))
}

fn parse_number(p: &mut Parser) -> Result<Node> {
    let value = parse_literal(p, TokenKind::Number)?;
    let span = value.span;
    Ok(node(NodeKind::Number(value), span))
}

fn parse_string(p: &mut Parser) -> Result<Node> {
    let value = parse_literal(p, TokenKind::String)?;
    let span = value.span;
    Ok(node(NodeKind::String(value), span))
}

fn parse_parens(p: &mut Parser) -> Result<Node> {
    let (nodes, span) = parse_sequence(
        p,
        TokenKind::LParen,
        TokenKind::RParen,
        TokenKind::Comma.into(),
        parse_expr,
    )?;

    Ok(node(
        match_vec(
            nodes,
            |node| NodeKind::Group(Group { node }),
            |items| NodeKind::Tuple(Tuple { items }),
        ),
        span,
    ))
}

fn parse_braces(p: &mut Parser) -> Result<Node> {
    todo!()
}

fn parse_brackets(p: &mut Parser) -> Result<Node> {
    let (items, span) = parse_sequence(
        p,
        TokenKind::LBracket,
        TokenKind::RBracket,
        TokenKind::Comma.into(),
        parse_expr,
    )?;

    Ok(node(
        NodeKind::List(List { items }),
        span,
    ))
}

fn parse_initial(p: &mut Parser) -> Result<Node> {
    let mut expr = parse_atom(p)?;

    loop {
        expr = match p.token() {
            TokenKind::Dot => {
                expect(p, TokenKind::Dot)?;
                let field = parse_atom(p)?;
                let span = Span::from(&expr, &field);
                node(
                    NodeKind::Access(Access { node: expr, field }),
                    span,
                )
            }

            TokenKind::LParen => {
                let (arguments, span) = parse_sequence(
                    p,
                    TokenKind::LParen,
                    TokenKind::RParen,
                    TokenKind::Comma.into(),
                    parse_expr,
                )?;
                let span = Span::from(&expr, &span);

                node(
                    NodeKind::Apply(Apply { callee: expr, arguments }),
                    span,
                )
            }

            _ => break,
        }
    }

    Ok(expr)
}

fn parse_unary(p: &mut Parser) -> Result<Node> {
    if let Some(operator) = operator(p, true) && operator.is_unary() {
        p.start();
        p.bump();
        let expr = parse_expr(p)?;

        Ok(node(
            NodeKind::Unary(Unary { operator, expr }),
            p.finish(),
        ))
    } else {
        parse_initial(p)
    }
}

fn parse_binary(p: &mut Parser, precedence: Precedence) -> Result<Node> {
    let mut expr = parse_unary(p)?;

    while let Some(operator) = operator(p, false) && !operator.is_unary() && operator.precedence() >= precedence {
        p.bump();

        let precedence = match operator.associativity() {
            Associativity::None
          | Associativity::Left  => operator.precedence() + 1,
            Associativity::Right => operator.precedence(),
        };

        let rexpr = parse_binary(p, precedence)?;
        let lexpr = expr;
        let span = Span::from(&lexpr, &rexpr);

        expr = node(NodeKind::Binary(Binary { operator, lexpr, rexpr }), span);
    }

    Ok(expr)
}

fn operator(p: &mut Parser, unary: bool) -> Option<Operator> {
    let span = p.span();

    let kind = match p.token() {
        TokenKind::Plus  if unary => OperatorKind::Pos,
        TokenKind::Minus if unary => OperatorKind::Neg,
        TokenKind::Not   if unary => OperatorKind::Not,
        TokenKind::Plus  => OperatorKind::Add,
        TokenKind::Minus => OperatorKind::Sub,
        TokenKind::Star  => OperatorKind::Mul,
        TokenKind::Slash => OperatorKind::Div,
        TokenKind::Mod   => OperatorKind::Mod,
        TokenKind::Caret => OperatorKind::Exp,
        TokenKind::And   => OperatorKind::And,
        TokenKind::Or    => OperatorKind::Or,
        TokenKind::Is    => OperatorKind::Is,
        TokenKind::EqEq  => OperatorKind::Eq,
        TokenKind::NoEq  => OperatorKind::Ne,
        TokenKind::Lt    => OperatorKind::Lt,
        TokenKind::LtEq  => OperatorKind::Le,
        TokenKind::Gt    => OperatorKind::Gt,
        TokenKind::GtEq  => OperatorKind::Ge,
        TokenKind::Dots  => OperatorKind::Range,
        _ => return None,
    };

    Some(Operator { kind, span })
}

fn parse_constraints(p: &mut Parser) -> Result<Vec<Constraint>> {
    let constraints = if maybe(p, TokenKind::Where) {
        if p.at(TokenKind::LBrace) {
            parse_sequence(
                p,
                TokenKind::LBrace,
                TokenKind::RBrace,
                None,
                parse_constraint,
            )?.0
        } else {
            vec![
                parse_constraint(p)?,
            ]
        }
    } else {
        vec![]
    };

    Ok(constraints)
}

fn parse_constraint(p: &mut Parser) -> Result<Constraint> {
    let Node { kind, span } = parse_expr(p)?;
    let kind = match *kind {
        NodeKind::Apply(constraint) => ConstraintKind::Apply(constraint),
        NodeKind::Unary(constraint) => ConstraintKind::Unary(constraint),
        NodeKind::Binary(constraint) => ConstraintKind::Binary(constraint),
        _ => return Err(p.fail("invalid constraint expression".to_string(), span)),
    };
    Ok(Constraint { kind, span })
}

fn parse_if(p: &mut Parser) -> Result<Node> {
    p.start();
    expect(p, TokenKind::If)?;

    let value = parse_expr(p)?;
    let then = parse_expr(p)?;
    let otherwise = maybe_parse(p, TokenKind::Else, parse_expr)?;

    Ok(node(
        NodeKind::If(If { value, then, otherwise }),
        p.finish(),
    ))
}

fn parse_match(p: &mut Parser) -> Result<Node> {
    p.start();
    expect(p, TokenKind::Match)?;

    let value = parse_expr(p)?;

    let braces = maybe(p, TokenKind::LBrace);

    let mut cases = vec![];

    while maybe(p, TokenKind::Case) {
        let case = parse_atom(p)?;
        let guard = maybe_parse(p, TokenKind::If, parse_expr)?;
        expect(p, TokenKind::Arrow)?;
        let then = parse_expr(p)?;
        cases.push(MatchCase { case, then, guard })
    }

    let otherwise = maybe_parse(p, TokenKind::Else, parse_expr)?;

    if braces {
        expect(p, TokenKind::RBrace)?;
    }

    Ok(node(
        NodeKind::Match(Match { value, cases, otherwise }),
        p.finish(),
    ))
}

fn parse_mut(p: &mut Parser) -> Result<Node> {
    p.start();
    expect(p, TokenKind::Mut)?;

    let name = parse_name(p)?;
    let value = maybe_parse(p, TokenKind::Equals, parse_expr)?;

    Ok(node(
        NodeKind::Mut(Mut { name, value }),
        p.finish(),
    ))
}

fn parse_yield(p: &mut Parser) -> Result<Node> {
    p.start();
    expect(p, TokenKind::Yield)?;

    Ok(node(
        NodeKind::Yield(Yield { value: parse_expr(p)? }),
        p.finish(),
    ))
}

fn parse_return(p: &mut Parser) -> Result<Node> {
    p.start();
    expect(p, TokenKind::Return)?;

    Ok(node(
        NodeKind::Return(Return { value: parse_expr(p)? }),
        p.finish(),
    ))
}

fn parse_sequence<T, F>(
    p: &mut Parser,
    open: TokenKind,
    close: TokenKind,
    separator: Option<TokenKind>,
    mut parse: F,
) -> Result<(Vec<T>, Span)>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    let mut result = vec![];

    let start = expect(p, open)?;

    while !p.at(close) {
        result.push(parse(p)?);
        if let Some(separator) = separator && !maybe(p, separator) {
            break;
        }
    }

    let end = expect(p, close)?;

    Ok((result, Span::from(&start, &end)))
}

fn maybe_parse<T, F>(
    p: &mut Parser,
    expected: TokenKind,
    mut parse: F,
) -> Result<Option<T>>
where
  F: FnMut(&mut Parser) -> Result<T>,
{
    if p.token() == expected {
        p.bump();
        Ok(Some(parse(p)?))
    } else {
        Ok(None)
    }
}

fn match_vec<T, U, O, M>(mut value: Vec<T>, one: O, many: M) -> U
where
  O: Fn(T) -> U, M: Fn(Vec<T>) -> U,
{
    match value.len() {
        1 => one(value.pop().unwrap()),
        _ => many(value),
    }
}

fn expect(p: &mut Parser, expected: TokenKind) -> Result<Span> {
    if p.token() == expected {
        let span = p.span();
        p.bump();
        Ok(span)
    } else {
        Err(p.fail(format!("Expected `{}`", expected), p.span()))
    }
}

fn maybe(p: &mut Parser, expected: TokenKind) -> bool {
    if p.token() == expected {
        p.bump();
        true
    } else {
        false
    }
}

fn node(kind: NodeKind, span: Span) -> Node {
    Node {
        kind: Box::new(kind),
        span,
    }
}
