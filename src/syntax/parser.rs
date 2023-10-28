use crate::error::{Error, Result};
use crate::span::Span;

use super::node::*;
use super::token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Parser<'p> {
    source: &'p str,
    tokens: &'p [Token],
    index: usize,
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p str, tokens: &'p [Token]) -> Parser<'p> {
        Parser {
            source,
            tokens,
            index: 0,
        }
    }

    fn bump(&mut self) {
        self.index += 1;
    }

    fn nth(&self, idx: usize) -> Token {
        if idx < self.tokens.len() {
            self.tokens[idx]
        } else {
            Token::default()
        }
    }

    fn token(&self) -> Token {
        self.nth(self.index)
    }

    fn peek(&self) -> Token {
        self.nth(self.index + 1)
    }

    fn start(&self) -> Span {
        self.token().span
    }

    fn span(&self, start: Span) -> Span {
        let end = self.nth(self.index - 1).span;
        start.to(end)
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.token().kind == kind
    }

    fn slice(&self, span: Span) -> &str {
        &self.source[span.range()]
    }

    fn fail(&self, message: String, span: Span) -> Error {
        Error::new(message, span)
    }
}

pub fn parse(p: &mut Parser) -> Result<Vec<Node>> {
    let mut nodes = vec![];

    while !p.at(TokenKind::EOF) {
        nodes.push(top_level(p)?);
    }

    Ok(nodes)
}

fn top_level(p: &mut Parser) -> Result<Node> {
    match p.token().kind {
        TokenKind::Import => import(p),
        TokenKind::Yield  => yield_expr(p),
        TokenKind::Break  => break_stmt(p),
        TokenKind::Return => return_stmt(p),
        TokenKind::If  => if_expr(p),
        TokenKind::For => for_expr(p),
        _ => default(p),
    }
}

fn default(p: &mut Parser) -> Result<Node> {
    let lvalue = expr(p)?;

    match p.token().kind {
        TokenKind::Equals => define(p, lvalue),
        TokenKind::Arrow => lambda(p, lvalue),
        TokenKind::Colon => typedef(p, lvalue),
        _ => Ok(lvalue),
    }
}

fn define(p: &mut Parser, pattern: Node) -> Result<Node> {
    let start = pattern.span;
    expect(p, TokenKind::Equals)?;
    let lvalue = pattern;
    let rvalue = expr(p)?;
    let constraints = when(p, TokenKind::Where, |p| seq1(p, expr))?;

    Ok(node(
        NodeKind::Define(Define {
            lvalue,
            rvalue,
            constraints,
        }),
        p.span(start),
    ))
}

fn lambda(p: &mut Parser, pattern: Node) -> Result<Node> {
    let start = pattern.span;
    expect(p, TokenKind::Arrow)?;
    let parameters = vec![pattern];
    let value = expr(p)?;
    let constraints = when(p, TokenKind::Where, |p| seq1(p, expr))?;

    Ok(node(
        NodeKind::Lambda(Lambda {
            parameters,
            value,
            constraints,
        }),
        p.span(start),
    ))
}

fn typedef(p: &mut Parser, _pattern: Node) -> Result<Node> {
    todo!()
}

fn expr(p: &mut Parser) -> Result<Node> {
    match p.token().kind {
        TokenKind::Yield => yield_expr(p),
        TokenKind::If  => if_expr(p),
        TokenKind::For => for_expr(p),
        _ => binary(p, 0 as Precedence),
    }
}

fn atom(p: &mut Parser) -> Result<Node> {
    let Token { kind, span } = p.token();
    match kind {
        TokenKind::Ident => ident(p),
        TokenKind::Number => number(p),
        TokenKind::String => string(p),
        TokenKind::OpenParen => parens(p),
        TokenKind::OpenBrace => braces(p),
        TokenKind::OpenBracket => brackets(p),
        TokenKind::Dot => field(p),
        TokenKind::Mut => mutable(p),
        _ => Err(p.fail("expected an expression".to_string(), span)),
    }
}

fn lvalue(p: &mut Parser) -> Result<Node> {
    binary(p, 0 as Precedence)
}

fn rvalue(p: &mut Parser) -> Result<Node> {
    expr(p)
}

fn literal(p: &mut Parser, kind: TokenKind) -> Result<Literal> {
    let span = expect(p, kind)?;
    let value = p.slice(span).to_string();
    Ok(Literal { value, span })
}

fn name(p: &mut Parser) -> Result<Name> {
    literal(p, TokenKind::Ident)
}

fn ident(p: &mut Parser) -> Result<Node> {
    let value = literal(p, TokenKind::Ident)?;
    let span = value.span;
    Ok(node(NodeKind::Name(value), span))
}

fn number(p: &mut Parser) -> Result<Node> {
    let value = literal(p, TokenKind::Number)?;
    let span = value.span;
    Ok(node(NodeKind::Number(value), span))
}

fn string(p: &mut Parser) -> Result<Node> {
    let value = literal(p, TokenKind::String)?;
    let span = value.span;
    Ok(node(NodeKind::String(value), span))
}

fn field(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::Dot)?;
    let field = atom(p)?;

    Ok(node(NodeKind::Field(Field { field }), p.span(start)))
}

fn mutable(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::Mut)?;
    let value = expr(p)?;

    Ok(node(NodeKind::Mutable(Mutable { value }), p.span(start)))
}

fn parens(p: &mut Parser) -> Result<Node> {
    let start = p.start();

    expect(p, TokenKind::OpenParen)?;
    let nodes = seq(p, expr)?;
    expect(p, TokenKind::CloseParen)?;

    Ok(node(
        match_vec(
            nodes,
            |node| NodeKind::Group(Group { node }),
            |items| NodeKind::Tuple(Tuple { items }),
            || NodeKind::Empty,
        ),
        p.span(start),
    ))
}

fn brackets(p: &mut Parser) -> Result<Node> {
    let start = p.start();
    expect(p, TokenKind::OpenBracket)?;
    let items = seq(p, expr)?;
    expect(p, TokenKind::CloseBracket)?;

    Ok(node(NodeKind::List(List { items }), p.span(start)))
}

fn braces(p: &mut Parser) -> Result<Node> {
    match p.peek().kind {
        TokenKind::Case => cases(p),
        TokenKind::Dot
      | TokenKind::Dots => record(p),
        _ => block(p)
    }
}

fn record(p: &mut Parser) -> Result<Node> {
    let start = p.start();
    expect(p, TokenKind::OpenBrace)?;
    let fields = seq(p, record_field)?;
    expect(p, TokenKind::CloseBrace)?;

    Ok(node(NodeKind::Record(Record { fields }), p.span(start)))
}

fn record_field(p: &mut Parser) -> Result<RecordField> {
    match p.token().kind {
        TokenKind::Dots => {
            let start = expect(p, TokenKind::Dots)?;
            let value = expr(p)?;

            Ok(RecordField {
                kind: RecordFieldKind::Spread(value),
                span: p.span(start),
            })
        }

        TokenKind::Dot => {
            let start = expect(p, TokenKind::Dot)?;
            let name = name(p)?;
            let value = match p.token().kind {
                TokenKind::Equals => {
                    expect(p, TokenKind::Equals)?;
                    Some(expr(p)?)
                }
                TokenKind::OpenBrace => {
                    Some(record(p)?)
                }
                TokenKind::Comma
              | TokenKind::CloseBrace => {
                    None
                }
                _ => return Err(unexpected(p, &[
                    TokenKind::Equals,
                    TokenKind::Comma,
                    TokenKind::CloseBrace,
                ]))
            };

            Ok(RecordField {
                kind: RecordFieldKind::Field(name, value),
                span: p.span(start),
            })
        }

        _ => Err(unexpected(p, &[
            TokenKind::Dot,
            TokenKind::Dots,
        ]))
    }
}

fn cases(p: &mut Parser) -> Result<Node> {
    let start = p.start();

    expect(p, TokenKind::OpenBrace)?;

    let mut cases = vec![];

    while maybe(p, TokenKind::Case) {
        let mut patterns = vec![];
        loop {
            patterns.push(atom(p)?);
            if !maybe(p, TokenKind::Or) {
                break;
            }
        }

        let guards = when(p, TokenKind::Where, |p| seq1(p, expr))?;
        expect(p, TokenKind::Arrow)?;
        let value = many(p, top_level)?;

        cases.push(Case {
            patterns,
            value,
            guards,
        });
    }

    let otherwise = when(p, TokenKind::Else, |p| many(p, top_level))?;

    expect(p, TokenKind::CloseBrace)?;

    Ok(node(
        NodeKind::Cases(Cases {
            cases,
            otherwise,
        }),
        p.span(start),
    ))
}

fn block(p: &mut Parser) -> Result<Node> {
    let start = p.start();

    expect(p, TokenKind::OpenBrace)?;

    let nodes = many(p, top_level)?;

    if matches!(p.token().kind, TokenKind::Arrow | TokenKind::Where) {
        let parameters = nodes;
        let constraints = when(p, TokenKind::Where, |p| seq1(p, expr))?;

        let block = expect(p, TokenKind::Arrow)?;
        let nodes = many(p, top_level)?;
        let value = node(NodeKind::Block(Block { nodes }), p.span(block));

        expect(p, TokenKind::CloseBrace)?;

        return Ok(node(
            NodeKind::Lambda(Lambda {
                parameters,
                value,
                constraints,
            }),
            p.span(start),
        ));
    }

    expect(p, TokenKind::CloseBrace)?;

    Ok(node(NodeKind::Block(Block { nodes }), p.span(start)))
}

fn access(p: &mut Parser, expr: Node) -> Result<Node> {
    let start = expr.span;
    expect(p, TokenKind::Dot)?;
    let field = atom(p)?;

    Ok(node(NodeKind::Access(Access { node: expr, field }), p.span(start)))
}

fn apply(p: &mut Parser, callee: Node) -> Result<Node> {
    let start = callee.span;
    expect(p, TokenKind::OpenParen)?;
    let arguments = seq(p, expr)?;
    expect(p, TokenKind::CloseParen)?;

    Ok(node(NodeKind::Apply(Apply { callee, arguments }), p.span(start)))
}

fn accept(p: &mut Parser, callee: Node) -> Result<Node> {
    let start = callee.span;
    let lambda = braces(p)?;

    Ok(node(
        NodeKind::Accept(Accept {
            callee,
            lambda,
        }),
        p.span(start),
    ))
}

fn basic(p: &mut Parser) -> Result<Node> {
    let mut value = atom(p)?;

    loop {
        value = match p.token().kind {
            TokenKind::OpenParen => apply(p, value)?,
            TokenKind::OpenBrace => accept(p, value)?,
            TokenKind::Dot => access(p, value)?,
            _ => break,
        }
    }

    Ok(value)
}

fn unary(p: &mut Parser) -> Result<Node> {
    let start = p.start();

    if let Some(operator) = operator(p, true) && operator.is_unary() {
        p.bump();

        let expr = basic(p)?;

        return Ok(node(
            NodeKind::Unary(Unary {
                expr,
                operator,
            }),
            p.span(start),
        ));
    }

    basic(p)
}

fn binary(p: &mut Parser, precedence: Precedence) -> Result<Node> {
    let mut expr = unary(p)?;

    while let Some(operator) = operator(p, false) && !operator.is_unary() && operator.precedence() >= precedence {
        p.bump();

        if operator.mode == OperatorMode::Assign {
            p.bump();
            return assign(p, expr, operator);
        }

        let precedence = match operator.associativity() {
            Associativity::None
          | Associativity::Left  => operator.precedence() + 1,
            Associativity::Right => operator.precedence(),
        };

        let rexpr = binary(p, precedence)?;
        let lexpr = expr;
        let span = lexpr.span.to(rexpr.span);

        expr = node(
            NodeKind::Binary(Binary {
                lexpr,
                rexpr,
                operator,
            }),
            span,
        );
    }

    Ok(expr)
}

fn operator(p: &mut Parser, unary: bool) -> Option<Operator> {
    let span = p.start();

    let kind = match p.token().kind {
        TokenKind::Plus  if unary => OperatorKind::Pos,
        TokenKind::Minus if unary => OperatorKind::Neg,
        TokenKind::Not   if unary => OperatorKind::Not,
        TokenKind::Plus  => OperatorKind::Add,
        TokenKind::Minus => OperatorKind::Sub,
        TokenKind::Star  => OperatorKind::Mul,
        TokenKind::Slash => OperatorKind::Div,
        TokenKind::Caret => OperatorKind::Exp,
        TokenKind::Mod   => OperatorKind::Mod,
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

    let mode = if !unary && p.peek().is(TokenKind::Equals) {
        OperatorMode::Assign
    } else {
        OperatorMode::Regular
    };

    Some(Operator { kind, mode, span })
}

fn assign(p: &mut Parser, lvalue: Node, operator: Operator) -> Result<Node> {
    let start = lvalue.span;
    let rvalue = rvalue(p)?;

    Ok(node(
        NodeKind::Assign(Assign {
            operator,
            lvalue,
            rvalue,
        }),
        p.span(start),
    ))
}

fn if_expr(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::If)?;
    let value = lvalue(p)?;
    let then = rvalue(p)?;
    let otherwise = when(p, TokenKind::Else, rvalue)?;

    Ok(node(
        NodeKind::If(If {
            value,
            then,
            otherwise,
        }),
        p.span(start),
    ))
}

fn for_expr(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::For)?;

    let mut bindings = vec![];

    loop {
        let pattern = atom(p)?;
        expect(p, TokenKind::In)?;
        let sequence = expr(p)?;

        bindings.push((pattern, sequence));

        if !maybe(p, TokenKind::Comma) {
            break;
        }
    }

    let value = expr(p)?;

    Ok(node(NodeKind::For(For { bindings, value }), p.span(start)))
}

fn yield_expr(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::Yield)?;
    let value = expr(p)?;

    Ok(node(NodeKind::Yield(Yield { value }), p.span(start)))
}

fn break_stmt(p: &mut Parser) -> Result<Node> {
    let span = expect(p, TokenKind::Break)?;

    Ok(node(NodeKind::Break, span))
}

fn return_stmt(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::Return)?;
    let value = expr(p)?;

    Ok(node(NodeKind::Return(Return { value }), p.span(start)))
}

fn import(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::Import)?;

    let module = name(p)?;

    let names = if p.at(TokenKind::OpenParen) {
        expect(p, TokenKind::OpenParen)?;
        let names = seq(p, name)?;
        expect(p, TokenKind::CloseParen)?;
        names
    } else {
        vec![]
    };

    Ok(node(
        NodeKind::Import(Import {
            module,
            names,
            // alias,
        }),
        p.span(start),
    ))
}

fn node(kind: NodeKind, span: Span) -> Node {
    Node {
        kind: Box::new(kind),
        span,
    }
}

fn when<T, F>(p: &mut Parser, when: TokenKind, mut parse: F) -> Result<Option<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    if maybe(p, when) {
        Ok(Some(parse(p)?))
    } else {
        Ok(None)
    }
}

fn maybe(p: &mut Parser, expected: TokenKind) -> bool {
    if p.token().kind == expected {
        p.bump();
        true
    } else {
        false
    }
}

fn match_vec<T, U, O, M, E>(mut vec: Vec<T>, one: O, many: M, empty: E) -> U
where
    O: FnOnce(T) -> U,
    M: FnOnce(Vec<T>) -> U,
    E: FnOnce() -> U,
{
    match vec.len() {
        0 => empty(),
        1 => one(vec.pop().unwrap()),
        _ => many(vec),
    }
}

fn expect(p: &mut Parser, expected: TokenKind) -> Result<Span> {
    let Token { kind, span } = p.token();
    if kind == expected {
        p.bump();
        Ok(span)
    } else {
        Err(p.fail(format!("Expected `{expected}`"), span))
    }
}

fn unexpected(p: &mut Parser, expected: &[TokenKind]) -> Error {
    let formatted = expected
        .iter()
        .map(|it| format!("`{it}`"))
        .collect::<Vec<_>>()
        .join(" or ");
    let span = p.token().span;
    p.bump();
    p.fail(format!("expected {formatted}"), span)
}

fn sequential<T, F, P>(
    p: &mut Parser,
    mut parse: F,
    allow_empty: bool,
    mut stop: P,
) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
    P: FnMut(&mut Parser) -> bool,
{
    fn attempt<F, T>(p: &mut Parser, mut parse: F) -> Result<Option<T>>
    where
        F: FnMut(&mut Parser) -> Result<T>,
    {
        let index = p.index;
        match parse(p) {
            Ok(value) => Ok(Some(value)),
            Err(error) if p.index != index => Err(error),
            Err(_) => Ok(None),
        }
    }

    let mut result = vec![];

    if !allow_empty {
        result.push(parse(p)?);
        if stop(p) {
            return Ok(result);
        }
    }

    while let Some(value) = attempt(p, &mut parse)? {
        result.push(value);
        if stop(p) {
            return Ok(result);
        }
    }

    Ok(result)
}

fn seq<T, F>(p: &mut Parser, parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    sequential(p, parse, true, |p| !maybe(p, TokenKind::Comma))
}

fn seq1<T, F>(p: &mut Parser, parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    sequential(p, parse, false, |p| !maybe(p, TokenKind::Comma))
}

fn many<T, F>(p: &mut Parser, parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    sequential(p, parse, true, |_| false)
}
