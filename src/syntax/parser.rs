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

    fn token(&self) -> TokenKind {
        self.nth(self.index).kind
    }

    fn peek(&self) -> TokenKind {
        self.nth(self.index + 1).kind
    }

    fn span(&self) -> Span {
        self.nth(self.index).span
    }

    fn last(&self) -> Span {
        self.nth(self.index - 1).span
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.token() == kind
    }

    fn slice(&self, span: Span) -> &str {
        &self.source[span.range()]
    }

    fn attempt<F, T>(&mut self, mut parse: F) -> Result<Option<T>>
    where
      F: FnMut(&mut Self) -> Result<T>
    {
        let index = self.index;

        match parse(self) {
            Ok(value) => Ok(Some(value)),
            Err(error) if self.index != index => Err(error),
            Err(_) => Ok(None), 
        }
    }

    fn lookahead<F, T>(&self, mut looker: F) -> T
    where
      F: FnMut(&Token) -> Option<T>
    {
        let mut idx = self.index + 1;
        loop {
            if let Some(value) = looker(&self.nth(idx)) {
                return value;
            }
            idx += 1;
        }
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
    match p.token() {
        TokenKind::Import => import(p),
        TokenKind::Yield  => r#yield(p),
        TokenKind::Break  => r#break(p),
        TokenKind::Return => r#return(p),
        TokenKind::If => r#if(p),
        TokenKind::For => r#for(p),
        _ => define(p)
    }
}

fn define(p: &mut Parser) -> Result<Node> {
    let lvalue = lvalue(p)?;

    if !maybe(p, TokenKind::Equals) {
        return Ok(lvalue);
    }

    let rvalue = rvalue(p)?;

    let constraints
        = maybe_parse(p, TokenKind::Where, |parser| seq(parser, expr))?;

    let span = Span::from(&lvalue, &p.last());

    Ok(node(
        NodeKind::Define(Define { lvalue, rvalue, constraints }),
        span,
    ))
}

fn expr(p: &mut Parser) -> Result<Node> {
    match p.token() {
        TokenKind::Yield => r#yield(p),
        TokenKind::If => r#if(p),
        TokenKind::For => r#for(p),
        _ => binary(p, 0 as Precedence)
    }
}

fn atom(p: &mut Parser) -> Result<Node> {
    match p.token() {
        TokenKind::Ident => ident(p),
        TokenKind::Number => number(p),
        TokenKind::String => string(p),
        TokenKind::LParen => parens(p),
        TokenKind::LBrace => braces(p),
        TokenKind::LBracket => brackets(p),
        TokenKind::Dot => field(p),
        TokenKind::Mut => mutable(p),
        _ => Err(p.fail("expected an expression".to_string(), p.span())),
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

    let span = Span::from(&start, &field);

    Ok(node(
        NodeKind::Field(Field { field }),
        span,
    ))
}

fn mutable(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::Mut)?;
    let value = expr(p)?;

    let span = Span::from(&start, &value);

    Ok(node(
        NodeKind::Mutable(Mutable { value }),
        span,
    ))
}

fn parens(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::LParen)?;
    let nodes = seq(p, expr)?;
    let end = expect(p, TokenKind::RParen)?;

    let span = Span::from(&start, &end);

    Ok(node(
        match_vec(
            nodes,
            |node| NodeKind::Group(Group { node }),
            |items| NodeKind::Tuple(Tuple { items }),
            || NodeKind::Empty,
        ),
        span,
    ))
}

fn brackets(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::LBracket)?;
    let items = seq(p, expr)?;
    let end = expect(p, TokenKind::RBracket)?;

    let span = Span::from(&start, &end);

    Ok(node(
        NodeKind::List(List { items }),
        span,
    ))
}

fn braces(p: &mut Parser) -> Result<Node> {
    if matches!(p.peek(), TokenKind::Dot | TokenKind::Dots) {
        record(p)
    } else if is_match(p) {
        r#match(p)
    } else if is_lambda(p) {
        lambda(p)
    } else {
        block(p)
    }
}

fn block(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::LBrace)?;
    let nodes = many(p, top_level)?;
    let end = expect(p, TokenKind::RBrace)?;

    let span = Span::from(&start, &end);

    Ok(node(
        NodeKind::Block(Block { nodes }),
        span,
    ))
}

fn record(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::LBrace)?;
    let fields = seq(p, record_field)?;
    let end = expect(p, TokenKind::RBrace)?;

    let span = Span::from(&start, &end);

    Ok(node(
        NodeKind::Record(Record { fields }),
        span,
    ))
}

fn record_field(p: &mut Parser) -> Result<RecordField> {
    match p.token() {
        TokenKind::Dots => {
            let start = expect(p, TokenKind::Dots)?;
            let value = expr(p)?;
            let span = Span::from(&start, &value);

            Ok(RecordField {
                kind: RecordFieldKind::Spread(value),
                span,
            })
        }

        TokenKind::Dot => {
            let start = expect(p, TokenKind::Dot)?;
            let name = name(p)?;
            let value = match p.token() {
                TokenKind::Equals => {
                    expect(p, TokenKind::Equals)?;
                    Some(expr(p)?)
                }
                TokenKind::LBrace => {
                    Some(record(p)?)
                }
                TokenKind::Comma
              | TokenKind::RBrace => {
                    None   
                }
                _ => return Err(unexpected(p, &[
                    TokenKind::Equals,
                    TokenKind::Comma,
                    TokenKind::RBrace,
                ]))
            };

            let span = Span::from(&start, &p.last());

            Ok(RecordField {
                kind: RecordFieldKind::Field(name, value),
                span,
            })
        }

        _ => Err(unexpected(p, &[
            TokenKind::Dot,
            TokenKind::Dots,
        ]))
    }
}

fn lambda(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::LBrace)?;

    let parameters = seq(p, atom)?;
    let constraints = maybe_parse(p, TokenKind::Where, |parser| seq(parser, expr))?;
    expect(p, TokenKind::Arrow)?;
    let value = many(p, top_level)?;

    let end = expect(p, TokenKind::RBrace)?;

    let span = Span::from(&start, &end);

    Ok(node(
        NodeKind::Lambda(Lambda { parameters, value, constraints }),
        span,
    ))
}

fn is_lambda(p: &Parser) -> bool {
    let mut nest_level = 1;

    p.lookahead(move |&token| {
        match token.kind {
            TokenKind::Arrow if nest_level == 1 => Some(true),
            TokenKind::RBrace if nest_level == 1 => Some(false),
            TokenKind::LBrace => { nest_level += 1; None },
            TokenKind::RBrace => { nest_level -= 1; None },
            _ => if nest_level > 0 {
                None
            } else {
                Some(false)
            }
        }
    })
}

fn r#match(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::LBrace)?;

    let mut match_cases = vec![];

    while maybe(p, TokenKind::Case) {
        let mut cases = vec![];
        loop {
            cases.push(atom(p)?);
            if !maybe(p, TokenKind::Or) {
                break;
            }
        }
        let constraints = maybe_parse(p, TokenKind::Where, |parser| seq(parser, expr))?;
        expect(p, TokenKind::Arrow)?;
        let value = many(p, top_level)?;

        match_cases.push(Case { cases, value, constraints });
    }

    let otherwise = maybe_parse(p, TokenKind::Else, |parser| many(parser, top_level))?;

    let end = expect(p, TokenKind::RBrace)?;

    let span = Span::from(&start, &end);

    Ok(node(
        NodeKind::Match(Match { cases: match_cases, otherwise }),
        span,
    ))
}

fn is_match(p: &Parser) -> bool {
    p.token().is(TokenKind::LBrace) && p.peek().is(TokenKind::Case)
}

fn access(p: &mut Parser, expr: Node) -> Result<Node> {
    let _span = expect(p, TokenKind::Dot)?;
    let field = atom(p)?;

    let span = Span::from(&expr, &field);

    Ok(node(
        NodeKind::Access(Access { node: expr, field }),
        span,
    ))
}

fn apply(p: &mut Parser, callee: Node) -> Result<Node> {
    expect(p, TokenKind::LParen)?;
    let arguments = seq(p, expr)?;
    expect(p, TokenKind::RParen)?;
    
    let span = Span::from(&callee, &p.last());

    Ok(node(
        NodeKind::Apply(Apply { callee, arguments }),
        span,
    ))
}

fn basic(p: &mut Parser) -> Result<Node> {
    let mut value = atom(p)?;

    loop {
        value = match p.token() {
            TokenKind::Dot    => access(p, value)?,
            TokenKind::LParen => apply(p, value)?,
            _ => break,
        }
    }

    Ok(value)
}

fn unary(p: &mut Parser) -> Result<Node> {
    if let Some(operator) = operator(p, true) && operator.is_unary() {
        p.bump();

        let expr = basic(p)?;
        let span = Span::from(&operator, &expr);

        return Ok(node(
            NodeKind::Unary(Unary { operator, expr }),
            span,
        ))
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
    let rvalue = rvalue(p)?;
    let span = Span::from(&lvalue, &rvalue);

    Ok(node(
        NodeKind::Assign(Assign { operator, lvalue, rvalue }),
        span,
    ))
}

fn r#if(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::If)?;
    let value = lvalue(p)?;
    let then = rvalue(p)?;
    let otherwise = maybe_parse(p, TokenKind::Else, rvalue)?;

    let span = Span::from(&start, &p.last());

    Ok(node(
        NodeKind::If(If { value, then, otherwise }),
        span,
    ))
}

fn r#for(p: &mut Parser) -> Result<Node> {
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

    let span = Span::from(&start, &value);

    Ok(node(
        NodeKind::For(For { bindings, value }),
        span,
    ))
}

fn r#yield(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::Yield)?;
    let value = expr(p)?;

    let span = Span::from(&start, &value);

    Ok(node(
        NodeKind::Yield(Yield { value }),
        span,
    ))
}

fn r#break(p: &mut Parser) -> Result<Node> {
    let span = expect(p, TokenKind::Break)?;
    
    Ok(node(
        NodeKind::Break,
        span,
    ))
}

fn r#return(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::Return)?;
    let value = expr(p)?;

    let span = Span::from(&start, &value);

    Ok(node(
        NodeKind::Return(Return { value }),
        span,
    ))
}

fn import(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::Import)?;

    let module = name(p)?;

    let qualified = if p.at(TokenKind::LParen) {
        expect(p, TokenKind::LParen)?;
        let names = seq(p, name)?;
        expect(p, TokenKind::RParen)?;
        names
    } else {
        vec![]
    };

    let span = Span::from(&start, &p.last());

    Ok(node(
        NodeKind::Import(Import { module, qualified }),
        span,
    ))
}

fn node(kind: NodeKind, span: Span) -> Node {
    Node {
        kind: Box::new(kind),
        span,
    }
}

fn seq<T, F>(p: &mut Parser, mut parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    let mut result = vec![];

    while let Some(value) = p.attempt(&mut parse)? {
        result.push(value);

        if !maybe(p, TokenKind::Comma) {
            break;
        }
    }

    Ok(result)
}

fn many<T, F>(p: &mut Parser, mut parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    let mut result = vec![];

    while let Some(value) = p.attempt(&mut parse)? {
        result.push(value);
    }

    Ok(result)
}

fn maybe_parse<T, F>(
    p: &mut Parser,
    expected: TokenKind,
    mut parse: F,
) -> Result<Option<T>>
where
  F: FnMut(&mut Parser) -> Result<T>,
{
    if maybe(p, expected) {
        Ok(Some(parse(p)?))
    } else {
        Ok(None)
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

fn match_vec<T, U, O, M, E>(mut value: Vec<T>, one: O, many: M, empty: E) -> U
where
    O: Fn(T) -> U,
    M: Fn(Vec<T>) -> U,
    E: Fn() -> U,
{
    match value.len() {
        0 => empty(),
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
        Err(p.fail(format!("Expected `{expected}`"), p.span()))
    }
}

fn unexpected(p: &mut Parser, expected: &[TokenKind]) -> Error {
    let formatted = expected
        .iter()
        .map(|it| {
            format!("`{it}`")
        })
        .collect::<Vec<_>>()
        .join(" or ");

    let span = p.span();
    
    p.bump();
    p.fail(format!("expected {formatted}"), span)
}
