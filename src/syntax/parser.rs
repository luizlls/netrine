use crate::span::Span;

use super::error::{SyntaxError, Result, SyntaxErrorKind};
use super::node::*;
use super::token::{Token, TokenKind::{self, *}};

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
            token: tokens.get(0).copied().unwrap_or_default(),
            index: 0,
        }
    }

    fn reset(&mut self, index: usize) {
        self.index = index;
        self.token = self.nth(self.index);
        if self.token.is(NL) && (self.prev().non_terminal() || self.peek().non_terminal()) {
            self.index += 1;
            self.token = self.tokens[self.index];
        }
    }

    fn bump(&mut self) {
        self.reset(self.index + 1)
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

    fn attempt<F, T>(&mut self, mut parse: F) -> Result<Option<T>>
    where
        F: FnMut(&mut Parser) -> Result<T>,
    {
        let index = self.index;
        match parse(self) {
            Ok(value) => Ok(Some(value)),
            Err(error) => {
                if self.index != index {
                    self.reset(index);
                    Err(error)
                } else {
                    Ok(None)
                }
            },
        }
    }

    fn one_of<T, E>(&mut self, parsers: &[fn(&mut Parser) -> Result<T>], none: E) -> Result<T>
    where
        E: Fn(&Parser) -> SyntaxError,
    {
        let index = self.index;
        for parse in parsers {
            let value = parse(self);
            if value.is_ok() {
                return value;
            }
            self.reset(index);
        }

        Err(none(self))
    }

    fn error(&self, kind: SyntaxErrorKind) -> SyntaxError {
        SyntaxError::new(kind, self.span())
    }
}

pub fn parse(src: &str, tokens: &[Token]) -> Result<Vec<Node>> {
    let mut parser = Parser::new(src, tokens);
    let mut nodes = vec![];

    while parser.index < tokens.len() {
        maybe(&mut parser, NL);
        nodes.push(top_level(&mut parser)?);
        endline(&mut parser)?;
    }

    Ok(nodes)
}

fn top_level(p: &mut Parser) -> Result<Node> {
    p.one_of(&[
        // import,
        function,
        def,
        // typedef,
        expr,
    ], |p| p.error(SyntaxErrorKind::ExpectedItem))
}

fn function(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    let name = name(p)?;
    token(p, LParen)?;
    let parameters = comma(p, parameter)?;
    token(p, RParen)?;
    token(p, Equals)?;
    newline(p);
    let value = expr(p)?;

    let end = p.last();

    Ok(Node::function(name, parameters, value, start.to(end)))
}

fn parameter(p: &mut Parser) -> Result<Parameter> {
    let patt = patt(p)?;
    let span = patt.span;
    Ok(Parameter::new(patt, None, span))
}

fn def(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    let lvalue = lvalue(p)?;
    token(p, Equals)?;
    let rvalue = rvalue(p)?;

    let end = p.last();

    Ok(Node::def(lvalue, rvalue, start.to(end)))
}

fn expr(p: &mut Parser) -> Result<Node> {
    binary(p, 0 as Precedence)
}

fn atom(p: &mut Parser) -> Result<Node> {
    match p.token.kind {
        Ident => ident(p),
        Integer => integer(p),
        Number => number(p),
        String => string(p),
        LParen => parens(p, expr),
        LBrace => braces(p),
        LBracket => brackets(p, expr),
        _ => {
            match p.token.kind {
                UnexpectedCharacter => Err(p.error(SyntaxErrorKind::UnexpectedCharacter)),
                UnterminatedString  => Err(p.error(SyntaxErrorKind::UnterminatedString)),
                _ => Err(p.error(SyntaxErrorKind::ExpectedExpr)),
            }
        }
    }
}

fn patt(p: &mut Parser) -> Result<Node> {
    match p.token.kind {
        Integer => integer(p),
        Number => number(p),
        String => string(p),
        _ => structural_patt(p)
    }
}

fn structural_patt(p: &mut Parser) -> Result<Node> {
    match p.token.kind {
        Ident => instance(p, patt),
        LParen => parens(p, patt),
        // LBrace => record(p),
        LBracket => brackets(p, patt),
        _ => Err(p.error(SyntaxErrorKind::ExpectedPatt))
    }
}

fn lvalue(p: &mut Parser) -> Result<Node> {
    structural_patt(p)
}

fn rvalue(p: &mut Parser) -> Result<Node> {
    expr(p)
}

fn literal(p: &mut Parser, kind: TokenKind, ctor: fn(Literal) -> NodeKind) -> Result<Node> {
    let span = token(p, kind)?;
    let value = p.slice(span).to_string();
    Ok(Node::literal(ctor, value, span))
}

fn name(p: &mut Parser) -> Result<Name> {
    let span = token(p, Ident)?;
    let value = p.slice(span).to_string();
    Ok(Literal { value, span })
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

fn parens<F>(p: &mut Parser, parse: F) -> Result<Node>
where
    F: FnMut(&mut Parser) -> Result<Node>
{
    let start = p.span();

    token(p, LParen)?;
    let items = comma(p, parse)?;
    token(p, RParen)?;

    let end = p.last();

    let span = start.to(end);
    Ok(match_vec(
        items,
        |item| Node::group(item, span),
        |items| Node::tuple(items, span),
        || Node::tuple(vec![], span),
    ))
}

fn brackets<F>(p: &mut Parser, parse: F) -> Result<Node>
where
    F: FnMut(&mut Parser) -> Result<Node>
{
    let start = p.span();

    token(p, LBracket)?;
    let items = comma(p, parse)?;
    token(p, RBracket)?;

    let end = p.last();

    Ok(Node::list(items, start.to(end)))
}

fn braces(p: &mut Parser) -> Result<Node> {
    block(p)
}

fn block(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, LBrace)?;
    let nodes = many(p, top_level)?;
    token(p, RBrace)?;

    let end = p.last();

    Ok(Node::block(nodes, start.to(end)))
}

fn argument(p: &mut Parser) -> Result<Argument> {
    let value = expr(p)?;
    let span = value.span;
    Ok(Argument::new(value, None, span))
}

fn instance<F>(p: &mut Parser, parse: F) -> Result<Node>
where
    F: FnMut(&mut Parser) -> Result<Node>
{
    let name = ident(p)?;
    if p.at(LParen) {
        let start = p.span();
        token(p, LParen)?;

        let items = comma(p, parse)?
            .into_iter()
            .map(|patt @ Node { span, .. }| {
                Argument::new(patt, None, span)
            })
            .collect();

        token(p, RParen)?;
        let end = p.last();

        Ok(Node::apply(name, items, start.to(end)))
    } else {
        Ok(name)
    }
}

fn basic(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    let mut node = atom(p)?;
    loop {
        node = match p.token.kind {
            LParen => {
                token(p, LParen)?;
                let arguments = comma(p, argument)?;
                token(p, RParen)?;
                let end = p.last();
                Node::apply(node, arguments, start.to(end))
            }
            Dot => {
                token(p, Dot)?;
                let field = atom(p)?;
                let end = p.last();
                Node::get(node,field, start.to(end))
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
          | Associativity::Left  => operator.precedence() + 1,
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

fn newline(p: &mut Parser) {
    if p.at(NL) {
        p.bump();
    }
}

fn endline(p: &mut Parser) -> Result<()> {
    if p.at(NL) || p.at(Semi) || p.at(EOF) {
        p.bump();
        Ok(())
    } else {
        Err(unexpected(p, &[NL, Semi]))
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

fn unexpected(p: &mut Parser, expected: &[TokenKind]) -> SyntaxError {
    let tk = p.token;
    SyntaxError::unexpected(tk.kind, expected, tk.span)
}

fn optional<F, T>(p: &mut Parser, mut parse: F) -> Option<T>
where
    F: FnMut(&mut Parser) -> Result<T>
{
    parse(p).ok()
}

fn comma<F, T>(p: &mut Parser, mut parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>
{
    let mut res = vec![];

    newline(p);
    while let Some(value) = p.attempt(&mut parse)? {
        res.push(value);
        newline(p);
        if !maybe(p, Comma) {
            break;
        }
        newline(p);
    }

    Ok(res)
}

fn many<F, T>(p: &mut Parser, mut parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>
{
    let mut res = vec![];

    newline(p);
    while let Some(value) = p.attempt(&mut parse)? {
        res.push(value);
        endline(p)?;
    }

    Ok(res)
}
