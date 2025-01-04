use crate::error::Diagnostics;
use crate::lexer;
use crate::node::{self, Module, Node, NodeKind, Literal, Operator, OperatorKind, Precedence, Associativity};
use crate::source::{Source, Span};
use crate::token::{Token, TokenKind, TokenKind::*};

type ParserResult<T> = Result<T, ()>;

#[derive(Debug)]
pub struct Parser<'p> {
    source: &'p Source,
    tokens: Vec<Token>,
    token: Token,
    index: usize,
    diagnostics: Diagnostics,
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p Source, tokens: Vec<Token>) -> Parser<'p> {
        Parser {
            source,
            tokens,
            token: Token::default(),
            index: 0,
            diagnostics: Diagnostics::new(source.source_id),
        }
        .init()
    }

    fn init(mut self) -> Parser<'p> {
        self.token = self.tokens.first().copied().unwrap_or_default();
        self
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

    fn kind(&self) -> TokenKind {
        self.token.kind
    }

    fn span(&self) -> Span {
        self.token.span
    }

    fn done(&self, start: Span) -> Span {
        Span::of(start, self.prev().span)
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    fn slice(&self, span: Span) -> &str {
        &self.source.slice(span)
    }

    fn fail<T>(&mut self, error: std::string::String) -> ParserResult<T> {
        self.diagnostics.error(error, self.span());
        self.recover();
        Err(())
    }

    fn recover(&mut self) {
        while !self.at(EOF) { self.bump(); }
    }
}

pub fn parse(source: &Source) -> Module {
    let tokens = lexer::tokens(&source.content);

    let mut parser = Parser::new(source, tokens);
    let mut nodes = vec![];

    newline(&mut parser);

    while !parser.at(EOF) {
        if let Ok(node) = expr(&mut parser) {
            nodes.push(node);
        }
        let _ = endline(&mut parser);
    }

    Module::new(source.source_id, nodes, parser.diagnostics)
}

fn expr(p: &mut Parser) -> ParserResult<Node> {
    binary(p, 0 as Precedence)
}

fn atom(p: &mut Parser) -> ParserResult<Node> {
    match p.kind() {
        Number => number(p),
        Integer => integer(p),
        LParen => parens(p),
        _ => {
            p.fail(
                match p.kind() {
                    UnexpectedCharacter => "unexpected character",
                    UnterminatedString => "unterminated string",
                    _ => "unexpected expression",
                }.to_string()
            )
        }
    }
}

fn literal(p: &mut Parser, kind: TokenKind, ctor: fn(Literal) -> NodeKind) -> ParserResult<Node> {
    let span = token(p, kind)?;
    let value = p.slice(span).to_string();

    Ok(node::Node::literal(value, ctor, span))
}

fn number(p: &mut Parser) -> ParserResult<Node> {
    literal(p, Number, NodeKind::Number)
}

fn integer(p: &mut Parser) -> ParserResult<Node> {
    literal(p, Integer, NodeKind::Integer)
}

fn parens(p: &mut Parser) -> ParserResult<Node> {
    let start = p.span();

    token(p, LParen)?;
    let item = expr(p)?;
    token(p, RParen)?;

    let span = p.done(start);

    Ok(node::Node::group(item, span))
}

fn unary(p: &mut Parser) -> ParserResult<Node> {
    if let Some(operator) = operator(p, true) && operator.is_unary() {
        p.bump(); // operator
        let expr = unary(p)?;

        Ok(node::Node::unary(operator, expr))
    } else {
        atom(p)
    }
}

fn binary(p: &mut Parser, precedence: Precedence) -> ParserResult<Node> {
    let mut expr = unary(p)?;

    while let Some(operator) = operator(p, false) && operator.precedence() >= precedence
    {
        p.bump(); // operator

        let precedence = match operator.associativity() {
            Associativity::None
          | Associativity::Left => operator.precedence() + 1,
            Associativity::Right => operator.precedence(),
        };

        let rexpr = binary(p, precedence)?;
        let lexpr = expr;

        expr = node::Node::binary(operator, lexpr, rexpr);
    }

    Ok(expr)
}

fn operator(p: &mut Parser, unary: bool) -> Option<Operator> {
    let span = p.span();

    let kind = if unary {
        match p.kind() {
            Plus  => OperatorKind::Pos,
            Minus => OperatorKind::Neg,
            Not   => OperatorKind::Not,
            _ => return None
        }
    } else {
        match p.kind() {
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
       }
    };

    Some(Operator::new(kind, span))
}

fn newline(p: &mut Parser) {
    maybe(p, NewLine);
}

fn endline(p: &mut Parser) -> ParserResult<()> {
    if p.at(NewLine) || p.at(EOF) {
        p.bump();
        Ok(())
    } else {
        p.fail(unexpected(&[NewLine]))
    }
}

fn token(p: &mut Parser, kind: TokenKind) -> ParserResult<Span> {
    let span = p.span();
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

fn comma<F, T>(p: &mut Parser, until: TokenKind, mut parse: F) -> ParserResult<Vec<T>>
where
    F: FnMut(&mut Parser) -> ParserResult<T>,
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

fn many<F, T>(p: &mut Parser, until: TokenKind, mut parse: F) -> ParserResult<Vec<T>>
where
    F: FnMut(&mut Parser) -> ParserResult<T>,
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
