use crate::ast::{
    Associativity, Binary, Group, Literal, Node, NodeKind, Operator, OperatorKind,
    Precedence, Unary,
};
use crate::error::{Error, Result};
use crate::lexer::Lexer;
use crate::source::{Source, Span};
use crate::token::{Token, TokenKind, TokenKind::*};

#[derive(Debug)]
pub struct Parser<'p> {
    source: &'p Source,
    lexer: Lexer<'p>,
    token: Token,
    prev: Token,
    peek: Token,
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p Source) -> Parser<'p> {
        let lexer = Lexer::new(&source.content);
        Parser {
            source,
            lexer,
            token: Token::default(),
            prev:  Token::default(),
            peek:  Token::default(),
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

    fn kind(&self) -> TokenKind {
        self.token.kind
    }

    fn span(&self) -> Span {
        self.token.span
    }

    fn done(&self, start: Span) -> Span {
        Span::of(start, self.prev.span)
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
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
        while !self.at(EOF) { self.bump(); }
    }
}

impl Iterator for Parser<'_> {
    type Item = Result<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.at(EOF) {
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
        Number => number(p),
        Integer => integer(p),
        LParen => parens(p),
        _ => p.fail(
            match p.kind() {
                UnexpectedCharacter => "unexpected character",
                UnterminatedString => "unterminated string",
                _ => "unexpected expression",
            }
            .to_string(),
        ),
    }
}

fn literal(p: &mut Parser, kind: TokenKind, ctor: fn(Literal) -> NodeKind) -> Result<Node> {
    let span = token(p, kind)?;

    Ok(Node {
        kind: ctor(Literal {
            value: p.slice(span).to_string(),
        }),
        span,
    })
}

fn number(p: &mut Parser) -> Result<Node> {
    literal(p, Number, NodeKind::Number)
}

fn integer(p: &mut Parser) -> Result<Node> {
    literal(p, Integer, NodeKind::Integer)
}

fn parens(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, LParen)?;
    let item = expr(p)?;
    token(p, RParen)?;

    let span = p.done(start);

    Ok(node(
        NodeKind::Group(Group {
            inner: Box::new(item),
        }),
        span,
    ))
}

fn unary(p: &mut Parser) -> Result<Node> {
    if let Some(operator) = operator(p, true) && operator.is_unary() {
        p.bump(); // operator
        let expr = unary(p)?;

        let span = Span::of(operator.span, expr.span);

        Ok(node(
            NodeKind::Unary(Unary {
                operator,
                expr: Box::new(expr),
            }),
            span,
        ))
    } else {
        atom(p)
    }
}

fn binary(p: &mut Parser, precedence: Precedence) -> Result<Node> {
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

        let span = Span::of(lexpr.span, rexpr.span);

        expr = node(
            NodeKind::Binary(Binary {
                operator,
                lexpr: Box::new(lexpr),
                rexpr: Box::new(rexpr),
            }),
            span,
        );
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

fn endline(p: &mut Parser) -> Result<()> {
    if p.at(NewLine) || p.at(EOF) {
        p.bump();
        Ok(())
    } else {
        p.fail(unexpected(&[NewLine]))
    }
}

fn node(kind: NodeKind, span: Span) -> Node {
    Node { kind, span }
}

fn token(p: &mut Parser, kind: TokenKind) -> Result<Span> {
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

fn unexpected(expected: &[TokenKind]) -> std::string::String {
    let expected = expected.iter().map(|it| format!("`{it}`")).collect::<Vec<_>>().join(", ");
    format!("expected {expected}")
}
