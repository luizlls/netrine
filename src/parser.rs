use crate::syntax::{
    Node, NodeKind, Operator, OperatorKind, Precedence, Associativity, Literal,
};
use crate::error::{Error, Result};
use crate::lexer::Lexer;
use crate::source::{Source, Span};
use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub struct Parser<'p> {
    source: &'p Source,
    lexer: Lexer<'p>,
    token: Token,
    prev:  Token,
    peek:  Token,
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

    fn kind(&self) -> TokenKind {
        self.token.kind
    }

    fn span(&self) -> Span {
        self.token.span
    }

    fn done(&self, start: Span) -> Span {
        Span::of(&start, &self.prev)
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
        while !self.at(TokenKind::EOF) { self.bump(); }
    }
}

impl Iterator for Parser<'_> {
    type Item = Result<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.at(TokenKind::EOF) {
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
        TokenKind::Number => number(p),
        TokenKind::Integer => integer(p),
        TokenKind::LParen => parens(p),
        _ => p.fail(
            match p.kind() {
                TokenKind::UnexpectedCharacter => "unexpected character",
                TokenKind::UnterminatedString => "unterminated string",
                _ => "unexpected expression",
            }
            .to_string(),
        ),
    }
}

fn literal(p: &mut Parser, kind: TokenKind, ctor: fn(Literal) -> NodeKind) -> Result<Node> {
    let span = token(p, kind)?;

    Ok(node(ctor(Literal {
        value: p.slice(span).to_string(),
        span,
    }), span))
}

fn number(p: &mut Parser) -> Result<Node> {
    literal(p, TokenKind::Number, NodeKind::Number)
}

fn integer(p: &mut Parser) -> Result<Node> {
    literal(p, TokenKind::Integer, NodeKind::Integer)
}

fn parens(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, TokenKind::LParen)?;
    let inner = expr(p)?;
    token(p, TokenKind::RParen)?;

    let span = p.done(start);

    Ok(node(
        NodeKind::Group(Box::new(inner)),
        span,
    ))
}

fn unary(p: &mut Parser) -> Result<Node> {
    if let Some(operator) = operator(p, true) && operator.is_unary() {
        p.bump(); // operator
        let expr = unary(p)?;
        let span = Span::of(&operator, &expr);

        Ok(node(
            NodeKind::Unary(operator, Box::new(expr)),
            span,
        ))
    } else {
        atom(p)
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
        let span = Span::of(&lexpr, &rexpr);

        expr = node(
            NodeKind::Binary(operator, Box::new(lexpr), Box::new(rexpr)),
            span,
        );
    }

    Ok(expr)
}

fn operator(p: &mut Parser, unary: bool) -> Option<Operator> {
    let kind = if unary {
        match p.kind() {
            TokenKind::Plus  => OperatorKind::Pos,
            TokenKind::Minus => OperatorKind::Neg,
            TokenKind::Not   => OperatorKind::Not,
            _ => return None
        }
    } else {
        match p.kind() {
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

    let span = p.span();

    Some(Operator { kind, span })
}

fn node(kind: NodeKind, span: Span) -> Node {
    Node {
        kind,
        span,
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
