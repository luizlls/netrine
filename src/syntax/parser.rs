use crate::error::{Error, Result};
use crate::source::{Source, Span};
use super::node::{
    Node, Unary, Binary, Group, Operator, OperatorKind, Precedence, Associativity, Literal,
};
use super::token::{Token, TokenKind};

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

    fn expr(&mut self) -> Result<Node> {
        self.binary(0 as Precedence)
    }

    fn atom(&mut self) -> Result<Node> {
        match self.token.kind {
            TokenKind::Number => self.number(),
            TokenKind::Integer => self.integer(),
            TokenKind::LParen => self.parens(),
            _ => self.fail(
                match self.token.kind {
                    TokenKind::UnexpectedCharacter => "unexpected character",
                    TokenKind::UnterminatedString => "unterminated string",
                    _ => "unexpected expression",
                }
                .to_string(),
            ),
        }
    }

    fn literal(&mut self, kind: TokenKind, ctor: fn(Literal) -> Node) -> Result<Node> {
        let span = self.token.span;
        self.expect(kind)?;
        let value = self.slice(span).to_string();

        Ok(ctor(Literal { value, span }.into()))
    }

    fn number(&mut self) -> Result<Node> {
        self.literal(TokenKind::Number, Node::Number)
    }

    fn integer(&mut self) -> Result<Node> {
        self.literal(TokenKind::Integer, Node::Integer)
    }

    fn parens(&mut self) -> Result<Node> {
        let start = self.token.span;

        self.expect(TokenKind::LParen)?;
        let inner = self.expr()?;
        self.expect(TokenKind::RParen)?;

        let span = self.span(start);

        Ok(Node::Group(Group { inner, span }.into()))
    }

    fn unary(&mut self) -> Result<Node> {
        if let Some(operator) = self.operator(-1 as Precedence, true) {
            let expr = self.atom()?;
            let span = Span::of(&operator, &expr);

            Ok(Node::Unary(
                Unary {
                    operator,
                    expr,
                    span,
                }.into()
            ))
        } else {
            self.atom()
        }
    }

    fn binary(&mut self, precedence: Precedence) -> Result<Node> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.operator(precedence, false) {
            let precedence = match operator.kind.associativity() {
                Associativity::None
              | Associativity::Left => operator.kind.precedence() + 1,
                Associativity::Right => operator.kind.precedence(),
            };

            let rexpr = self.binary(precedence)?;
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

    fn operator(&mut self, precedence: Precedence, unary: bool) -> Option<Operator> {
        let kind = if unary {
            match self.token.kind {
                TokenKind::Plus  => OperatorKind::Pos,
                TokenKind::Minus => OperatorKind::Neg,
                TokenKind::Not   => OperatorKind::Not,
                _ => return None
            }
        } else {
            match self.token.kind {
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
                _ => return None,
            }
        };

        if kind.precedence() >= precedence {
            let span = self.token.span;
            self.bump();
            Some(Operator { kind, span })
        } else {
            None
        }
    }

    fn newline(&mut self) {
        self.maybe(TokenKind::NewLine);
    }

    fn endline(&mut self) -> Result<()> {
        if self.at(TokenKind::NewLine) || self.at(TokenKind::EOF) {
            self.bump();
            Ok(())
        } else {
            self.fail(self.unexpected(&[TokenKind::NewLine]))
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        if self.at(kind) {
            self.bump();
            Ok(())
        } else {
            self.fail(self.unexpected(&[kind]))
        }
    }

    fn maybe(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn comma<F, T>(&mut self, until: TokenKind, mut parse: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Parser) -> Result<T>,
    {
        let mut res = vec![];

        self.newline();

        while !self.at(until) {
            res.push(parse(self)?);

            self.newline();
            if !self.maybe(TokenKind::Comma) {
                break;
            }
            self.newline();
        }

        Ok(res)
    }

    fn many<F, T>(&mut self, until: TokenKind, mut parse: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Parser) -> Result<T>,
    {
        let mut res = vec![];

        self.newline();

        while !self.at(until) {
            res.push(parse(self)?);
            self.endline()?;
        }

        Ok(res)
    }

    fn unexpected(&self, expected: &[TokenKind]) -> std::string::String {
        let expected = expected.iter().map(|it| format!("`{it}`")).collect::<Vec<_>>().join(", ");
        format!("expected {expected}")
    }

}

pub fn parse(source: &Source, tokens: &[Token]) -> Result<Vec<Node>> {
    let mut parser = Parser::new(source, tokens);
    let mut nodes = vec![];

    while !parser.at(TokenKind::EOF) {
        nodes.push(parser.expr()?);
    }

    Ok(nodes)
}
