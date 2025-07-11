use super::node::{
    Associativity, Binary, Group, Literal, Node, Operator, OperatorKind, Precedence, Unary,
};
use crate::error::{Error, Result};
use super::lexer::Tokens;
use crate::source::Span;
use super::token::TokenKind;

#[derive(Debug)]
struct Parser<'src> {
    tokens: Tokens<'src>,
}

impl<'src> Parser<'src> {
    fn new(tokens: Tokens<'src>) -> Parser<'src> {
        Parser {
            tokens,
        }
    }

    fn bump(&mut self) {
        self.tokens.bump();
    }

    fn span(&self, start: Span) -> Span {
        let prev = self.tokens.prev();
        Span::of(&start, &prev)
    }

    fn fail(&mut self, error: std::string::String) -> Error {
        let span = self.tokens.token().span;
        self.recover();
        Error::new(error, span)
    }

    fn recover(&mut self) {
        while !self.tokens.done() {
            self.bump();
        }
    }

    pub fn at(&self, kind: TokenKind) -> bool {
        self.tokens.token().kind == kind
    }

    fn done(&self) -> bool {
        self.tokens.done()
    }

    fn top_level(&mut self) -> Result<Node> {
        self.expr()
    }

    fn expr(&mut self) -> Result<Node> {
        self.binary(0 as Precedence)
    }

    fn atom(&mut self) -> Result<Node> {
        match self.tokens.token().kind {
            TokenKind::Number => self.number(),
            TokenKind::Integer => self.integer(),
            TokenKind::LParen => self.parens(),
            _ => Err(self.fail(
                match self.tokens.token().kind {
                    TokenKind::UnexpectedCharacter => "unexpected character",
                    TokenKind::UnterminatedString => "unterminated string",
                    _ => "unexpected expression",
                }
                .to_string(),
            )),
        }
    }

    fn literal(&mut self, kind: TokenKind, ctor: fn(Box<Literal>) -> Node) -> Result<Node> {
        let token = self.tokens.token();
        self.expect(kind)?;
        let span  = token.span;
        let value = self.tokens.value(span).to_string();

        Ok(ctor(Literal { value, span }.into()))
    }

    fn number(&mut self) -> Result<Node> {
        self.literal(TokenKind::Number, Node::Number)
    }

    fn integer(&mut self) -> Result<Node> {
        self.literal(TokenKind::Integer, Node::Integer)
    }

    fn parens(&mut self) -> Result<Node> {
        let start = self.tokens.token().span;

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

            Ok(Node::Unary(Unary { operator, expr, span }.into()))
        } else {
            self.atom()
        }
    }

    fn binary(&mut self, precedence: Precedence) -> Result<Node> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.operator(precedence, false) {
            let precedence = match operator.associativity() {
                Associativity::None
              | Associativity::Left => operator.precedence() + 1,
                Associativity::Right => operator.precedence(),
            };

            let rexpr = self.binary(precedence)?;
            let lexpr = expr;
            let span = Span::of(&lexpr, &rexpr);

            expr = Node::Binary(Binary { operator, lexpr, rexpr, span }.into());
        }

        Ok(expr)
    }

    fn operator(&mut self, precedence: Precedence, unary: bool) -> Option<Operator> {
        let kind = if unary {
            match self.tokens.token().kind {
                TokenKind::Plus => OperatorKind::Pos,
                TokenKind::Minus => OperatorKind::Neg,
                TokenKind::Not => OperatorKind::Not,
                _ => return None,
            }
        } else {
            match self.tokens.token().kind {
                TokenKind::Plus => OperatorKind::Add,
                TokenKind::Minus => OperatorKind::Sub,
                TokenKind::Star => OperatorKind::Mul,
                TokenKind::Slash => OperatorKind::Div,
                TokenKind::Caret => OperatorKind::Exp,
                TokenKind::Mod => OperatorKind::Mod,
                TokenKind::And => OperatorKind::And,
                TokenKind::Or => OperatorKind::Or,
                TokenKind::EqEq => OperatorKind::Eq,
                TokenKind::NoEq => OperatorKind::Ne,
                TokenKind::Lt => OperatorKind::Lt,
                TokenKind::LtEq => OperatorKind::Le,
                TokenKind::Gt => OperatorKind::Gt,
                TokenKind::GtEq => OperatorKind::Ge,
                _ => return None,
            }
        };

        let operator = Operator {
            kind,
            span: self.tokens.token().span,
        };

        if operator.precedence() >= precedence {
            self.bump();
            Some(operator)
        } else {
            None
        }
    }

    fn newline(&mut self) {
        self.maybe(TokenKind::EOL);
    }

    fn endline(&mut self) -> Result<()> {
        if self.maybe(TokenKind::EOL)
        || self.maybe(TokenKind::EOF) {
            Ok(())
        } else {
            Err(self.fail(self.unexpected(&[TokenKind::EOL])))
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        if self.at(kind) {
            self.bump();
            Ok(())
        } else {
            Err(self.fail(self.unexpected(&[kind])))
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

    fn seq<F, T>(&mut self, until: TokenKind, mut parse: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        self.newline();

        while !self.at(until) {
            result.push(parse(self)?);

            self.newline();
            if !self.maybe(TokenKind::Comma) {
                break;
            }
            self.newline();
        }

        Ok(result)
    }

    fn many<F, T>(&mut self, until: TokenKind, mut parse: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        self.newline();

        while !self.at(until) {
            result.push(parse(self)?);
            self.endline()?;
        }

        Ok(result)
    }

    fn unexpected(&self, expected: &[TokenKind]) -> std::string::String {
        let expected = expected.iter().map(|it| format!("`{it}`")).collect::<Vec<_>>().join(", ");
        format!("expected {expected}")
    }
}

pub fn parse(tokens: Tokens<'_>) -> Result<Vec<Node>> {
    let mut parser = Parser::new(tokens);
    let mut nodes = vec![];

    parser.newline();

    while !parser.done() {
        let node = parser.top_level()?;
        nodes.push(node);
        parser.endline()?;
    }

    Ok(nodes)
}
