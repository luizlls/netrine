use super::node::{
    Associativity, Binary, Literal, Node, Operator, OperatorKind, Precedence, Unary,
};
use crate::error::{Error, Result};
use super::lexer::Tokens;
use crate::source::Span;
use super::token::{Token, TokenKind};

#[derive(Debug)]
struct Parser<'src> {
    tokens: Tokens<'src>,
    token: Token,
}

impl<'src> Parser<'src> {
    fn new(tokens: Tokens<'src>) -> Parser<'src> {
        Parser {
            tokens,
            token: Token::default(),
        }.init()
    }

    fn init(mut self) -> Parser<'src> {
        self.token = self.tokens.token();
        self
    }

    fn bump(&mut self) {
        self.tokens.bump();
        self.token = self.tokens.token();
    }

    fn fail(&mut self, error: String) -> Error {
        let span = self.token.span;
        self.recover();
        Error::new(error, span)
    }

    fn recover(&mut self) {
        while !self.tokens.done() {
            self.bump();
        }
    }

    pub fn at(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
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
        let token = self.token;
        match token.kind {
            TokenKind::Number => self.number(),
            TokenKind::Integer => self.integer(),
            TokenKind::LParen => self.parens(),
            _ => Err(self.fail(
                match token.kind {
                    TokenKind::UnexpectedCharacter => "unexpected character",
                    TokenKind::UnterminatedString => "unterminated string",
                    _ => "unexpected expression",
                }
                .to_string(),
            )),
        }
    }

    fn literal(&mut self, kind: TokenKind, ctor: fn(Literal) -> Node) -> Result<Node> {
        let token = self.token;
        self.expect(kind)?;
        let span  = token.span;
        let value = self.tokens.value(token).to_string();

        Ok(ctor(Literal { value, span }))
    }

    fn number(&mut self) -> Result<Node> {
        self.literal(TokenKind::Number, Node::Number)
    }

    fn integer(&mut self) -> Result<Node> {
        self.literal(TokenKind::Integer, Node::Integer)
    }

    fn parens(&mut self) -> Result<Node> {
        self.expect(TokenKind::LParen)?;
        let expr = self.expr()?;
        self.expect(TokenKind::RParen)?;

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Node> {
        if let Some(operator) = self.operator(0 as Precedence, true) {
            let expr = self.atom()?;

            Ok(Node::Unary(Unary {
                span: Span::from(&operator, &expr),
                expr,
                operator,
            }.into()))
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

            expr = Node::Binary(Binary {
                span: Span::from(&lexpr, &rexpr),
                lexpr,
                rexpr,
                operator,
            }.into());
        }

        Ok(expr)
    }

    fn operator(&mut self, precedence: Precedence, unary: bool) -> Option<Operator> {
        let token = self.token;

        let kind = match token.kind {
            TokenKind::Plus if unary => OperatorKind::Add,
            TokenKind::Minus if unary => OperatorKind::Sub,
            TokenKind::Not if unary => OperatorKind::Not,
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
        };

        let operator = Operator {
            kind,
            span: token.span,
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
