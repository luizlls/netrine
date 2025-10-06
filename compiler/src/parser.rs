use crate::error::{Error, Result};
use crate::lexer::Tokens;
use crate::source::Span;
use crate::syntax::{
    Binary, Def, Literal, Module, Name, Node, Operator, OperatorKind, Precedence, Unary,
};
use crate::token::{Token, TokenKind};

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
        }
        .init()
    }

    fn init(mut self) -> Parser<'src> {
        self.token = self.tokens.token();
        self
    }

    fn bump(&mut self) {
        self.tokens.bump();
        self.token = self.tokens.token();
    }

    fn fail<T>(&mut self, message: impl Into<String>) -> Result<T> {
        let span = self.token.span;
        self.recover();
        Err(Error::error(span, message.into()))
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
        if self.at(TokenKind::Identifier) && self.tokens.peek().is(TokenKind::Equals) {
            self.def()
        } else {
            self.expr()
        }
    }

    fn def(&mut self) -> Result<Node> {
        let name = self.name()?;
        self.expect(TokenKind::Equals)?;
        let value = self.expr()?;

        Ok(Node::Def(
            Def {
                span: Span::from(&name, &value),
                name,
                value,
            }
            .into(),
        ))
    }

    fn expr(&mut self) -> Result<Node> {
        self.binary(0 as Precedence)
    }

    fn atom(&mut self) -> Result<Node> {
        let token = self.token;
        match token.kind {
            TokenKind::Identifier => self.ident(),
            TokenKind::Number => self.number(),
            TokenKind::Integer => self.integer(),
            TokenKind::LParen => self.parens(),
            _ => {
                self.fail(match token.kind {
                    TokenKind::UnexpectedCharacter => "unexpected character",
                    TokenKind::UnterminatedString => "unterminated string",
                    _ => "unexpected expression",
                })
            }
        }
    }

    fn literal(&mut self, kind: TokenKind) -> Result<Literal> {
        let token = self.token;
        self.expect(kind)?;
        let span = token.span;
        let value = self.tokens.value(token).into();

        Ok(Literal { value, span })
    }

    fn name(&mut self) -> Result<Name> {
        self.literal(TokenKind::Identifier)
    }

    fn ident(&mut self) -> Result<Node> {
        self.literal(TokenKind::Identifier).map(Node::Name)
    }

    fn number(&mut self) -> Result<Node> {
        self.literal(TokenKind::Number).map(Node::Number)
    }

    fn integer(&mut self) -> Result<Node> {
        self.literal(TokenKind::Integer).map(Node::Integer)
    }

    fn parens(&mut self) -> Result<Node> {
        self.expect(TokenKind::LParen)?;
        let expr = self.expr()?;
        self.expect(TokenKind::RParen)?;

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Node> {
        let Some(operator) = self.operator(0 as Precedence, true) else {
            return self.atom();
        };

        let expr = self.unary()?;

        Ok(Node::Unary(
            Unary {
                span: Span::from(&operator, &expr),
                expr,
                operator,
            }
            .into(),
        ))
    }

    fn binary(&mut self, precedence: Precedence) -> Result<Node> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.operator(precedence, false) {
            let lexpr = expr;
            let rexpr = self.binary(operator.next_precedence())?;

            expr = Node::Binary(
                Binary {
                    span: Span::from(&lexpr, &rexpr),
                    lexpr,
                    rexpr,
                    operator,
                }
                .into(),
            );
        }

        Ok(expr)
    }

    fn operator(&mut self, precedence: Precedence, unary: bool) -> Option<Operator> {
        let token = self.token;

        let kind = match token.kind {
            TokenKind::Plus if unary => OperatorKind::Pos,
            TokenKind::Minus if unary => OperatorKind::Neg,
            TokenKind::Not if unary => OperatorKind::Not,
            TokenKind::Plus => OperatorKind::Add,
            TokenKind::Minus => OperatorKind::Sub,
            TokenKind::Star => OperatorKind::Mul,
            TokenKind::Slash => OperatorKind::Div,
            TokenKind::Caret => OperatorKind::Pow,
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
        if self.maybe(TokenKind::EOL) || self.maybe(TokenKind::Semi) || self.maybe(TokenKind::EOF) {
            Ok(())
        } else {
            self.fail(self.unexpected(&[TokenKind::EOL]))
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

    fn unexpected(&self, expected: &[TokenKind]) -> String {
        let expected = expected
            .iter()
            .map(|it| format!("`{it}`"))
            .collect::<Vec<_>>()
            .join(", ");
        let actual = self.token.kind;
        format!("expected {expected}, found `{actual}`")
    }
}

pub fn parse(tokens: Tokens<'_>) -> Result<Module> {
    let mut parser = Parser::new(tokens);
    let mut nodes = vec![];

    parser.newline();

    while !parser.done() {
        let node = parser.top_level()?;
        nodes.push(node);
        parser.endline()?;
    }

    Ok(Module { nodes })
}
